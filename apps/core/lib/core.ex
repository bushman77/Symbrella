defmodule Core do
  @moduledoc """
  Pipeline:
  Tokenize → Rebuild word n-grams → STM → LTM (DB read/enrich)
  → MWE Signatures (late bias) → (Episodes attach) → LIFG Stage-1 → ATL → Hippocampus encode
  → (Optional) Persist episode → Notify Brain.

  Notes:
  • resolve_input/2 defaults to :prod mode and runs the full pipeline.
  • LTM no longer seeds or refetches via Lexicon — DB is the source of truth.
  • Any legacy `type: "seed"` rows are ignored when selecting candidates.
  • LIFG tie-breaks: uses a tiny POS prior only for razor-thin margins.
  • MWE shadowing: winning multi-word tokens suppress overlapping unigram winners.
  • MWE Signatures: cheap, symbolic sliding-window MWE detection producing `si.intent_bias`.
    - We run it twice: optional early (pre-LTM) and required late (post-LTM, pre-LIFG).
    - Toggled by env `:core, :mwe_signatures` (":on" by default; set to `:off` to disable),
      or per-call via opts `mwe_signatures: :off`.
  • All Brain interactions are guarded so tests without the Brain process still pass.
  • Episodes attach can be toggled via `opts[:episodes]` (default true) or env
    `Application.get_env(:brain, :episodes_mode, :on) != :off`.
  • (Optional) Episode persistence is controlled by:
      - opts:   persist_episodes: true
      - env:    EPISODES_PERSIST=on|true|1
      - config: Application.put_env(:brain, :episodes_persist, :on)
  """

  # ── P-253/254 knobs ─────────────────────────────────────────────────────────
  @lifg_tie_epsilon 0.01      # score margin considered "razor-thin"
  @lifg_prob_epsilon 0.01     # probability margin considered "razor-thin"
  @greet_phrase_bump 0.01     # extra nudge for MWE when intent==:greet
  @mwe_general_bump 0.008     # NEW: small nudge for any multi-word token in thin ties

  alias Brain
  alias Core.SemanticInput
  alias Core.Intent.Selection
  alias Core.MWE.Signatures
  alias Db
  alias Db.BrainCell

  @type si :: map()
  @type opts :: keyword()

  @spec resolve_input(String.t(), opts()) :: SemanticInput.t()
def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
  mode  = Keyword.get(opts, :mode, :prod)
  max_n = Keyword.get(opts, :max_wordgram_n, 3)

  # optionally pull a default from config so it's “always on” unless overridden
  lifg_defaults = Application.get_env(:brain, :lifg_defaults, [])
  lifg_opts = Keyword.merge(lifg_defaults, Keyword.get(opts, :lifg_opts, []))

  si0 =
    phrase
    |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n)
    |> wrap_si(phrase)
    |> rebuild_word_ngrams(max_n)
    |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
    |> Map.put_new(:trace, [])
    |> Map.put(:lifg_opts, lifg_opts)   # ← ensure it’s on the SI

  si1 = Selection.select(si0, opts)

case mode do
    :prod ->
      si1
      |> Brain.stm()
      |> keep_only_word_boundary_tokens()
      |> maybe_run_mwe(:early, opts)
      |> ltm_stage(opts)
      |> keep_only_word_boundary_tokens()
      |> maybe_run_mwe(:late, opts)
      |> Core.Relations.attach_edges()
      |> maybe_attach_episodes(opts)
      |> run_lifg_and_attach(opts)
      |> maybe_ingest_atl(opts)
      |> then(&Brain.ATL.attach_lifg_pairs(&1, opts))   # ← add this line
      |> maybe_encode_hippocampus()
      |> maybe_persist_episode(opts)
      |> notify_brain_activation(opts)

  _ ->
    si1
end


end



  # ─────────────────────── Brain notify ───────────────────────

  defp id_norm(nil), do: nil
  defp id_norm(id) when is_binary(id), do: id |> String.split("|") |> hd()

  # Extract POS from an id (e.g., "hello|noun|0" -> "noun")
  defp id_pos(nil), do: nil
  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, p | _] -> p
      _ -> nil
    end
  end

  defp notify_brain_activation(si, opts) do
    payload = %{
      delta: Keyword.get(opts, :delta, 0.1),
      decay: Keyword.get(opts, :decay, 0.98),
      via: :core
    }

    rows = Map.get(si, :active_cells, []) || []
    lifg_count = si |> Map.get(:lifg_choices, []) |> length()

    if rows != [] and Process.whereis(Brain) do
      GenServer.cast(Brain, {:activate_cells, rows, payload})
    end

    Map.update(si, :trace, [], fn tr ->
      [{:activated, %{rows: length(rows), lifg_choices: lifg_count, shape: :activate_cells}} | tr]
    end)
  end

  # ─────────────────────── LIFG stage-1 ───────────────────────

  defp run_lifg_and_attach(si, lifg_opts) do
    groups =
      si.tokens
      |> Enum.reduce(%{}, fn t, acc ->
        nrm = norm(Map.get(t, :phrase))
        tn  = Map.get(t, :n, 1)

        senses =
          si.active_cells
          |> Enum.filter(fn s -> (Map.get(s, :norm) || Map.get(s, "norm")) == nrm end)
          |> Enum.filter(&compatible_cell_for_token?(tn, &1))
          |> drop_seed_rows()

        if senses == [], do: acc, else: Map.put(acc, Map.get(t, :index), senses)
      end)

    # P-213: visibility telemetry on candidate volume pre-call
    cand_count =
      groups
      |> Map.values()
      |> Enum.map(&length/1)
      |> Enum.sum()

    emit([:brain, :lifg, :candidates_ready], %{candidates: cand_count}, %{stage: :pre_call})

    ctx = [1.0]

    lifg_input = %{
      candidates_by_token: groups,
      tokens: si.tokens,
      sentence: si.sentence,
      atl_slate: Map.get(si, :atl_slate),
      evidence: Map.get(si, :evidence)           # episodes available to Stage-1
    }

    # Pass intent bias downstream (non-breaking; ignored if unused)
    lifg_opts2 = Keyword.put(lifg_opts, :intent_bias, Map.get(si, :intent_bias, %{}))

    case Brain.lifg_stage1(lifg_input, ctx, lifg_opts2) do
      {:ok, out} ->
        # P-213: telemetry when LIFG returns no choices (helps gate tuning)
        if (Map.get(out, :choices, []) || []) == [] do
          emit([:brain, :lifg, :no_choices], %{candidates: cand_count}, %{stage: :post_call})
        end

        ev =
          out.audit
          |> Map.merge(%{
            stage: :lifg_stage1,
            choices: out.choices,
            boosts: out.boosts,
            inhibitions: out.inhibitions
          })

        lifg_choices =
          Enum.map(out.choices, fn ch ->
            token_index = Map.get(ch, :token_index, 0)
            t = Enum.at(si.tokens, token_index, %{})

            # (1) Lemma from the token's phrase (normalized)
            token_norm =
              t
              |> Map.get(:phrase, "")
              |> norm()

            chosen_id  = Map.get(ch, :chosen_id)
            scores     = Map.get(ch, :scores) || %{}
            feats      = Map.get(ch, :features) || %{}
            alt_ids    = Map.get(ch, :alt_ids, [])
            margin     = Map.get(ch, :margin, 0.0)
            prob_margin = Map.get(ch, :prob_margin, 0.0)

            # Greeting MWE tiny bump + NEW general MWE bump for thin ties
            phrase_bump_for = fn id, thin? ->
              cond do
                Map.get(t, :n, 1) > 1 and id_norm(id) == token_norm ->
                  base = if thin?, do: @mwe_general_bump, else: 0.0
                  greet = if Map.get(si, :intent) == :greet, do: @greet_phrase_bump, else: 0.0
                  base + greet
                true ->
                  0.0
              end
            end

            base_score =
              if is_binary(chosen_id) and is_map(scores) do
                Map.get(scores, chosen_id, 0.0)
              else
                Map.get(feats, :score_norm, 0.0)
              end

            candidates =
              [chosen_id | alt_ids]
              |> Enum.uniq()
              |> Enum.reject(&is_nil/1)

            # Favor ids whose id-norm matches the token norm
            matching =
              candidates
              |> Enum.filter(&(id_norm(&1) == token_norm))

            # POS prior for razor-thin ties (phrase > noun > others)
            pos_prior = %{"phrase" => 0.03, "noun" => 0.01, "verb" => 0.0, "adjective" => 0.0}

            thin? = (margin < @lifg_tie_epsilon) or (prob_margin < @lifg_prob_epsilon)

            score_of = fn id ->
              Map.get(scores, id, base_score) + phrase_bump_for.(id, thin?)
            end

            pick_with_prior = fn ids ->
              Enum.max_by(
                ids,
                fn id ->
                  score_of.(id) + Map.get(pos_prior, id_pos(id), 0.0)
                end,
                fn -> chosen_id end
              )
            end

            {chosen_id2, score2} =
              cond do
                matching != [] and thin? ->
                  best = pick_with_prior.(matching)
                  {best, Map.get(scores, best, base_score)}

                matching != [] ->
                  best = Enum.max_by(matching, &score_of.(&1), fn -> chosen_id end)
                  {best, Map.get(scores, best, base_score)}

                thin? ->
                  best = pick_with_prior.(candidates)
                  {best, Map.get(scores, best, base_score)}

                true ->
                  {chosen_id, base_score}
              end

            %{
              token_index: token_index,
              lemma: token_norm,
              id: chosen_id2,
              alt_ids: alt_ids,
              score: score2
            }
          end)
          |> mwe_shadow(si.tokens)   # shadow unigrams covered by a winning MWE

        si
        |> Map.put(:lifg_choices, lifg_choices)
        |> Map.put(:acc_conflict, get_in(out, [:si, :acc_conflict]) || 0.0)
        |> Map.update(:trace, [], &[ev | &1])

      {:error, _} ->
        si
    end
  end

  defp maybe_ingest_atl(%{lifg_choices: choices, tokens: tokens} = si, _opts)
       when is_list(choices) and is_list(tokens) do
    if choices == [] do
      si
    else
      slate =
        case Process.whereis(Brain.ATL) do
          pid when is_pid(pid) -> Brain.ATL.ingest(choices, tokens)
          _ -> Brain.ATL.reduce(choices, tokens)
        end

      si
      |> Map.put(:atl_slate, slate)
      |> Map.update(:trace, [], fn tr ->
        [
          %{
            stage: :atl,
            ts_ms: System.system_time(:millisecond),
            winners: Map.get(slate, :winner_count, 0),
            concepts: slate |> Map.get(:by_norm, %{}) |> map_size()
          }
          | tr
        ]
      end)
    end
  end

  defp maybe_ingest_atl(si, _opts), do: si

  # Only allow unigrams to consider unigram senses, and MWEs to consider MWEs.
  defp compatible_cell_for_token?(token_n, cell) do
    nrm = Map.get(cell, :norm) || Map.get(cell, "norm") || ""
    has_space = String.contains?(nrm, " ")
    (token_n > 1 and has_space) or (token_n == 1 and not has_space)
  end

  # Always drop legacy seed rows if any are present.
  defp drop_seed_rows(senses),
    do: Enum.reject(senses, &seed?/1)

  defp seed?(s), do: (Map.get(s, :type) || Map.get(s, "type")) == "seed"

  defp maybe_encode_hippocampus(%{atl_slate: slate} = si) when is_map(slate) do
    if Process.whereis(Brain.Hippocampus) do
      ep = Brain.Hippocampus.encode(slate)
      Map.put(si, :episode, Map.take(ep, [:ts_ms, :token_count, :winner_count]))
    else
      si
    end
  end

  defp maybe_encode_hippocampus(si), do: si

  # ─────────────────────── NEW: Episodes attach ───────────────────────

  defp maybe_attach_episodes(si, opts) when is_map(si) do
    enabled?  = episodes_enabled?(opts)
    hippo_up? = is_pid(Process.whereis(Brain.Hippocampus))

    cond do
      not enabled? or not hippo_up? ->
        si

      true ->
        pass =
          []
          |> put_if_present(:source,     Keyword.get(opts, :recall_source))
          |> put_if_present(:embedding,  Keyword.get(opts, :episode_embedding))

        # attach_episodes builds cues itself from SI (winners/atl_slate/tokens/sentence)
        si2 = Brain.Hippocampus.attach_episodes(si, pass)
        eps = get_in(si2, [:evidence, :episodes]) || []
        emit([:brain, :core, :episodes_attached], %{count: length(eps)}, %{mode: :pre_lifg})
        si2
    end
  catch
    _, _ -> si   # safety: never break pipeline
  end

  defp episodes_enabled?(opts) do
    opt = Keyword.get(opts, :episodes, nil)

    case opt do
      true -> true
      false -> false
      _ ->
        # default to on unless env explicitly sets :off
        Application.get_env(:brain, :episodes_mode, :on) != :off
    end
  end

  # ─────────────────────── NEW: Optional persistence ───────────────────────

  defp maybe_persist_episode(si, opts) when is_map(si) do
    # Delegates gating to Writer (persist flag/env). Always returns SI unchanged.
    Brain.Hippocampus.Writer.maybe_persist(
      si,
      persist:   Keyword.get(opts, :persist_episodes),
      embedding: Keyword.get(opts, :episode_embedding),
      user_id:   Keyword.get(opts, :user_id)
    )
  rescue
    _ -> si
  end

  # ─────────────────────── LTM orchestrator ───────────────────────

  # Consumes Db.ltm/2. Merges rows into :active_cells and nudges Brain.
  defp ltm_stage(si, opts) when is_map(si) do
    case Db.ltm(si, opts) do
      {:ok, %{rows: rows, db_hits: db_hits}} ->
        existing =
          case Map.get(si, :active_cells, []) do
            l when is_list(l) -> l
            _ -> []
          end

        active_cells =
          existing
          |> Kernel.++(rows)
          |> Enum.flat_map(&sanitize_cell/1)
          |> Enum.reject(&(cell_id(&1) == nil))
          |> Enum.uniq_by(&cell_id/1)

        activation_summary =
          si
          |> Map.get(:activation_summary, %{})
          |> Map.update(:db_hits, db_hits, fn acc -> MapSet.union(acc, db_hits) end)

        si1 =
          si
          |> Map.put(:active_cells, active_cells)
          |> Map.put(:activation_summary, activation_summary)

        if rows != [] and Process.whereis(Brain) do
          GenServer.cast(Brain, {:activate_cells, rows, %{delta: 1.0, decay: 0.98, via: :ltm}})
        end

        si1

      _ ->
        si
    end
  end

  # ─────────────────────── Tokens / n-grams ───────────────────────

  defp rebuild_word_ngrams(%{sentence: s} = si, max_n)
       when is_binary(s) and is_integer(max_n) and max_n > 0 do
    s_norm = s |> String.trim() |> String.replace(~r/\s+/u, " ")
    words  = if s_norm == "", do: [], else: String.split(s_norm, " ")

    # char start offsets for each word
    starts =
      words
      |> Enum.reduce({[], 0}, fn w, {acc, pos} -> {[pos | acc], pos + String.length(w) + 1} end)
      |> elem(0)
      |> Enum.reverse()

    wlen = length(words)

    tokens =
      for i <- 0..(wlen - 1), n <- min(max_n, wlen - i)..1//-1 do
        phrase = words |> Enum.slice(i, n) |> Enum.join(" ")
        start  = Enum.at(starts, i)
        len    = String.length(phrase)

        %{index: nil, span: {start, len}, n: n, phrase: phrase, mw: n > 1, instances: []}
      end
      |> Enum.with_index()
      |> Enum.map(fn {t, idx} -> Map.put(t, :index, idx) end)

    si
    |> Map.put(:sentence, s_norm)
    |> Map.put(:tokens, tokens)
  end

  defp rebuild_word_ngrams(si, _), do: si

  defp keep_only_word_boundary_tokens(%{sentence: s, tokens: toks} = si)
       when is_binary(s) and is_list(toks) do
    s_norm = s |> String.trim() |> String.replace(~r/\s+/u, " ")
    words  = if s_norm == "", do: [], else: String.split(s_norm, " ")
    wcount = length(words)

    kept =
      Enum.filter(toks, fn t ->
        phrase = Map.get(t, :phrase) |> norm()

        case Map.get(t, :span) do
          {a, b} when is_integer(a) and is_integer(b) and a >= 0 ->
            char_match = b > 0 and norm(String.slice(s_norm, a, b) || "") == phrase

            if char_match do
              true
            else
              j = b
              window_ok = a < j and a < wcount and j <= wcount

              if window_ok do
                joined = words |> Enum.slice(a, j - a) |> Enum.join(" ") |> norm()
                joined == phrase
              else
                false
              end
            end

          _ ->
            false
        end
      end)

    %{si | tokens: kept}
  end

  defp keep_only_word_boundary_tokens(si), do: si

  # ───────────────────────── Helpers ─────────────────────────

  defp wrap_si(%SemanticInput{} = si, _orig_sentence), do: si

  defp wrap_si(tokens, sentence) when is_list(tokens),
    do: %SemanticInput{sentence: sentence, tokens: tokens, source: :test, trace: []}

  defp wrap_si(other, _sentence) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp wrap_si(_other, sentence),
    do: %SemanticInput{sentence: sentence, tokens: [], source: :test, trace: []}

  # used when merging active_cells
  defp sanitize_cell(%BrainCell{} = s), do: [s]
  defp sanitize_cell(%{id: _} = m), do: [m]
  defp sanitize_cell(%{"id" => _} = m), do: [m]
  defp sanitize_cell(id) when is_binary(id), do: [%{id: id}]
  defp sanitize_cell(_), do: []

  defp cell_id(%BrainCell{id: id}), do: id
  defp cell_id(%{id: id}), do: id
  defp cell_id(%{"id" => id}), do: id
  defp cell_id(id) when is_binary(id), do: id
  defp cell_id(_), do: nil

  # P-213: Unicode-punctuation-safe normalization
  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    # Strip leading & trailing Unicode punctuation; keep inner apostrophes/hyphens
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(v) do
    v
    |> Kernel.to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  # ─────────────────────── Introspection / Examine ───────────────────────

  @ui_regions [
    {:brain,        Brain,             :snapshot, []},
    {:lifg,         Brain.LIFG,        :status,   []},
    {:pmtg,         Brain.PMTG,        :status,   []},
    {:atl,          Brain.ATL,         :status,   []},
    {:hippocampus,  Brain.Hippocampus, :snapshot, []}
  ]

  @extra_regions [
    {:acc,          Brain.ACC,         :auto,     []},
    {:thalamus,     Brain.Thalamus,    :auto,     []},
    {:ofc,          Brain.OFC,         :auto,     []},
    {:dlpfc,        Brain.DLPFC,       :auto,     []},
    {:negcache,     Core.NegCache,     :auto,     []},
    {:curiosity,    Curiosity,         :auto,     []},
    {:core_curiosity, Core.Curiosity,  :auto,     []}
  ]

  @default_regions @ui_regions

  @doc """
  Inspect the state of brain regions.
  """
  def examine(opts) do
    compact?  = Keyword.get(opts, :compact?, false)
    ui_only?  = Keyword.get(opts, :ui_only?, true)
    all?      = Keyword.get(opts, :all?, false)

    base_regions =
      cond do
        is_list(Keyword.get(opts, :regions)) ->
          Keyword.fetch!(opts, :regions)

        all? ->
          @ui_regions ++ @extra_regions

        ui_only? ->
          @ui_regions

        true ->
          @default_regions
      end

    sup           = Keyword.get(opts, :supervisor, Symbrella.Supervisor)
    discover?     = Keyword.get(opts, :discover_from_sup?, all?)
    discovered    = if discover?, do: discover_regions_from_sup(sup), else: []

    regions =
      (base_regions ++ discovered)
      |> uniq_regions()
      |> apply_only_filter(Keyword.get(opts, :only))
      |> apply_exclude_filter(Keyword.get(opts, :exclude))

    regions
    |> Enum.map(&safe_invoke/1)
    |> Enum.each(fn
      {:ok, label, state} ->
        if compact? do
          IO.inspect(compact_state(state), label: label)
        else
          IO.inspect(state, label: label)
        end

      {:missing, label} ->
        IO.puts("#{label}: (not running / no module)")

      {:error, label, reason} ->
        IO.puts("#{label}: ERROR #{inspect(reason)}")
    end)

    :ok
  end

  def examine(), do: examine([])

  # helpers for examine
  defp safe_invoke({label, mod, fun, args}) when is_atom(label) and is_atom(mod) do
    cond do
      Code.ensure_loaded?(mod) and fun == :auto ->
        auto_snapshot(label, mod)

      Code.ensure_loaded?(mod) and function_exported?(mod, fun, length(args)) ->
        try do
          {:ok, label, apply(mod, fun, args)}
        rescue
          e -> {:error, label, e}
        catch
          kind, term -> {:error, label, {kind, term}}
        end

      true ->
        case Process.whereis(mod) do
          pid when is_pid(pid) ->
            try do
              {:ok, label, :sys.get_state(pid)}
            rescue
              e -> {:error, label, e}
            catch
              kind, term -> {:error, label, {kind, term}}
            end

          _ ->
            {:missing, label}
        end
    end
  end

  defp safe_invoke(mod) when is_atom(mod),
    do: safe_invoke({module_label(mod), mod, :auto, []})

  defp auto_snapshot(label, mod) do
    cond do
      function_exported?(mod, :status, 0)     -> {:ok, label, apply(mod, :status, [])}
      function_exported?(mod, :snapshot, 0)   -> {:ok, label, apply(mod, :snapshot, [])}
      function_exported?(mod, :state, 0)      -> {:ok, label, apply(mod, :state, [])}
      true ->
        case Process.whereis(mod) do
          pid when is_pid(pid) ->
            try do
              {:ok, label, :sys.get_state(pid)}
            rescue
              e -> {:error, label, e}
            catch
              kind, term -> {:error, label, {kind, term}}
            end

          _ ->
            {:missing, label}
        end
    end
  end

  defp discover_regions_from_sup(sup) do
    pid =
      case sup do
        m when is_atom(m) -> Process.whereis(m)
        p when is_pid(p)  -> p
        _ -> nil
      end

    cond do
      is_pid(pid) ->
        Supervisor.which_children(pid)
        |> Enum.flat_map(fn
          {_id, _pid, _type, [mod]} when is_atom(mod) -> [{module_label(mod), mod, :auto, []}]
          {_id, _pid, _type, mod}  when is_atom(mod) -> [{module_label(mod), mod, :auto, []}]
          _ -> []
        end)

      true ->
        []
    end
  end

  defp module_label(mod) when is_atom(mod) do
    mod
    |> Atom.to_string()
    |> String.split(".")
    |> List.last()
    |> Macro.underscore()
    |> String.to_atom()
  end

  defp label_from_selector(sel) when is_atom(sel) do
    case Atom.to_string(sel) do
      "Elixir." <> _ -> module_label(sel)
      _ -> sel
    end
  end

  defp uniq_regions(specs) do
    specs
    |> Enum.uniq_by(fn
      {label, _m, _f, _a} -> label
      m when is_atom(m)   -> module_label(m)
      other               -> other
    end)
  end

  defp apply_only_filter(specs, nil), do: specs
  defp apply_only_filter(specs, only) when is_list(only) do
    wanted = only |> Enum.map(&label_from_selector/1) |> MapSet.new()
    Enum.filter(specs, fn
      {label, _m, _f, _a} -> MapSet.member?(wanted, label)
      m when is_atom(m)   -> MapSet.member?(wanted, module_label(m))
      _                   -> false
    end)
  end

  defp apply_exclude_filter(specs, nil), do: specs
  defp apply_exclude_filter(specs, exclude) when is_list(exclude) do
    blocked = exclude |> Enum.map(&label_from_selector/1) |> MapSet.new()
    Enum.reject(specs, fn
      {label, _m, _f, _a} -> MapSet.member?(blocked, label)
      m when is_atom(m)   -> MapSet.member?(blocked, module_label(m))
      _                   -> false
    end)
  end

  defp compact_state(%{} = map) do
    keys = Map.keys(map)
    %{
      __summary__: true,
      size: map_size(map),
      keys: Enum.take(Enum.sort(keys), 10)
    }
  end

  defp compact_state(list) when is_list(list), do: %{__summary__: true, length: length(list)}
  defp compact_state(other), do: other

  # ─────────────────────── MWE shadowing helpers ───────────────────────

  defp mwe_shadow(lifg_choices, tokens) when is_list(lifg_choices) and is_list(tokens) do
    mwe_spans =
      lifg_choices
      |> Enum.map(& &1.token_index)
      |> Enum.map(&Enum.at(tokens, &1, %{}))
      |> Enum.filter(fn t -> Map.get(t, :n, 1) > 1 end)
      |> Enum.map(&Map.get(&1, :span))

    if mwe_spans == [] do
      lifg_choices
    else
      Enum.reject(lifg_choices, fn ch ->
        t = Enum.at(tokens, ch.token_index, %{})
        if Map.get(t, :n, 1) == 1 do
          covered_by_any_mwe?(Map.get(t, :span), mwe_spans)
        else
          false
        end
      end)
    end
  end

  defp covered_by_any_mwe?({a, l}, spans) when is_integer(a) and is_integer(l) do
    b = a + l
    Enum.any?(spans, fn
      {s, sl} when is_integer(s) and is_integer(sl) ->
        e = s + sl
        a >= s and b <= e
      _ -> false
    end)
  end

  defp covered_by_any_mwe?(_, _), do: false

  # ─────────────────────── Local telemetry shim ───────────────────────

  defp emit(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end

  # ─────────────────────── Small util ───────────────────────
  defp put_if_present(kvs, _k, nil), do: kvs
  defp put_if_present(kvs, k, v),   do: Keyword.put(kvs, k, v)

  # Gate + forwarder for MWE Signatures
  defp maybe_run_mwe(si, stage, opts) do
    env_on  = Application.get_env(:core, :mwe_signatures, :on) != :off
    opt_val = Keyword.get(opts, :mwe_signatures, :inherit)
    enabled = case opt_val do
      :off -> false
      false -> false
      :inherit -> env_on
      _ -> true
    end

    if enabled do
      Signatures.run(si,
        stage: stage,
        extra_lex: Keyword.get(opts, :mwe_extra_lex, []),
        multiplier: Keyword.get(opts, :mwe_multiplier, 1.0),
        demote_funcs?: Keyword.get(opts, :mwe_demote_funcs?, true)
      )
    else
      si
    end
  end
end

