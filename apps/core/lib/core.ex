# apps/core/lib/core.ex
defmodule Core do
  @moduledoc """
  Pipeline:
  Tokenize → Rebuild word n-grams → STM → LTM (DB read/enrich)
  → MWE Signatures (late bias) → (Episodes attach) → LIFG Stage-1 → ATL → Hippocampus encode
  → (Optional) Persist episode → Notify Brain.

  Notes:
  • `resolve_input/2` defaults to :prod mode and runs the full pipeline.
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
      - opts:   `persist_episodes: true`
      - env:    `EPISODES_PERSIST=on|true|1`
      - config: `Application.put_env(:brain, :episodes_persist, :on)`
  """

  # ── P-253/254 knobs ─────────────────────────────────────────────────────────
  @lifg_tie_epsilon 0.01          # score margin considered "razor-thin"
  @lifg_prob_epsilon 0.01         # probability margin considered "razor-thin"

  # Bias knobs for multi-word expressions (config-driven):
  #   :mwe_greet_phrase_bump — extra score for greeting phrases ("hello there", "hey you")
  #   :mwe_general_bump      — extra score for non-greeting MWEs
  @greet_phrase_bump Application.compile_env(:core, :mwe_greet_phrase_bump, 0.01)
  @mwe_general_bump Application.compile_env(:core, :mwe_general_bump, 0.008)

  alias Brain
  alias Core.SemanticInput
  alias Core.Intent.Selection
  alias Core.MWE.Signatures
alias Core.Response
  alias Db
  alias Db.BrainCell

  @type si :: map()
  @type opts :: keyword()

  # ───────────────────────── Struct/merge helpers ─────────────────────────────

  @si_template %SemanticInput{}
  @si_fields   @si_template |> Map.from_struct() |> Map.keys() |> MapSet.new()

  # Accepts %SemanticInput{} or a plain map; returns %SemanticInput{} with safe keys
  defp coerce_si(%SemanticInput{} = si), do: si

  defp coerce_si(%{} = m) do
    filtered =
      m
      |> Enum.filter(fn {k, _} -> is_atom(k) and MapSet.member?(@si_fields, k) end)
      |> Map.new()

    struct(SemanticInput, filtered)
  end

  defp coerce_si(other) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp coerce_si(_), do: %SemanticInput{}

  # Merge only whitelisted keys from a Brain map back into the struct
  defp merge_into_si(%SemanticInput{} = si, %{} = brain_out) do
    Enum.reduce(brain_out, si, fn {k, v}, acc ->
      if MapSet.member?(@si_fields, k), do: Map.put(acc, k, v), else: acc
    end)
  end

  # Call a Brain function with a plain map; merge result safely into struct.
  # Accepts either a %SemanticInput{} or a plain map (coerces maps into the struct).
  defp brain_roundtrip(si_or_map, fun) when is_function(fun, 1) do
    si = coerce_si(si_or_map)
    brain_in = Map.from_struct(si)

    case fun.(brain_in) do
      %{} = out -> merge_into_si(si, out)
      _         -> si
    end
  end

  # ───────────────────────── Public API ───────────────────────────────────────

@spec resolve_input(String.t(), opts()) :: SemanticInput.t()
def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
  mode  = Keyword.get(opts, :mode, :prod)
  max_n = Keyword.get(opts, :max_wordgram_n, 3)

  # Optionally pull defaults for LIFG (always on unless overridden)
  lifg_defaults = Application.get_env(:brain, :lifg_defaults, [])
  lifg_opts     = Keyword.merge(lifg_defaults, Keyword.get(opts, :lifg_opts, []))

  si0 =
    phrase
    |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n)
    |> wrap_si(phrase)
    |> rebuild_word_ngrams(max_n)
    |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
    |> Map.put_new(:trace, [])
    |> Map.put(:lifg_opts, lifg_opts)

  # Be compatible with both Selection.select/2 shapes:
  # - legacy: returns a plain si (map/struct)
  # - new:    returns {si, res}
  sel_out = Selection.select(si0, opts)

  si1 =
    case sel_out do
      {si, _res} -> coerce_si(si)
      si         -> coerce_si(si)
    end

  case mode do
    :prod ->
      si1
      |> brain_roundtrip(&Brain.stm/1)
      |> keep_only_word_boundary_tokens()
      |> maybe_run_mwe(:early, opts)
      |> ltm_stage(opts)
      |> keep_only_word_boundary_tokens()
      |> maybe_run_mwe(:late, opts)
      |> Core.Relations.attach_edges()
      |> maybe_attach_episodes(opts)
      |> maybe_amygdala_react(opts)
      # 🔽 use LIFG-specific opts here
      |> run_lifg_and_attach(lifg_opts)
      |> maybe_ingest_atl(opts)
      |> brain_roundtrip(&Brain.ATL.attach_lifg_pairs(&1, opts))
      |> maybe_encode_hippocampus()
      |> maybe_persist_episode(opts)
      |> maybe_build_response_plan(opts)
      |> notify_brain_activation(opts)
      |> maybe_attach_response(opts)

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

  # ─────────────────────── LIFG (full pipeline) ───────────────────────

# apps/core/lib/core.ex

defp run_lifg_and_attach(%{tokens: tokens} = si, lifg_opts) when is_list(tokens) do
  # Pass intent bias downstream (non-breaking; ignored if unused)
  lifg_opts2 = Keyword.put(lifg_opts, :intent_bias, Map.get(si, :intent_bias, %{}))

  case safe_lifg_run(si, lifg_opts2) do
    {:ok, %{si: si_after, choices: raw_choices, slate: slate, flips: flips}} ->
      # Prefer tokens from si_after in case LIFG ever enriches them;
      # fall back to the original tokens.
      tokens_for_choices = Map.get(si_after, :tokens, tokens)

      lifg_choices =
        raw_choices
        |> Enum.map(fn ch ->
          token_index = Map.get(ch, :token_index, 0)
          t = Enum.at(tokens_for_choices, token_index, %{})
          token_norm = t |> Map.get(:phrase, "") |> norm()

          chosen_id   = Map.get(ch, :chosen_id)
          scores      = Map.get(ch, :scores) || %{}
          feats       = Map.get(ch, :features) || %{}
          alt_ids     = Map.get(ch, :alt_ids, [])
          margin      = Map.get(ch, :margin, 0.0)
          prob_margin = Map.get(ch, :prob_margin, 0.0)

          phrase_bump_for = fn id, thin? ->
            cond do
              Map.get(t, :n, 1) > 1 and id_norm(id) == token_norm ->
                base  = if thin?, do: @mwe_general_bump, else: 0.0
                greet =
                  if Map.get(si_after, :intent, si.intent) == :greet,
                    do: @greet_phrase_bump,
                    else: 0.0

                base + greet

              true ->
                0.0
            end
          end

          base_score =
            if is_binary(chosen_id) and is_map(scores),
              do: Map.get(scores, chosen_id, 0.0),
              else: Map.get(feats, :score_norm, 0.0)

          candidates =
            [chosen_id | alt_ids]
            |> Enum.uniq()
            |> Enum.reject(&is_nil/1)

          matching = Enum.filter(candidates, &(id_norm(&1) == token_norm))

          pos_prior = %{
            "phrase"   => 0.03,
            "noun"     => 0.01,
            "verb"     => 0.0,
            "adjective"=> 0.0
          }

          thin? = margin < @lifg_tie_epsilon or prob_margin < @lifg_prob_epsilon
          score_of = fn id -> Map.get(scores, id, base_score) + phrase_bump_for.(id, thin?) end

          pick_with_prior = fn ids ->
            Enum.max_by(
              ids,
              fn id -> score_of.(id) + Map.get(pos_prior, id_pos(id), 0.0) end,
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
        |> mwe_shadow(tokens_for_choices)

      ev = %{
        stage: :lifg_run,
        ts_ms: System.system_time(:millisecond),
        choice_count: length(raw_choices),
        flips: flips
      }

      si_after
      |> Map.put(:atl_slate, slate)
      |> Map.put(:lifg_choices, lifg_choices)
      |> Map.put(:acc_conflict, Map.get(si_after, :acc_conflict, 0.0))
      |> Map.update(:trace, [], &[ev | &1])

    {:error, _reason} ->
      si
  end
end

# Fallback clause: if someone accidentally passes an emotion map or anything
# without :tokens, just give it back untouched instead of blowing up.
defp run_lifg_and_attach(si, _lifg_opts), do: si

  defp safe_lifg_run(si, opts) do
    cond do
      Code.ensure_loaded?(Brain) and function_exported?(Brain, :lifg_run, 2) ->
        try do
          apply(Brain, :lifg_run, [si, opts])
        rescue
          _ -> apply(Brain.LIFG, :run, [si, opts])
        end

      Code.ensure_loaded?(Brain.LIFG) and function_exported?(Brain.LIFG, :run, 2) ->
        apply(Brain.LIFG, :run, [si, opts])

      true ->
        {:error, :lifg_unavailable}
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

  defp maybe_encode_hippocampus(%{atl_slate: slate} = si) when is_map(slate) do
    if Process.whereis(Brain.Hippocampus) do
      ep = Brain.Hippocampus.encode(slate)
      Map.put(si, :episode, Map.take(ep, [:ts_ms, :token_count, :winner_count]))
    else
      si
    end
  end

  defp maybe_encode_hippocampus(si), do: si

  # ─────────────────────── Episodes attach ───────────────────────

  defp maybe_attach_episodes(si, opts) when is_map(si) do
    enabled? = episodes_enabled?(opts)
    hippo_up? = is_pid(Process.whereis(Brain.Hippocampus))

    cond do
      not enabled? or not hippo_up? ->
        si

      true ->
        pass =
          []
          |> put_if_present(:source, Keyword.get(opts, :recall_source))
          |> put_if_present(:embedding, Keyword.get(opts, :episode_embedding))

        si2 = Brain.Hippocampus.attach_episodes(si, pass)
        eps = get_in(si2, [:evidence, :episodes]) || []
        emit([:brain, :core, :episodes_attached], %{count: length(eps)}, %{mode: :pre_lifg})
        si2
    end
  catch
    _, _ -> si
  end

  defp episodes_enabled?(opts) do
    opt = Keyword.get(opts, :episodes, nil)

    case opt do
      true -> true
      false -> false
      _ -> Application.get_env(:brain, :episodes_mode, :on) != :off
    end
  end

  # ─────────────────────── Optional persistence ───────────────────────

  defp maybe_persist_episode(si, opts) when is_map(si) do
    Brain.Hippocampus.Writer.maybe_persist(
      si,
      persist: Keyword.get(opts, :persist_episodes),
      embedding: Keyword.get(opts, :episode_embedding),
      user_id: Keyword.get(opts, :user_id)
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
    words = if s_norm == "", do: [], else: String.split(s_norm, " ")

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
        start = Enum.at(starts, i)
        len = String.length(phrase)

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
    words = if s_norm == "", do: [], else: String.split(s_norm, " ")
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
    {:brain, Brain, :snapshot, []},
    {:lifg, Brain.LIFG, :status, []},
    {:pmtg, Brain.PMTG, :status, []},
    {:atl, Brain.ATL, :status, []},
    {:hippocampus, Brain.Hippocampus, :snapshot, []}
  ]

  @extra_regions [
    {:acc, Brain.ACC, :auto, []},
    {:thalamus, Brain.Thalamus, :auto, []},
    {:ofc, Brain.OFC, :auto, []},
    {:dlpfc, Brain.DLPFC, :auto, []},
    {:negcache, Core.NegCache, :auto, []},
    {:curiosity, Curiosity, :auto, []},
    {:core_curiosity, Core.Curiosity, :auto, []}
  ]

  @default_regions @ui_regions

  @doc """
  Inspect the state of brain regions.
  """
  def examine(opts \\ []) do
    compact? = Keyword.get(opts, :compact?, false)
    ui_only? = Keyword.get(opts, :ui_only?, true)
    all? = Keyword.get(opts, :all?, false)

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

    sup = Keyword.get(opts, :supervisor, Symbrella.Supervisor)
    discover? = Keyword.get(opts, :discover_from_sup?, all?)
    discovered = if discover?, do: discover_regions_from_sup(sup), else: []

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
      function_exported?(mod, :status, 0) ->
        {:ok, label, apply(mod, :status, [])}

      function_exported?(mod, :snapshot, 0) ->
        {:ok, label, apply(mod, :snapshot, [])}

      function_exported?(mod, :state, 0) ->
        {:ok, label, apply(mod, :state, [])}

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
        p when is_pid(p) -> p
        _ -> nil
      end

    cond do
      is_pid(pid) ->
        Supervisor.which_children(pid)
        |> Enum.flat_map(fn
          {_id, _pid, _type, [mod]} when is_atom(mod) -> [{module_label(mod), mod, :auto, []}]
          {_id, _pid, _type, mod} when is_atom(mod) -> [{module_label(mod), mod, :auto, []}]
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
      m when is_atom(m) -> module_label(m)
      other -> other
    end)
  end

  defp apply_only_filter(specs, nil), do: specs

  defp apply_only_filter(specs, only) when is_list(only) do
    wanted = only |> Enum.map(&label_from_selector/1) |> MapSet.new()

    Enum.filter(specs, fn
      {label, _m, _f, _a} -> MapSet.member?(wanted, label)
      m when is_atom(m) -> MapSet.member?(wanted, module_label(m))
      _ -> false
    end)
  end

  defp apply_exclude_filter(specs, nil), do: specs

  defp apply_exclude_filter(specs, exclude) when is_list(exclude) do
    blocked = exclude |> Enum.map(&label_from_selector/1) |> MapSet.new()

    Enum.reject(specs, fn
      {label, _m, _f, _a} -> MapSet.member?(blocked, label)
      m when is_atom(m) -> MapSet.member?(blocked, module_label(m))
      _ -> false
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
  defp put_if_present(kvs, k, v), do: Keyword.put(kvs, k, v)

  # Gate + forwarder for MWE Signatures
  defp maybe_run_mwe(si, stage, opts) do
    env_on = Application.get_env(:core, :mwe_signatures, :on) != :off
    opt_val = Keyword.get(opts, :mwe_signatures, :inherit)

    enabled =
      case opt_val do
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

    # Amygdala hook: fast affective appraisal between episodes + LIFG
defp maybe_amygdala_react(si, opts) do
  if Code.ensure_loaded?(Brain.Amygdala) and
       function_exported?(Brain.Amygdala, :react, 2) do
    try do
      case Brain.Amygdala.react(si, opts) do
        # Amygdala returns an emotion summary map like
        # %{from: ..., latents: ..., tone_reaction: ..., ...}
        %{} = emotion ->
          # 🔑 Keep the SemanticInput, just enrich it
          Map.put(si, :emotion, emotion)

        # If it returns something unexpected, don’t break the pipeline
        _ ->
          si
      end
    rescue
      _ -> si
    catch
      _, _ -> si
    end
  else
    si
  end
end

  # ─────────────────────── Response planner hook ───────────────────────

  # Public-ish gate: lets us turn the planner off via opts if needed.
  defp maybe_attach_response(si, opts) when is_map(si) do
    case Keyword.get(opts, :response, :auto) do
      :off -> si
      _    -> attach_response(si)
    end
  end

  defp maybe_attach_response(si, _opts), do: si

  # Build a skinny input for Core.Response.plan/2 from the SI + trace.
  defp attach_response(%{sentence: sentence} = si) do
    {intent, keyword, confidence} = intent_from_trace(si)

    planner_si = %{
      intent: intent,
      keyword: keyword,
      confidence: confidence,
      text: sentence || ""
    }

    # For now we let the planner run with no explicit mood sample;
    # it’s written to handle an empty context safely.
    mood_ctx = %{}

    {tone, text, meta} = Core.Response.plan(planner_si, mood_ctx)

    si
    |> Map.put(:response_tone, tone)
    |> Map.put(:response_text, text)
    |> Map.put(:response_meta, meta)
  rescue
    _ -> si
  end

  defp intent_from_trace(%{trace: trace} = si) when is_list(trace) do
    default = {:unknown, Map.get(si, :sentence, "") || "", 0.0}

    trace
    |> Enum.reverse()
    |> Enum.find_value(default, fn
      {:intent, %{intent: intent, keyword: kw, confidence: conf}} ->
        {
          intent || :unknown,
          (kw || Map.get(si, :sentence, "")) || "",
          conf || 0.0
        }

      _ ->
        nil
    end)
  end

  defp intent_from_trace(si),
    do: {:unknown, Map.get(si, :sentence, "") || "", 0.0}


  # ─────────────────────── Response planner hook ───────────────────────

  # Attach Core.Response planning output onto the SI as:
  #   :response_tone, :response_text, :response_meta
  #
  # This is a no-op if Response.plan/2 is unavailable for any reason.
  defp maybe_build_response_plan(%{} = si, opts) do
    # Only attempt to call the planner if the module is available
    if Code.ensure_loaded?(Response) and function_exported?(Response, :plan, 2) do
      si_like  = build_response_si_like(si)
      mood_like = build_response_mood_like(si, opts)

      case Response.plan(si_like, mood_like) do
        {tone, text, meta} ->
          si
          |> Map.put(:response_tone, tone)
          |> Map.put(:response_text, text)
          |> Map.put(:response_meta, meta)

        _other ->
          si
      end
    else
      si
    end
  rescue
    _ -> si
  catch
    _, _ -> si
  end

  defp maybe_build_response_plan(si, _opts), do: si

  # Build the minimal SI-like map that Core.Response expects.
  # Priority for intent/keyword/confidence:
  #   1) top-level SI (if present and concrete)
  #   2) Amygdala emotion.from
  #   3) trace entry {:intent, %{...}}
  #   4) final fallback: unknown / 0.0 / sentence-as-text
  defp build_response_si_like(si) do
    {intent, confidence, keyword, text} = pick_intent_conf_keyword_and_text(si)

    %{
      intent: intent,
      keyword: keyword,
      confidence: confidence,
      text: text
    }
  end

  defp pick_intent_conf_keyword_and_text(si) do
    # Pull any intent snapshot the pipeline produced
    from_emotion = get_in(si, [:emotion, :from]) || %{}
    from_trace   = find_intent_trace(si) || %{}

    base_text =
      Map.get(si, :sentence) ||
        Map.get(si, :text) ||
        Map.get(si, :keyword) ||
        Map.get(from_emotion, :keyword) ||
        Map.get(from_trace, :keyword) ||
        ""

    intent_si     = Map.get(si, :intent)
    intent_em     = Map.get(from_emotion, :intent)
    intent_trace  = Map.get(from_trace, :intent)

    intent =
      cond do
        intent_si not in [nil, :unknown, :other] ->
          intent_si

        intent_em not in [nil, :unknown, :other] ->
          intent_em

        intent_trace not in [nil, :unknown, :other] ->
          intent_trace

        true ->
          :unknown
      end

    conf_si    = Map.get(si, :confidence)
    conf_em    = Map.get(from_emotion, :confidence)
    conf_trace = Map.get(from_trace, :confidence)

    confidence =
      cond do
        is_number(conf_si) -> conf_si
        is_number(conf_em) -> conf_em
        is_number(conf_trace) -> conf_trace
        true -> 0.0
      end

    keyword =
      Map.get(si, :keyword) ||
        Map.get(from_emotion, :keyword) ||
        Map.get(from_trace, :keyword) ||
        base_text

    {intent, confidence, keyword, to_string(base_text)}
  end

  # Look inside si.trace for the latest {:intent, %{...}} entry
  defp find_intent_trace(%{trace: trace}) when is_list(trace) do
    trace
    |> Enum.reverse() # prefer the most recent intent event
    |> Enum.find_value(fn
      {:intent, %{} = payload} -> payload
      _ -> nil
    end)
  end

  defp find_intent_trace(_), do: nil

  # Build the mood-like map from Amygdala emotion (if present).
  # We map Amygdala latents → exploration / inhibition / vigilance / plasticity,
  # and forward tone_reaction as a tone_hint.
  defp build_response_mood_like(si, _opts) do
    emotion = Map.get(si, :emotion, %{})
    tone_hint =
      case emotion do
        %{} -> Map.get(emotion, :tone_reaction)
        _ -> nil
      end

    latents = Map.get(emotion, :latents, %{})

    control = clamp01(Map.get(latents, :control, 0.0))
    threat  = clamp01(Map.get(latents, :threat, 0.0))
    safety  = clamp01(Map.get(latents, :safety, 0.0))
    reward  = clamp01(Map.get(latents, :reward, 0.0))

    # Heuristic mapping:
    #   exploration ≈ reward
    #   inhibition  ≈ control
    #   vigilance   ≈ threat + (1 - safety) + a bit of control
    #   plasticity  ≈ baseline 0.5 tilted by reward vs threat
    vigilance =
      0.6 * threat +
        0.2 * (1.0 - safety) +
        0.2 * control
      |> clamp01()

    exploration = reward
    inhibition  = control
    plasticity  =
      0.5 +
        0.3 * (reward - threat)
      |> clamp01()

    %{
      mood: %{
        exploration: exploration,
        inhibition: inhibition,
        vigilance: vigilance,
        plasticity: plasticity
      },
      tone_hint: tone_hint
    }
  end

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp01(_), do: 0.0

end

