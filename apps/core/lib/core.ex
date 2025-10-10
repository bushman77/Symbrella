defmodule Core do
  @moduledoc """
  Tokenize → rebuild word n-grams → STM → LTM (DB read/enrich) → Lexicon Stage (fetch+upsert+refetch)
  → LIFG Stage-1 → notify Brain.
  """

  alias Brain
  alias Core.Lexicon
  alias Core.SemanticInput
  alias Db
  alias Db.BrainCell

  @type si :: map()
  @type opts :: keyword()

  @spec resolve_input(String.t(), opts()) :: SemanticInput.t()
  def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
    mode = Keyword.get(opts, :mode, :prod)
    max_n = Keyword.get(opts, :max_wordgram_n, 3)

    si0 =
      phrase
      |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n)
      # ensure %SemanticInput{}
      |> wrap_si(phrase)
      |> rebuild_word_ngrams(max_n)
      |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
      |> Map.put_new(:trace, [])

    case mode do
      :prod ->
        si0
        |> Brain.stm()
        |> keep_only_word_boundary_tokens()
        # LTM pass (includes optional seeding for *unigrams* and immediate re-fetch)
        |> ltm_stage(opts)
        # Single lexicon pass: Stage.run → DB re-fetch → merge into :active_cells
        |> Lexicon.all(opts)
        |> keep_only_word_boundary_tokens()
        |> run_lifg_and_attach(opts)
        |> maybe_ingest_atl(opts)
        |> maybe_encode_hippocampus()
        |> notify_brain_activation(opts)

      _ ->
        si0
    end
  end

  # ─────────────────────── Brain notify ───────────────────────
  # add this helper near your other privates if you don’t already have it
  defp id_norm(nil), do: nil
  defp id_norm(id) when is_binary(id), do: id |> String.split("|") |> hd()

  # Uses direct GenServer.cast/2 instead of any wrapper.
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
        tn = Map.get(t, :n, 1)

        senses =
          si.active_cells
          |> Enum.filter(fn s -> (Map.get(s, :norm) || Map.get(s, "norm")) == nrm end)
          |> Enum.filter(&compatible_cell_for_token?(tn, &1))
          |> drop_seeds_if_lexicon_present()

        if senses == [], do: acc, else: Map.put(acc, Map.get(t, :index), senses)
      end)

    ctx = [1.0]

    case Brain.lifg_stage1(
           %{candidates_by_token: groups, tokens: si.tokens, sentence: si.sentence},
           ctx,
           lifg_opts
         ) do
      {:ok, out} ->
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
            token_norm = norm(Map.get(ch, :lemma) || "")
            chosen_id = Map.get(ch, :chosen_id)
            scores = Map.get(ch, :scores) || %{}
            feats = Map.get(ch, :features) || %{}
            alt_ids = Map.get(ch, :alt_ids, [])

            base_score =
              if is_binary(chosen_id) and is_map(scores),
                do: Map.get(scores, chosen_id, 0.0),
                else: Map.get(feats, :score_norm, 0.0)

            # Prefer an id whose norm matches the token’s norm, choosing the highest score among matches.
            # Consider both the chosen_id and alt_ids.
            candidates = [chosen_id | alt_ids]

            matching =
              candidates
              |> Enum.filter(&(id_norm(&1) == token_norm))

            {chosen_id2, score2} =
              case matching do
                [] ->
                  {chosen_id, base_score}

                matches ->
                  Enum.max_by(matches, fn id -> Map.get(scores, id, -1.0) end, fn -> chosen_id end)
                  |> then(fn best -> {best, Map.get(scores, best, base_score)} end)
              end

            %{
              token_index: Map.get(ch, :token_index),
              lemma: token_norm,
              id: chosen_id2,
              alt_ids: alt_ids,
              score: score2
            }
          end)

        si
        |> Map.put(:lifg_choices, lifg_choices)
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
          pid when is_pid(pid) ->
            # server path
            Brain.ATL.ingest(choices, tokens)

          _ ->
            # pure fallback path
            Brain.ATL.reduce(choices, tokens)
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

  # If any real lexicon row (with type="lexicon" or definition) exists,
  # drop seed rows from the candidate set.
  defp drop_seeds_if_lexicon_present(senses) do
    has_lex? =
      Enum.any?(senses, fn s ->
        (Map.get(s, :type) || Map.get(s, "type")) == "lexicon" or
          is_binary(Map.get(s, :definition) || Map.get(s, "definition"))
      end)

    if has_lex?, do: Enum.reject(senses, &seed?/1), else: senses
  end

  defp maybe_encode_hippocampus(%{atl_slate: slate} = si) when is_map(slate) do
    if Process.whereis(Brain.Hippocampus) do
      ep = Brain.Hippocampus.encode(slate)
      # keep a tiny summary on SI; full episode lives in Hippocampus state
      Map.put(si, :episode, Map.take(ep, [:ts_ms, :token_count, :winner_count]))
    else
      si
    end
  end

  defp maybe_encode_hippocampus(si), do: si

  defp seed?(s), do: (Map.get(s, :type) || Map.get(s, "type")) == "seed"

  # ─────────────────────── LTM orchestrator ───────────────────────

  # Consumes Db.ltm/2. Enriches missing *unigram* norms with seeds, immediately re-fetches,
  # merges into :active_cells, and nudges Brain.
  defp ltm_stage(si, opts) when is_map(si) do
    case Db.ltm(si, opts) do
      {:ok, %{rows: rows0, missing_norms: missing0, db_hits: db_hits}} ->
        # Only seed unigrams (no spaces)
        missing =
          missing0
          |> List.wrap()
          |> Enum.reject(&String.contains?(&1, " "))

        rows =
          if missing != [] and Keyword.get(opts, :enrich_lexicon?, true) do
            _ = Lexicon.ensure_cells(missing)
            rows0 ++ Db.Lexicon.fetch_by_norms(missing)
          else
            rows0
          end

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

      _other ->
        si
    end
  end

  # ─────────────────────── Tokens / n-grams ───────────────────────

  # Build contiguous word n-grams with character spans against a normalized sentence.
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

    si |> Map.put(:sentence, s_norm) |> Map.put(:tokens, tokens)
  end

  defp rebuild_word_ngrams(si, _), do: si

  # Keep tokens where sentence slice matches token.phrase (normalized)
  # Supports char-span {start,len} and word-window {i,j} (j exclusive)
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
            # First, treat as char-span {start,len}
            char_match =
              b > 0 and
                norm(String.slice(s_norm, a, b) || "") == phrase

            if char_match do
              true
            else
              # Fallback: interpret as word-window {i,j} with j exclusive
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

  # Ensure we have a %SemanticInput{} struct after tokenize.
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

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()
end
