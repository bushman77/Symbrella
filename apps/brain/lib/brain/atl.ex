defmodule Brain.ATL do
  @moduledoc """
  Anterior Temporal Lobe (ATL) — semantic hub & consolidation.

  • Runs AFTER Brain.LIFG picks per-token winners.
  • Groups winners by lemma/norm and id, keeps light rolling stats.
  • Exposes a snapshot for downstream regions.

  Public API:
    - start_link/1         # provided by `use Brain, region: :atl`
    - ingest/1             # ingest %{lifg_choices, tokens} (server)
    - ingest/2             # ingest choices + tokens directly (server)
    - reduce/1,2           # pure version (no server required)
    - snapshot/0           # peek state
    - reset/0              # clear rolling window & counters
  """

  use Brain, region: :atl

  alias Brain.Utils.Safe
  alias Brain.LIFG.SlateFilter

  @name __MODULE__

  # ── Client API (server-backed) ───────────────────────────────────────────

  @doc "Ingest a SemanticInput-like map with :lifg_choices and :tokens; returns a slate map."
  @spec ingest(map()) :: map()
  def ingest(%{lifg_choices: choices, tokens: tokens}) do
    GenServer.call(@name, {:ingest, List.wrap(choices), List.wrap(tokens)})
  end

  @doc "Ingest raw winners + tokens; returns a slate map."
  @spec ingest(list(), list()) :: map()
  def ingest(choices, tokens) when is_list(choices) and is_list(tokens) do
    GenServer.call(@name, {:ingest, choices, tokens})
  end

  @doc "Peek current ATL state."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Clear rolling window & counters."
  @spec reset() :: :ok
  def reset, do: GenServer.call(@name, :reset)

  # ── GenServer callbacks ─────────────────────────────────────────────────

  @impl true
  def init(opts) do
    keep = Keyword.get(opts, :keep, 100)

    {:ok,
     %{
       region: :atl,
       opts: Map.new(opts),
       keep: keep,
       last_slate: %{},
       # norm => count
       concept_counts: %{},
       # id   => count
       sense_counts: %{},
       # rolling [{ts_ms, slate}, ...]
       window: []
     }}
  end

  @impl true
  def handle_call({:ingest, choices, tokens}, _from, state) do
    slate = reduce(choices, tokens)

    concept_counts =
      slate.by_norm
      |> Map.keys()
      |> Enum.reduce(state.concept_counts, fn norm, acc -> Map.update(acc, norm, 1, &(&1 + 1)) end)

    sense_counts =
      slate.by_id
      |> Map.keys()
      |> Enum.reduce(state.sense_counts, fn id, acc -> Map.update(acc, id, 1, &(&1 + 1)) end)

    ts = System.system_time(:millisecond)
    window = [{ts, slate} | state.window] |> Enum.take(state.keep)

    new_state =
      state
      |> Map.put(:last_slate, slate)
      |> Map.put(:concept_counts, concept_counts)
      |> Map.put(:sense_counts, sense_counts)
      |> Map.put(:window, window)

    {:reply, slate, new_state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call(:reset, _from, state) do
    {:reply, :ok, %{state | last_slate: %{}, concept_counts: %{}, sense_counts: %{}, window: []}}
  end

  # ── Pure reducers (no server) ───────────────────────────────────────────

  @doc "Pure reducer (no server): takes %{lifg_choices, tokens}."
  @spec reduce(map()) :: map()
  def reduce(%{lifg_choices: choices, tokens: tokens}),
    do: reduce(List.wrap(choices), List.wrap(tokens))

  @doc "Pure reducer (no server): takes winners and tokens directly."
  @spec reduce(list(), list()) :: map()
  def reduce(choices, tokens) when is_list(choices) and is_list(tokens) do
    winners =
      choices
      |> Enum.map(&normalize_choice/1)
      # only keep items with an id
      |> Enum.reject(&is_nil(&1.id))

    %{
      tokens: tokens,
      token_count: length(tokens),
      winners: winners,
      winner_count: length(winners),
      by_norm: Enum.group_by(winners, & &1.norm),
      by_id: Enum.group_by(winners, & &1.id)
    }
  end

  # ── Internals — normalize choice ─────────────────────────────────────────

  # Accepts both SenseChoice style (%{chosen_id:, lemma:, margin:, scores: ...})
  # and simpler %{id:, lemma: ...}. Falls back where possible.
  defp normalize_choice(ch) when is_map(ch) do
    id = fetch_any(ch, [:chosen_id, "chosen_id", :id, "id"])
    token_idx = fetch_any(ch, [:token_index, "token_index"])
    score = fetch_from_scores(ch, id) || fetch_any(ch, [:score, "score"]) || 0.0
    margin = fetch_any(ch, [:margin, "margin"]) || 0.0
    lemma0 = fetch_any(ch, [:lemma, "lemma"]) |> norm_text()

    %{
      id: id,
      token_index: token_idx,
      lemma: lemma0,
      norm: id_norm(id) || lemma0,
      score: score,
      margin: margin,
      raw: ch
    }
  end

  defp normalize_choice(other),
    do: %{id: nil, token_index: nil, lemma: "", norm: "", score: 0.0, margin: 0.0, raw: other}

  defp fetch_any(map, keys) do
    Enum.reduce_while(keys, nil, fn k, _acc ->
      case Map.get(map, k) do
        nil -> {:cont, nil}
        v -> {:halt, v}
      end
    end)
  end

  defp fetch_from_scores(%{scores: %{} = m}, id) when is_binary(id), do: Map.get(m, id)
  defp fetch_from_scores(%{"scores" => %{} = m}, id) when is_binary(id), do: Map.get(m, id)
  defp fetch_from_scores(_, _), do: nil

  defp id_norm(nil), do: nil
  defp id_norm(id) when is_binary(id), do: id |> String.split("|") |> List.first()

  defp norm_text(nil), do: ""

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm_text(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

  # ───────── Sense slate → si.sense_candidates ────────────────────────────

  @doc """
  Attach sense candidates to the Semantic Input (SI), keyed by token index.

  - Pulls winners and near-winners from the current ATL `slate`
  - Uses `raw.scores` when available to expand alt candidates
  - Keeps a compact, LIFG-ready payload: %{token_index => [%{id, score, ...}]}

  Also supports an optional flag:
    * `si.lifg_opts[:absorb_unigrams_into_mwe?]` — when true,
      injects winners for child unigrams whose spans lie inside a winning MWE span,
      but only when that MWE winner is a `|phrase|fallback` (to limit noise).
  """
  @spec attach_sense_candidates(map(), map(), keyword()) :: map()
  def attach_sense_candidates(si, slate, opts \\ []) when is_map(si) and is_map(slate) do
    absorb? =
      case Map.get(si, :lifg_opts) do
        kw when is_list(kw) -> Keyword.get(kw, :absorb_unigrams_into_mwe?, false)
        m when is_map(m) -> Map.get(m, :absorb_unigrams_into_mwe?, false)
        _ -> false
      end

    slate1 =
      if absorb? do
        maybe_augment_winners_with_child_unigrams(
          si,
          slate,
          inject_child_unigrams?: true,
          only_if_fallback?: true
        )
      else
        slate
      end

    candidates = promote_sense_candidates_from_slate(slate1, opts)

    existing0 = Map.get(si, :sense_candidates) || %{}
    existing = if is_map(existing0), do: existing0, else: %{}

    merged = merge_sense_candidates(existing, candidates)

    Map.put(si, :sense_candidates, merged)
  end

  # Inject winners for child unigrams when an MWE |phrase|fallback is the winner.
  defp maybe_augment_winners_with_child_unigrams(
         %{tokens: tokens} = si,
         %{winners: winners} = slate,
         opts
       )
       when is_list(tokens) and is_list(winners) do
    inject? = Keyword.get(opts, :inject_child_unigrams?, true)
    only_if_fallback = Keyword.get(opts, :only_if_fallback?, true)
    cells = Map.get(si, :active_cells, []) |> Enum.map(&Safe.to_plain/1)

    unless inject?, do: slate

    cells_by_norm =
      Enum.group_by(cells, fn c ->
        (c[:norm] || c["norm"] || c[:word] || c["word"] || "") |> down()
      end)

    extra =
      Enum.flat_map(Enum.with_index(tokens), fn {tok, idx_mwe} ->
        n = Map.get(tok, :n, 1)
        mw? = Map.get(tok, :mw, n > 1)

        if not mw? do
          []
        else
          w_for_idx = Enum.filter(winners, &(&1[:token_index] == idx_mwe))

          ok? =
            if only_if_fallback do
              Enum.any?(w_for_idx, fn w -> phrase_fallback?(w[:id]) end)
            else
              w_for_idx != []
            end

          if not ok? do
            []
          else
            mwe_span = Map.get(tok, :span)

            child_uni =
              tokens
              |> Enum.with_index()
              |> Enum.filter(fn {t, j} ->
                j != idx_mwe and Map.get(t, :n, 1) == 1 and inside?(Map.get(t, :span), mwe_span)
              end)

            Enum.flat_map(child_uni, fn {t, j} ->
              norm =
                (Map.get(t, :phrase) || Map.get(t, :word) ||
                   Map.get(t, "phrase") || Map.get(t, "word") || "")
                |> down()

              cells_by_norm
              |> Map.get(norm, [])
              |> Enum.map(fn c ->
                %{
                  id: to_string(c[:id] || c["id"]),
                  token_index: j,
                  lemma: norm,
                  norm: norm,
                  # modest seed; Stage-1 will re-score
                  score: 0.30,
                  margin: 0.0,
                  raw: %{from: :atl_child_unigram}
                }
              end)
            end)
          end
        end
      end)

    if extra == [] do
      slate
    else
      Map.update(slate, :winners, winners, fn lst ->
        (lst ++ extra) |> Enum.uniq_by(fn w -> {w[:id], w[:token_index]} end)
      end)
    end
  end

  defp maybe_augment_winners_with_child_unigrams(_si, slate, _opts), do: slate

  defp phrase_fallback?(id) when is_binary(id), do: String.contains?(id, "|phrase|fallback")
  defp phrase_fallback?(_), do: false

  defp inside?({s, e}, {ps, pe})
       when is_integer(s) and is_integer(e) and is_integer(ps) and is_integer(pe) do
    s >= ps and e <= pe
  end

  defp inside?(_, _), do: false

defp span_for(tokens, idx) do
  case Enum.find(tokens, &(&1[:index] == idx)) do
    %{span: {s, e}} when is_integer(s) and is_integer(e) and e >= s -> {s, e}
    %{span: {s, l}} when is_integer(s) and is_integer(l) and l > 0 -> {s, s + l}
    _ -> {0, 0}
  end
end

  defp down(nil), do: ""

  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")

  defp down(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.trim()
      |> String.replace(~r/\s+/u, " ")

  # ── Candidate promotion helpers ─────────────────────────────────────────

  @spec promote_sense_candidates_from_slate(map(), keyword()) ::
          %{non_neg_integer() => [map()]}
  def promote_sense_candidates_from_slate(%{winners: winners}, opts) when is_list(winners) do
    top_k = Keyword.get(opts, :top_k, 3)
    margin_window = Keyword.get(opts, :margin_window, 0.05)

    winners
    |> Enum.group_by(& &1.token_index)
    |> Enum.into(%{}, fn {idx, entries} ->
      winner = List.first(entries) || %{}
      w_score = winner[:score] || get_in(winner, [:raw, :score_norm]) || 0.0

      alts =
        winner
        |> Map.get(:raw, %{})
        |> Map.get(:scores, %{})
        |> Enum.map(fn {id, s} ->
          %{
            id: id,
            score: as_float(s),
            rank: nil,
            from: :atl_scores,
            pos: pos_from_id(id),
            norm: Map.get(winner, :norm),
            lemma: Map.get(winner, :lemma),
            features: Map.get(winner, :raw, %{}) |> Map.get(:features, %{}),
            margin: nil
          }
        end)

      winner_as_candidate = %{
        id: winner[:id],
        score: as_float(w_score),
        rank: 0,
        from: :atl_winner,
        pos: pos_from_id(winner[:id]),
        norm: winner[:norm],
        lemma: winner[:lemma],
        features: Map.get(winner, :raw, %{}) |> Map.get(:features, %{}),
        margin: Map.get(winner, :margin, 0.0)
      }

      near =
        alts
        |> Enum.reject(&is_nil(&1.id))
        |> Enum.uniq_by(& &1.id)
        |> Enum.sort_by(&(-1 * (&1.score || 0.0)))
        |> Enum.filter(fn c -> w_score <= 0.0 or c.score >= w_score * (1.0 - margin_window) end)
        |> Enum.take(top_k)
        |> Enum.with_index(1)
        |> Enum.map(fn {c, i} -> %{c | rank: i} end)

      {idx, uniq_by_id([winner_as_candidate | near])}
    end)
    |> SlateFilter.sanitize_map()
  end

  def promote_sense_candidates_from_slate(_slate, _opts), do: %{}

  defp uniq_by_id(list),
    do: list |> Enum.reject(&is_nil(&1.id)) |> Enum.uniq_by(& &1.id)

  defp as_float(nil), do: 0.0
  defp as_float(num) when is_number(num), do: num

  defp as_float(str) when is_binary(str) do
    case Float.parse(str) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp pos_from_id(nil), do: nil

  defp pos_from_id(id) when is_binary(id) do
    case String.split(id, "|") do
      [_lemma, pos, _sense] -> pos
      _ -> nil
    end
  end

  # ── Finalize slate for current SI ───────────────────────────────────────

  @doc """
  Finalize the ATL slate for the current `si`.

  Returns `{si, slate}`.

  Behavior:
    * If `si` already has `:lifg_choices`, build a fresh `slate` via `reduce/2`.
    * Otherwise, fall back to the ATL server's `last_slate` if the server is running.
    * If nothing is available, return an empty `%{}` slate.
  """
  @spec finalize(map(), keyword()) :: {map(), map()}
  def finalize(si, _opts \\ []) when is_map(si) do
    choices = Map.get(si, :lifg_choices) || Map.get(si, "lifg_choices") || []
    tokens = Map.get(si, :tokens) || Map.get(si, "tokens") || []

    slate =
      cond do
        is_list(choices) and choices != [] and is_list(tokens) ->
          reduce(choices, tokens)

        true ->
          case Process.whereis(@name) do
            nil ->
              %{}

            _pid ->
              case snapshot() do
                %{last_slate: sl} when is_map(sl) -> sl
                _ -> %{}
              end
          end
      end

    {si, slate}
  end

  @doc """
  Optionally derive and attach `:lifg_pairs` (MWE↔unigram) to `si` for WM gating.

  Stores three fields in `si.evidence` when pairs exist:
    * `:lifg_pairs` — normalized `[{token_index, unigram_id}]`
    * `:lifg_pairs_by_token` — `%{token_index => [unigram_id, ...]}`
    * `:lifg_pairs_rich` — the full structs/maps when available (for introspection)

  Safe when winners are missing; returns `si` unchanged if nothing to attach.
  """
  @spec attach_lifg_pairs(map(), keyword()) :: map()
  def attach_lifg_pairs(si, opts \\ []) when is_map(si) do
    if Keyword.get(opts, :derive_lifg_pairs?, true) do
      tokens = Map.get(si, :tokens, [])
      slate = Map.get(si, :atl_slate, %{})
      winners = Map.get(slate, :winners, [])

      pairs_rich_or_tuples =
        cond do
          is_list(winners) and winners != [] ->
            cells =
              si
              |> Map.get(:active_cells, [])
              |> Enum.map(&Brain.Utils.Safe.to_plain/1)

            # returns a list of maps with keys: :token_index, :unigram_id, :mwe_id, ...
            derive_lifg_pairs(winners, tokens, cells)

          is_list(Map.get(si, :lifg_choices)) ->
            # Legacy fallback: produce tuple pairs from :lifg_choices (not MWE↔unigram).
            # We still normalize them below; rich list will be empty in this branch.
            si
            |> Map.get(:lifg_choices, [])
            |> Enum.flat_map(fn ch ->
              ti = Map.get(ch, :token_index) || Map.get(ch, "token_index")
              id = Map.get(ch, :chosen_id) || Map.get(ch, "chosen_id")
              if is_integer(ti) and is_binary(id), do: [{ti, id}], else: []
            end)

          true ->
            []
        end

      # Normalize into simple `[{token_index, unigram_id}]`
      {pairs_simple, pairs_rich} =
        cond do
          pairs_rich_or_tuples == [] ->
            {[], []}

          is_list(pairs_rich_or_tuples) and
              Enum.all?(pairs_rich_or_tuples, &is_map/1) ->
            {
              pairs_rich_or_tuples
              |> Enum.flat_map(fn
                %{token_index: ti, unigram_id: id}
                when is_integer(ti) and is_binary(id) ->
                  [{ti, id}]

                _ ->
                  []
              end)
              |> Enum.uniq(),
              pairs_rich_or_tuples
            }

          is_list(pairs_rich_or_tuples) ->
            {
              pairs_rich_or_tuples
              |> Enum.flat_map(fn
                {ti, id} when is_integer(ti) and is_binary(id) -> [{ti, id}]
                _ -> []
              end)
              |> Enum.uniq(),
              []
            }
        end

      if pairs_simple == [] do
        si
      else
        by_token =
          pairs_simple
          |> Enum.group_by(fn {ti, _} -> ti end, fn {_ti, id} -> id end)

        evidence1 =
          (Map.get(si, :evidence) || %{})
          |> Map.put(:lifg_pairs, pairs_simple)
          |> Map.put(:lifg_pairs_by_token, by_token)
          |> maybe_put(:lifg_pairs_rich, pairs_rich, not Enum.empty?(pairs_rich))

        trace1 =
          (Map.get(si, :trace) || []) ++
            [
              {:lifg_pairs, %{count: length(pairs_simple), rich: length(pairs_rich)}}
            ]

        update_si_with_pairs(si, evidence1, trace1)
      end
    else
      si
    end
  end

  # ——— helper: preserves struct type if `si` is a struct ———
  defp update_si_with_pairs(%{__struct__: _} = si, evidence, trace),
    do: struct(si, evidence: evidence, trace: trace)

  defp update_si_with_pairs(si, evidence, trace) when is_map(si),
    do: Map.merge(si, %{evidence: evidence, trace: trace})

  defp maybe_put(map, _k, _v, false), do: map
  defp maybe_put(map, k, v, true), do: Map.put(map, k, v)

  # (derive_lifg_pairs/3 and helpers remain unchanged)
  defp derive_lifg_pairs(winners, tokens, cells) when is_list(winners) and is_list(tokens) do
    cells_by_norm =
      Enum.group_by(cells, fn c ->
        (c[:norm] || c["norm"] || c[:word] || c["word"] || "") |> down()
      end)

    winners
    |> Enum.filter(&phrase_fallback?(&1[:id]))
    |> Enum.flat_map(fn w ->
      idx = w[:token_index]
      mwe_span = span_for(tokens, idx)

      child_idxs =
        tokens
        |> Enum.with_index()
        |> Enum.filter(fn {t, j} ->
          j != idx and Map.get(t, :n, 1) == 1 and inside?(Map.get(t, :span), mwe_span)
        end)
        |> Enum.map(&elem(&1, 1))

      Enum.flat_map(child_idxs, fn j ->
        norm = tokens |> Enum.at(j) |> Map.get(:phrase) |> down()

        for c <- Map.get(cells_by_norm, norm, []) do
          %{
            type: :mwe_unigram,
            mwe_id: to_string(w[:id]),
            unigram_id: to_string(c[:id] || c["id"]),
            token_index: j,
            weight: 1.0,
            from: :atl
          }
        end
      end)
    end)
    |> Enum.uniq_by(&{&1.mwe_id, &1.unigram_id, &1.token_index})
  end

  defp derive_lifg_pairs(_, _, _), do: []

  defp merge_sense_candidates(a, b) when is_map(a) and is_map(b) do
    a
    |> Map.merge(b, fn _idx, old_list, new_list ->
      merge_candidate_lists(old_list, new_list)
    end)
    |> SlateFilter.sanitize_map()
  end

  defp merge_candidate_lists(old_list, new_list) do
    old = List.wrap(old_list)
    new = List.wrap(new_list)

    (new ++ old)
    |> Enum.map(&Safe.to_plain/1)
    |> Enum.filter(&is_map/1)
    |> Enum.uniq_by(fn c -> c[:id] || c["id"] end)
    |> Enum.reject(fn c -> is_nil(c[:id] || c["id"]) end)
  end
end
