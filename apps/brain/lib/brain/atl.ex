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

  # Use Brain's region macro (gives us start_link/1, child_spec/1, status, etc.)
  use Brain, region: :atl

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
       # rolling state
       keep: keep,
       last_slate: %{},
       concept_counts: %{}, # norm => count (concept-level)
       sense_counts: %{},   # id   => count (sense-level)
       window: []           # [{ts_ms, slate}, ...]
     }}
  end

  @impl true
  def handle_call({:ingest, choices, tokens}, _from, state) do
    slate = reduce(choices, tokens)

    concept_counts =
      slate.by_norm
      |> Map.keys()
      |> Enum.reduce(state.concept_counts, fn norm, acc ->
        Map.update(acc, norm, 1, &(&1 + 1))
      end)

    sense_counts =
      slate.by_id
      |> Map.keys()
      |> Enum.reduce(state.sense_counts, fn id, acc ->
        Map.update(acc, id, 1, &(&1 + 1))
      end)

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
      |> Enum.reject(&is_nil(&1.id)) # only keep items with an id

    by_norm = Enum.group_by(winners, & &1.norm)
    by_id   = Enum.group_by(winners, & &1.id)

    %{
      tokens: tokens,
      token_count: length(tokens),
      winners: winners,
      winner_count: length(winners),
      by_norm: by_norm,
      by_id: by_id
    }
  end

  # ── Internals ───────────────────────────────────────────────────────────

  # Accepts both SenseChoice style (%{chosen_id:, lemma:, margin:, scores: ...})
  # and simpler %{id:, lemma: ...}. Falls back where possible.
  defp normalize_choice(ch) when is_map(ch) do
    id         = fetch_any(ch, [:chosen_id, "chosen_id", :id, "id"])
    token_idx  = fetch_any(ch, [:token_index, "token_index"])
    score_norm = fetch_from_scores(ch, id) || fetch_any(ch, [:score, "score"]) || 0.0
    margin     = fetch_any(ch, [:margin, "margin"]) || 0.0
    lemma0     = fetch_any(ch, [:lemma, "lemma"]) |> norm_text()

    %{
      id: id,
      token_index: token_idx,
      lemma: lemma0,
      norm: id_norm(id) || lemma0,
      score: score_norm,
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
        v   -> {:halt, v}
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

# ───────── Sense slate promotion → si.sense_candidates ─────────

@doc """
Attach sense candidates to the Semantic Input (SI), keyed by token index.

- Pulls winners and near-winners from the current ATL `slate`
- Uses `raw.scores` when available to expand alt candidates
- Keeps a compact, LIFG-ready payload: %{token_index => [%{id, score, ...}]}

Opts:
  * :top_k         — default 3
  * :margin_window — default 0.05 (accept alts within 5% of winner score)
"""
@spec attach_sense_candidates(map(), map(), keyword()) :: map()
def attach_sense_candidates(si, slate, opts \\ []) when is_map(si) and is_map(slate) do
  candidates = promote_sense_candidates_from_slate(slate, opts)
  Map.put(si, :sense_candidates, candidates)
end

@spec promote_sense_candidates_from_slate(map(), keyword()) ::
        %{non_neg_integer() => [map()]}
def promote_sense_candidates_from_slate(%{winners: winners} = _slate, opts) do

  top_k         = Keyword.get(opts, :top_k, 3)
  margin_window = Keyword.get(opts, :margin_window, 0.05)

  # Build per-token candidate lists from winners + raw.scores (if present)
  winners
  |> Enum.group_by(& &1.token_index)
  |> Enum.into(%{}, fn {idx, entries} ->
    # Winner for this token is entries |> Enum.max_by(&score) in most setups,
    # but your slate already has per-token winner at head; keep head as winner.
    winner = List.first(entries) || %{}
    w_score =
      winner[:score] ||
        get_in(winner, [:raw, :score_norm]) ||
        0.0

    # Expand alternatives from raw.scores if present; otherwise just keep the winner.
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

    # Include the winner explicitly (first, de-duplicated)
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

    # Filter near-winners by margin window around the winner score, take top_k
    near =
      alts
      |> Enum.reject(&is_nil(&1.id))
      |> Enum.uniq_by(& &1.id)
      |> Enum.sort_by(&(-1 * (&1.score || 0.0)))
      |> Enum.filter(fn c ->
        w_score <= 0.0 or c.score >= w_score * (1.0 - margin_window)
      end)
      |> Enum.take(top_k)
      |> Enum.with_index(1)
      |> Enum.map(fn {c, i} -> %{c | rank: i} end)

    # Final list for this token
    {idx, uniq_by_id([winner_as_candidate | near])}
  end)
end

defp uniq_by_id(list),
  do: list |> Enum.reject(&is_nil(&1.id)) |> Enum.uniq_by(& &1.id)

defp as_float(nil),  do: 0.0
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
  tokens  = Map.get(si, :tokens)       || Map.get(si, "tokens")       || []

  slate =
    cond do
      is_list(choices) and choices != [] and is_list(tokens) ->
        reduce(choices, tokens)

      true ->
        case Process.whereis(@name) do
          nil -> %{}
          _pid ->
            case snapshot() do
              %{last_slate: sl} when is_map(sl) -> sl
              _ -> %{}
            end
        end
    end

  {si, slate}
end

end

