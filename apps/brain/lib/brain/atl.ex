defmodule Brain.ATL do
  @moduledoc """
  Anterior Temporal Lobe (ATL) — semantic hub & consolidation.

  • Runs AFTER Brain.LIFG picks per-token winners.
  • Groups winners by lemma/norm and id, keeps light rolling stats.
  • Exposes a snapshot for downstream regions.

  Public API:
    - start_link/1
    - ingest/1        # ingest %{lifg_choices, tokens}
    - ingest/2        # ingest choices + tokens directly
    - reduce/1,2      # pure version (no server required)
    - snapshot/0
    - reset/0
  """

  use GenServer
  @name __MODULE__

  # ── Public API ────────────────────────────────────────────────────────────

  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, opts, name: @name)

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

  @doc "Pure reducer (no server): takes %{lifg_choices, tokens}."
  @spec reduce(map()) :: map()
  def reduce(%{lifg_choices: choices, tokens: tokens}),
    do: reduce(List.wrap(choices), List.wrap(tokens))

  @doc "Pure reducer (no server): takes winners and tokens directly."
  @spec reduce(list(), list()) :: map()
  def reduce(choices, tokens) when is_list(choices) and is_list(tokens) do
    winners = Enum.map(choices, &normalize_choice/1)
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

  @doc "Peek current ATL state."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Clear rolling window & counters."
  @spec reset() :: :ok
  def reset, do: GenServer.call(@name, :reset)

  # ── GenServer callbacks ──────────────────────────────────────────────────

  @impl true
  def init(opts) do
    keep = Keyword.get(opts, :keep, 100)

    {:ok,
     %{
       keep: keep,
       last_slate: %{},
       concept_counts: %{}, # norm => count
       sense_counts: %{},   # id   => count
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

  # ── Internals ────────────────────────────────────────────────────────────

  # Accept keys as atoms or strings; tolerate missing fields.
  defp normalize_choice(ch) when is_map(ch) do
    id    = fetch_any(ch, [:id, "id"])
    score = fetch_any(ch, [:score, "score"]) || 0.0
    lemma = fetch_any(ch, [:lemma, "lemma"]) |> norm_text()
    norm  = id_norm(id) || lemma

    %{id: id, score: score, lemma: lemma, norm: norm, raw: ch}
  end

  defp normalize_choice(other), do: %{id: nil, score: 0.0, lemma: "", norm: "", raw: other}

  defp fetch_any(map, keys) do
    Enum.reduce_while(keys, nil, fn k, _acc ->
      case Map.get(map, k) do
        nil -> {:cont, nil}
        v   -> {:halt, v}
      end
    end)
  end

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
end

