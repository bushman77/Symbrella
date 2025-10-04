defmodule Brain.Hippocampus do
  @moduledoc ~S"""
  Hippocampus — in-memory episodic window.

  v1 Capabilities:
  - Rolling window of recent episodes: `{timestamp, %{slate, meta}}`.
  - Pass-through `encode/2`: record episode; return `slate` unchanged.
  - `recall/2`: rank prior episodes by Jaccard overlap (cue↔episode tokens)
    modulated by time decay via a configurable half-life.
  - Synchronous API for deterministic tests.
  - `configure/1` accepts:
      * :window_keep (or alias :keep)
      * :half_life_ms (recency half-life for recall scoring)
      * :recall_limit (default top-K to return)
  - `snapshot/0`, `reset/0` for introspection/cleanup.

  Scoring:
    score = Jaccard(cues, episode_tokens) * RecencyFactor(age_ms, half_life_ms)
    where RecencyFactor(a, h) = 0.5 ** (a / h)

  Notes:
  - We extract “tokens” from `slate.winners` by preferring `lemma`, else the
    `id`’s first segment before `|`, else `word`. This keeps v1 simple while
    you build richer token snapshots.
  - Persistence + vector recall can be layered on without changing this API.
  """

  use GenServer

  @type slate    :: map()
  @type meta     :: map()
  @type episode  :: %{slate: slate(), meta: meta()}
  @type recall_r :: %{score: float(), at: non_neg_integer(), episode: episode()}

  @default_keep 300
  @default_half_life 300_000   # 5 minutes
  @default_recall_limit 3

  ## Public API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))
  end

  @doc """
  Record an episode (slate + meta) into the rolling window.
  Returns `slate` unchanged so callers can pipe.
  """
  @spec encode(slate(), meta()) :: slate()
  def encode(slate, meta \\ %{}) when is_map(slate) and is_map(meta) do
    GenServer.call(__MODULE__, {:encode, slate, meta})
    slate
  end

  @doc """
  Recall prior episodes relevant to `cues`.

  `cues` may be:
    - a list of strings (lemmas/words), or
    - a map shaped like a `slate` (e.g., with `winners`), or
    - a full `si` map that contains a `winners` or `tokens` list.

  Options:
    - :limit (default from config; returns top-K)
    - :half_life_ms (override for this call)

  Returns a list of `%{score, at, episode}` sorted descending by `score`.
  """
  @spec recall(list(String.t()) | map(), keyword()) :: [recall_r()]
  def recall(cues, opts \\ []) when is_list(cues) or is_map(cues) do
    GenServer.call(__MODULE__, {:recall, cues, Map.new(opts)})
  end

  @doc """
  Configure runtime options. Accepts:
    - :window_keep or alias :keep
    - :half_life_ms
    - :recall_limit
  """
  @spec configure(keyword()) :: :ok
  def configure(opts) when is_list(opts) do
    GenServer.call(__MODULE__, {:configure, Map.new(opts)})
  end

  @doc "Reset in-memory state to defaults."
  @spec reset() :: :ok
  def reset, do: GenServer.call(__MODULE__, :reset)

  @doc "Return the full internal state map."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(__MODULE__, :snapshot)

  ## GenServer callbacks

  @impl true
  def init(_state), do: {:ok, default_state()}

  @impl true
  def handle_call({:encode, slate, meta}, _from, state) do
    now = System.system_time(:millisecond)
    ep  = %{slate: slate, meta: meta}

    window =
      [{now, ep} | state.window]
      |> Enum.take(state.window_keep)

    new_state = %{state | window: window, last: {now, ep}}
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:recall, cues, opts}, _from, state) do
    now        = System.system_time(:millisecond)
    limit      = Map.get(opts, :limit, state.opts.recall_limit)
    half_life  = normalize_half_life(Map.get(opts, :half_life_ms, state.opts.half_life_ms))
    cue_set    = cue_set(cues)

    scored =
      state.window
      |> Enum.map(fn {at, ep} ->
        ep_set = episode_token_set(ep)
        jacc   = jaccard(cue_set, ep_set)
        rec    = recency_factor(now - at, half_life)
        %{score: jacc * rec, at: at, episode: ep}
      end)
      |> Enum.filter(&(&1.score > 0.0))
      |> Enum.sort_by(& &1.score, :desc)
      |> Enum.take(limit)

    {:reply, scored, state}
  end

  @impl true
  def handle_call({:configure, opts}, _from, state) do
    keep =
      opts
      |> Map.get(:window_keep, Map.get(opts, :keep, state.window_keep))
      |> normalize_keep()

    half_life =
      opts
      |> Map.get(:half_life_ms, state.opts.half_life_ms)
      |> normalize_half_life()

    recall_limit =
      opts
      |> Map.get(:recall_limit, state.opts.recall_limit)
      |> normalize_limit()

    new_opts =
      state.opts
      |> Map.put(:window_keep, keep)
      |> Map.put(:half_life_ms, half_life)
      |> Map.put(:recall_limit, recall_limit)

    new_window = Enum.take(state.window, keep)

    {:reply, :ok, %{state | window_keep: keep, opts: new_opts, window: new_window}}
  end

  @impl true
  def handle_call(:reset, _from, _state), do: {:reply, :ok, default_state()}

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  ## Helpers

  defp default_state do
    %{
      window_keep: @default_keep,
      window: [],
      last: nil,
      opts: %{
        window_keep: @default_keep,
        half_life_ms: @default_half_life,
        recall_limit: @default_recall_limit
      }
    }
  end

  defp normalize_keep(k) when is_integer(k) and k > 0, do: k
  defp normalize_keep(_), do: @default_keep

  defp normalize_half_life(h) when is_integer(h) and h > 0, do: h
  defp normalize_half_life(_), do: @default_half_life

  defp normalize_limit(k) when is_integer(k) and k > 0, do: k
  defp normalize_limit(_), do: @default_recall_limit

  defp cue_set(list) when is_list(list) do
    list
    |> Enum.map(&norm_str/1)
    |> Enum.reject(&is_nil/1)
    |> MapSet.new()
  end

  defp cue_set(%{} = slate_or_si) do
    slate_or_si
    |> extract_norms_from_any()
    |> MapSet.new()
  end

  defp episode_token_set(%{slate: slate}) do
    slate
    |> extract_norms_from_any()
    |> MapSet.new()
  end

  defp extract_norms_from_any(%{winners: winners}) when is_list(winners),
    do: Enum.map(winners, &winner_norm/1)

  defp extract_norms_from_any(%{"winners" => winners}) when is_list(winners),
    do: Enum.map(winners, &winner_norm/1)

  defp extract_norms_from_any(%{tokens: tokens}) when is_list(tokens),
    do: Enum.map(tokens, &token_norm/1)

  defp extract_norms_from_any(%{"tokens" => tokens}) when is_list(tokens),
    do: Enum.map(tokens, &token_norm/1)

  defp extract_norms_from_any(_), do: []

  defp winner_norm(map) when is_map(map) do
    Map.get(map, :lemma)
    || Map.get(map, "lemma")
    || parse_id_word(Map.get(map, :id) || Map.get(map, "id"))
    || Map.get(map, :word)
    || Map.get(map, "word")
    |> norm_str()
  end

  defp token_norm(map) when is_map(map) do
    Map.get(map, :norm)
    || Map.get(map, "norm")
    || Map.get(map, :lemma)
    || Map.get(map, "lemma")
    || Map.get(map, :word)
    || Map.get(map, "word")
    |> norm_str()
  end

  defp norm_str(nil), do: nil
  defp norm_str(s) when is_binary(s), do: String.downcase(String.trim(s))

  defp parse_id_word(nil), do: nil
  defp parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  defp jaccard(a, b) do
    ai = MapSet.size(MapSet.intersection(a, b))
    au = MapSet.size(MapSet.union(a, b))
    if au == 0, do: 0.0, else: ai / au
  end

  defp recency_factor(age_ms, half_life_ms) when half_life_ms > 0 do
    :math.pow(0.5, age_ms / half_life_ms)
  end
end

