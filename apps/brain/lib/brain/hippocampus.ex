defmodule Brain.Hippocampus do
  @moduledoc ~S"""
  Hippocampus — in-memory episodic window with lightweight recall + telemetry.

  ## What it does
  - Rolling window of episodes: `{timestamp, %{slate, meta}}`.
  - Pass-through `encode/2`: records the episode and returns the `slate` unchanged.
    * **Dedup-on-write:** if the new slate’s token set equals the current head,
      we refresh the head timestamp instead of appending another copy (meta is preserved).
  - `recall/2`: ranks episodes by `Jaccard(cues, episode_tokens) × recency_half_life`.
    * Optional scope filter via `:scope` (**matches either `meta` or `meta.scope`**).
    * Optional minimum-overlap via `:min_jaccard` threshold on **raw** Jaccard.
  - `:telemetry` events (emitted from the server process **and echoed to the caller**):
      - `[:brain, :hippocampus, :write]`
        - **measurements:** `%{window_size: integer}`
        - **metadata:** `%{meta: map()}`
      - `[:brain, :hippocampus, :recall]`
        - **measurements:** `%{cue_count: integer, window_size: integer, returned: integer, top_score: float}`
        - **metadata:** `%{limit: integer, half_life_ms: integer}`
  - Synchronous API: deterministic for tests, no races.

  ## Runtime options
  Use `configure/1` (or per-call opts in `recall/2`):

  - `:window_keep` (alias `:keep`) — **integer**, default **300**
  - `:half_life_ms` — **integer (ms)**, default **300_000** (5 minutes)
  - `:recall_limit` — **integer**, default **3**
  - `:min_jaccard` — **float [0.0, 1.0]**, default **0.0**
  - `:scope` (recall/2 only) — **map**

  ## Helper
  - `attach_episodes/2` writes recall results to `si.evidence[:episodes]`.
  """

  use GenServer

  @type slate    :: map()
  @type meta     :: map()
  @type episode  :: %{slate: slate(), meta: meta()}
  @type recall_r :: %{score: float(), at: non_neg_integer(), episode: episode()}

  @default_keep 300
  @default_half_life 300_000   # 5 minutes
  @default_recall_limit 3
  @default_min_jaccard 0.0

  ## Public API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))
  end

  @doc "Record an episode (slate + meta) into the rolling window. Returns `slate` unchanged."
  @spec encode(slate(), meta()) :: slate()
  def encode(slate, meta \\ %{}) when is_map(slate) and is_map(meta) do
    :ok = GenServer.call(__MODULE__, {:encode, slate, meta})
    slate
  end

  @doc """
  Recall prior episodes relevant to `cues`.

  `cues` may be:
    - a list of strings (lemmas/words), or
    - a map shaped like a `slate` (e.g., with `winners`), or
    - a full `si` map/struct that contains a `winners` or `tokens` list.

  Options:
    - `:limit` (default from config; returns top-K)
    - `:half_life_ms` (override for this call)
    - `:min_jaccard` (override for this call; default from config)
    - `:scope` (map) — all key/values must be present (==) in `episode.meta` or `episode.meta.scope`

  Returns a list of `%{score, at, episode}` sorted by score desc.
  """
  @spec recall(list(String.t()) | map(), keyword()) :: [recall_r()]
  def recall(cues, opts \\ []) when is_list(cues) or is_map(cues) do
    GenServer.call(__MODULE__, {:recall, cues, Map.new(opts)})
  end

  @doc """
  Attach episodic evidence into `si.evidence[:episodes]`.

  Passes any opts through to `recall/2` (e.g., `limit: 3`, `scope: %{...}`, `min_jaccard: ...`).
  """
  @spec attach_episodes(map(), keyword()) :: map()
  def attach_episodes(si, opts \\ []) when is_map(si) or is_struct(si) do
    episodes = recall(si, opts)
    evidence = Map.get(si, :evidence) || %{}
    Map.put(si, :evidence, Map.put(evidence, :episodes, episodes))
  end

  @doc """
  Configure runtime options. Accepts:
    - `:window_keep` or alias `:keep`
    - `:half_life_ms`
    - `:recall_limit`
    - `:min_jaccard`
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

# apps/brain/lib/brain/hippocampus.ex
def handle_call({:encode, slate, meta}, from, state) do
  now = System.system_time(:millisecond)
  new_ep  = %{slate: slate, meta: meta}
  new_set =
    slate
    |> extract_norms_from_any()
    |> Enum.reject(&empty?/1)
    |> MapSet.new()

# --- in handle_call({:encode, slate, meta}, from, state) ---

    window =
      case state.window do
        [{_at_head, ep_head} | tail] ->
          head_set = episode_token_set(ep_head)
          if MapSet.equal?(new_set, head_set) do
            # De-dup: refresh head timestamp, keep window size stable
            ep_head2 = bump_dup_count(ep_head)   # <— INCREMENT dup counter on the head
            [{now, ep_head2} | tail]
          else
            # New distinct episode: push to head, respect keep
            [{now, new_ep} | state.window] |> Enum.take(state.window_keep)
          end

        [] ->
          [{now, new_ep}]
      end

  meas = %{window_size: length(window)}
  meta_map = %{meta: meta}

  :telemetry.execute([:brain, :hippocampus, :write], meas, meta_map)
  if @test_env, do: send(elem(from, 0), {:telemetry, [:brain, :hippocampus, :write], meas, meta_map})

  new_last =
    case window do
      [{^now, ep}|_] -> {now, ep}
      _ -> {now, new_ep}
    end

  {:reply, :ok, %{state | window: window, last: new_last}}
end

  @impl true
# apps/brain/lib/brain/hippocampus.ex
def handle_call({:recall, cues, opts}, from, state) do
  now         = System.system_time(:millisecond)
  limit       = Map.get(opts, :limit, state.opts.recall_limit)
  half_life   = normalize_half_life(Map.get(opts, :half_life_ms, state.opts.half_life_ms))
  min_jacc    = normalize_min_jaccard(Map.get(opts, :min_jaccard, state.opts.min_jaccard))
  scope_opt   = Map.get(opts, :scope, nil)
  cue_set     = cue_set(cues)

  # >>> REPLACE your previous "scored =" build with THIS block <<<
  base_scored =
    if MapSet.size(cue_set) == 0 do
      []
    else
      state.window
      |> Enum.reduce([], fn {at, ep}, acc ->
        if scope_match?(ep.meta, scope_opt) do
          ep_set = episode_token_set(ep)
          jacc   = jaccard(cue_set, ep_set)

          if jacc < min_jacc do
            acc
          else
            rec = recency_factor(now - at, half_life)
            [%{score: jacc * rec, at: at, episode: ep} | acc]
          end
        else
          acc
        end
      end)
      |> Enum.filter(&(&1.score > 0.0))
      |> Enum.sort_by(& &1.score, :desc)
    end

  # Expand by duplicate count, then respect the limit
  scored =
    base_scored
    |> Enum.flat_map(fn r -> List.duplicate(r, dup_count(r.episode)) end)
    |> Enum.take(limit)
  # <<< END replacement >>>

  meas = %{
    cue_count: MapSet.size(cue_set),
    window_size: length(state.window),
    returned: length(scored),
    top_score: case scored do
      [%{score: s} | _] -> s
      _ -> 0.0
    end
  }

  meta_map = %{
    limit: limit,
    half_life_ms: half_life
  }

  :telemetry.execute([:brain, :hippocampus, :recall], meas, meta_map)
  if @test_env do
    send(elem(from, 0), {:telemetry, [:brain, :hippocampus, :recall], meas, meta_map})
  end

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

    min_jaccard =
      opts
      |> Map.get(:min_jaccard, state.opts.min_jaccard)
      |> normalize_min_jaccard()

    new_opts =
      state.opts
      |> Map.put(:window_keep, keep)
      |> Map.put(:half_life_ms, half_life)
      |> Map.put(:recall_limit, recall_limit)
      |> Map.put(:min_jaccard, min_jaccard)

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
        recall_limit: @default_recall_limit,
        min_jaccard: @default_min_jaccard
      }
    }
  end

  defp normalize_keep(k) when is_integer(k) and k > 0, do: k
  defp normalize_keep(_), do: @default_keep

  defp normalize_half_life(h) when is_integer(h) and h > 0, do: h
  defp normalize_half_life(_), do: @default_half_life

  defp normalize_limit(k) when is_integer(k) and k > 0, do: k
  defp normalize_limit(_), do: @default_recall_limit

  defp normalize_min_jaccard(x) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  defp normalize_min_jaccard(_), do: @default_min_jaccard

  # --- token/cue extraction and scoring ---

# apps/brain/lib/brain/hippocampus.ex
# --- PATCH 1: accept list-of-maps cues (not just list of strings) ---
defp cue_set(list) when is_list(list) do
  if Enum.any?(list, &is_map/1) do
    list
    |> Enum.map(fn
      %{} = m -> winner_norm(m) || token_norm(m)
      other   -> norm_str(other)
    end)
    |> Enum.reject(&empty?/1)
    |> MapSet.new()
  else
    list
    |> Enum.map(&norm_str/1)
    |> Enum.reject(&empty?/1)
    |> MapSet.new()
  end
end


  defp episode_token_set(%{slate: slate}) do
    slate
    |> extract_norms_from_any()
    |> Enum.reject(&empty?/1)
    |> MapSet.new()
  end

  # Accept several shapes: %{winners: [...]}, %{"winners" => [...]}, %{tokens: [...]}, %{"tokens" => [...]}
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
    val =
      Map.get(map, :lemma) ||
        Map.get(map, "lemma") ||
        parse_id_word(Map.get(map, :id) || Map.get(map, "id")) ||
        Map.get(map, :word) ||
        Map.get(map, "word")

    norm_str(val)
  end

# apps/brain/lib/brain/hippocampus.ex

# ... keep everything else the same ...
# --- PATCH 2: tokens can be ID-only; fall back to parse from :id ---
defp token_norm(map) when is_map(map) do
  val =
    Map.get(map, :norm) ||
      Map.get(map, "norm") ||
      Map.get(map, :lemma) ||
      Map.get(map, "lemma") ||
      Map.get(map, :word) ||
      Map.get(map, "word") ||
      # fallback when tokens only carry an :id like "alpha|noun|0"
      parse_id_word(Map.get(map, :id) || Map.get(map, "id"))

  norm_str(val)
end

  defp norm_str(nil), do: nil
  defp norm_str(s) when is_binary(s), do: s |> String.trim() |> String.downcase()

  defp parse_id_word(nil), do: nil
  defp parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

# Keep the existing helper, but make the union a touch clearer/robus
defp jaccard(a, b) do
  ai = MapSet.size(MapSet.intersection(a, b))
  au = MapSet.size(a) + MapSet.size(b) - ai
  if au == 0, do: 0.0, else: ai / au
end

  defp recency_factor(age_ms, half_life_ms) when half_life_ms > 0 do
    :math.pow(0.5, age_ms / half_life_ms)
  end

  # Scope matching: allow `scope` to match either top-level meta OR nested `meta.scope`
  defp scope_match?(meta, scope) when is_map(scope) and map_size(scope) > 0 and is_map(meta) do
    effective =
      case Map.get(meta, :scope) || Map.get(meta, "scope") do
        s when is_map(s) -> Map.merge(meta, s)
        _ -> meta
      end

    Enum.all?(scope, fn {k, v} -> fetch_meta(effective, k) == v end)
  end

  defp scope_match?(_meta, _scope), do: true

  defp fetch_meta(meta, key) when is_map(meta) do
    cond do
      is_atom(key) -> Map.get(meta, key) || Map.get(meta, Atom.to_string(key))
      is_binary(key) ->
        Map.get(meta, key) ||
          case Enum.find(meta, fn
                 {mk, _} when is_atom(mk) -> Atom.to_string(mk) == key
                 _ -> false
               end) do
            {_, v} -> v
            _ -> nil
          end
      true -> nil
    end
  end

  defp empty?(nil), do: true
  defp empty?(""), do: true
  defp empty?(_), do: false

# Count how many times this exact token set was written consecutively (collapsed by dedup).
defp bump_dup_count(%{meta: m} = ep) when is_map(m) do
  cnt = Map.get(m, :dup_count, 1)
  %{ep | meta: Map.put(m, :dup_count, cnt + 1)}
end
defp bump_dup_count(ep), do: ep

defp dup_count(%{meta: m}) when is_map(m), do: Map.get(m, :dup_count, 1)
defp dup_count(_), do: 1

end
