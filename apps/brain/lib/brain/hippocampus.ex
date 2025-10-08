defmodule Brain.Hippocampus do
  @moduledoc """
  Hippocampus — in-memory episodic window with lightweight recall + telemetry.

  Public API:
    • encode/2 — record an episode and return the `slate` unchanged (de-dup on head).
    • recall/2 — rank prior episodes by Jaccard×recency with optional scope/min_jaccard.
    • attach_episodes/2 — write recall results to `si.evidence[:episodes]`.
    • configure/1, reset/0, snapshot/0 — runtime config and inspection.
  """

  use GenServer

  alias Brain.Hippocampus.{Window, Recall, Normalize, Evidence, Config, Telemetry}

  @type slate    :: map()
  @type meta     :: map()
  @type episode  :: %{slate: slate(), meta: meta(), norms: MapSet.t()}
  @type recall_r :: %{score: float(), at: non_neg_integer(), episode: episode()}

  ## ─────────────────────────────── Public API ───────────────────────────────

  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  @doc "Record an episode (slate + meta) into the rolling window. Returns `slate` unchanged."
  @spec encode(slate(), meta()) :: slate()
  def encode(slate, meta \\ %{}) when is_map(slate) and is_map(meta) do
    :ok = GenServer.call(__MODULE__, {:encode, slate, meta})
    slate
  end

  @doc """
  Recall prior episodes relevant to `cues` (list/stringy slate/si).
  Options (per-call override):
    • :limit, :half_life_ms, :min_jaccard, :scope
    • :ignore_head — :auto | :always | :never | false
  """
  @spec recall([String.t()] | map(), keyword()) :: [recall_r()]
  def recall(cues, opts \\ []) when is_list(cues) or is_map(cues),
    do: GenServer.call(__MODULE__, {:recall, cues, Map.new(opts)})

  @doc """
  Attach episodic evidence into `si.evidence[:episodes]`.
  Passes any opts to `recall/2`. Default `ignore_head: false` for attach.
  """
  @spec attach_episodes(map(), keyword()) :: map()
  def attach_episodes(si, opts \\ []) when is_map(si) or is_struct(si) do
    cues  = Keyword.get(opts, :cues, si)
    opts2 = Keyword.put_new(opts, :ignore_head, false)

    episodes =
      __MODULE__.recall(cues, opts2)
      |> Enum.map(&Evidence.ensure_winners_for_evidence/1)

    evidence = Map.get(si, :evidence) || %{}
    Map.put(si, :evidence, Map.put(evidence, :episodes, episodes))
  end

  @doc """
  Configure runtime options. Accepts any of:
    :window_keep (or :keep), :half_life_ms, :recall_limit, :min_jaccard
  """
  @spec configure(keyword()) :: :ok
  def configure(opts) when is_list(opts),
    do: GenServer.call(__MODULE__, {:configure, Map.new(opts)})

  @doc "Reset in-memory state to defaults."
  @spec reset() :: :ok
  def reset, do: GenServer.call(__MODULE__, :reset)

  @doc "Return the full internal state map."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(__MODULE__, :snapshot)

  ## ─────────────────────────────── GenServer ───────────────────────────────

  @impl true
  def init(_state), do: {:ok, default_state()}

  @impl true
  def handle_call({:encode, slate, meta}, from, state) do
    now = System.system_time(:millisecond)

    ep = %{
      slate: slate,
      meta: meta,
      norms:
        slate
        |> Normalize.extract_norms_from_any()
        |> Enum.reject(&Normalize.empty?/1)
        |> MapSet.new()
    }

    window2 = Window.append_or_refresh_head(state.window, ep, state.window_keep)

    Telemetry.emit_write(%{window_size: length(window2)}, %{meta: meta})
    Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :write], %{window_size: length(window2)}, %{meta: meta})

    new_last =
      case window2 do
        [{^now, e} | _] -> {now, e}
        [{at, e} | _] when is_integer(at) -> {at, e}
        _ -> nil
      end

    {:reply, :ok, %{state | window: window2, last: new_last}}
  end

  @impl true
  def handle_call({:recall, cues, opts}, from, state) do
    window       = state.window
    limit        = Map.get(opts, :limit, state.opts.recall_limit)
    half_life_ms = Config.normalize_half_life(Map.get(opts, :half_life_ms, state.opts.half_life_ms))
    min_jacc     = Config.normalize_min_jaccard(Map.get(opts, :min_jaccard, state.opts.min_jaccard))
    scope_opt    = Map.get(opts, :scope, nil)
    ignore_head  = Map.get(opts, :ignore_head, :auto)

    {scored, meas, meta_map} =
      Recall.run(cues, window,
        limit: limit,
        half_life_ms: half_life_ms,
        min_jaccard: min_jacc,
        scope: scope_opt,
        ignore_head: ignore_head
      )

    Telemetry.emit_recall(meas, meta_map)
    Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :recall], meas, meta_map)

    {:reply, scored, state}
  end

  @impl true
  def handle_call({:configure, opts}, _from, state) do
    keep =
      opts
      |> Map.get(:window_keep, Map.get(opts, :keep, state.window_keep))
      |> Config.normalize_keep()

    half_life =
      opts
      |> Map.get(:half_life_ms, state.opts.half_life_ms)
      |> Config.normalize_half_life()

    recall_limit =
      opts
      |> Map.get(:recall_limit, state.opts.recall_limit)
      |> Config.normalize_limit()

    min_jaccard =
      opts
      |> Map.get(:min_jaccard, state.opts.min_jaccard)
      |> Config.normalize_min_jaccard()

    new_opts =
      state.opts
      |> Map.put(:window_keep, keep)
      |> Map.put(:half_life_ms, half_life)
      |> Map.put(:recall_limit, recall_limit)
      |> Map.put(:min_jaccard, min_jaccard)

    {:reply, :ok, %{state | window_keep: keep, opts: new_opts, window: Window.trim(state.window, keep)}}
  end

  @impl true
  def handle_call(:reset, _from, _state), do: {:reply, :ok, default_state()}

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  ## ─────────────────────────────── Helpers ───────────────────────────────

  defp default_state do
    %{
      window_keep: Config.defaults().window_keep,
      window: [],
      last: nil,
      opts: %{
        window_keep:   Config.defaults().window_keep,
        half_life_ms:  Config.defaults().half_life_ms,
        recall_limit:  Config.defaults().recall_limit,
        min_jaccard:   Config.defaults().min_jaccard
      }
    }
  end
end

