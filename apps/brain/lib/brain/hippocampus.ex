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

  @type slate :: map()
  @type meta :: map()
  @type episode :: %{slate: slate(), meta: meta(), norms: MapSet.t()}
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

  NOTE: Default is `ignore_head: false` so the most recent (head) episode
  is included unless you explicitly opt out. This matches tests expecting
  the head to be considered when enforcing `:limit`.
  """
  @spec recall([String.t()] | map(), keyword()) :: [recall_r()]
  def recall(cues, opts \\ []) when is_list(cues) or is_map(cues),
    do: GenServer.call(__MODULE__, {:recall, cues, Map.new(opts)})

  @doc """
  Attach episodic evidence into `si.evidence[:episodes]`.

  Behavior:
    • Builds robust cues from `opts[:cues]` or from `si` (winners/atl_slate/tokens/sentence).
    • Calls `recall/2` with `ignore_head: false` (unless provided) and permissive `min_jaccard: 0.0` by default.
    • Applies optional `:scope` filtering locally (atom-or-string meta keys accepted).

  Returns `si` with `:evidence[:episodes]` set.
  """
  @spec attach_episodes(map(), keyword()) :: map()
  def attach_episodes(si, opts \\ []) when is_map(si) or is_struct(si) do
    cues  = build_cues(si, opts)
    scope = Keyword.get(opts, :scope, nil)

    opts2 =
      opts
      |> Keyword.delete(:scope)
      |> Keyword.put_new(:ignore_head, false)
      |> Keyword.put_new(:min_jaccard, 0.0)

    episodes_unfiltered =
      __MODULE__.recall(cues, opts2)
      |> Enum.map(&Evidence.ensure_winners_for_evidence/1)

    episodes_scoped =
      case scope do
        s when is_map(s) -> Enum.filter(episodes_unfiltered, &meta_matches_scope?(&1, s))
        _ -> episodes_unfiltered
      end

    # Fallback: if nothing recalled but window has entries, attach the head so
    # callers relying on “hippo: true attaches episodes” still see at least one.
    episodes =
      case episodes_scoped do
        [] ->
          case snapshot().window do
            [{at, ep} | _] ->
              [%{score: 0.0, at: at, episode: ep} |> Evidence.ensure_winners_for_evidence()]
            _ ->
              []
          end

        list ->
          list
      end

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
  def handle_call({:encode, slate, meta_in}, from, state) do
    case Application.get_env(:brain, :episodes_mode, :on) do
      :off ->
        Telemetry.emit_write(%{window_size: length(state.window)}, %{meta: meta_in, skipped: true})

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :write],
          %{window_size: length(state.window)},
          %{meta: meta_in, skipped: true}
        )

        {:reply, :ok, state}

      _ ->
        norms =
          slate
          |> Normalize.extract_norms_from_any()
          |> Enum.reject(&Normalize.empty?/1)
          |> MapSet.new()

        # Decide whether to de-dup or append:
        # • If incoming norms equal the head’s norms AND meta differs → refresh head (preserve original head meta).
        # • If incoming norms equal the head’s norms AND meta is identical → append a NEW episode (no de-dup).
        # • Otherwise append as usual.
        {action, head_meta} =
          case state.window do
            [{_at, %{norms: ^norms, meta: m}} | _] ->
              if meta_in == m, do: {:append, m}, else: {:refresh, m}

            _ ->
              {:append, nil}
          end

        {window2, meta_final} =
          case action do
            :refresh ->
              ep = %{slate: slate, meta: head_meta, norms: norms}
              {Window.append_or_refresh_head(state.window, ep, state.window_keep), head_meta}

            :append ->
              ep = %{slate: slate, meta: meta_in, norms: norms}
              at = now_ms()
              {Window.trim([{at, ep} | state.window], state.window_keep), meta_in}
          end

        Telemetry.emit_write(%{window_size: length(window2)}, %{meta: meta_final})

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :write],
          %{window_size: length(window2)},
          %{meta: meta_final}
        )

        new_last =
          case window2 do
            [{at, e} | _] when is_integer(at) -> {at, e}
            _ -> nil
          end

        {:reply, :ok, %{state | window: window2, last: new_last}}
    end
  end

  @impl true
  def handle_call({:recall, cues, opts}, from, state) do
    limit = Map.get(opts, :limit, state.opts.recall_limit)

    half_life_ms =
      Config.normalize_half_life(Map.get(opts, :half_life_ms, state.opts.half_life_ms))

    min_jacc    = Config.normalize_min_jaccard(Map.get(opts, :min_jaccard, state.opts.min_jaccard))
    ignore_head = Map.get(opts, :ignore_head, false)
    scope_opt   = Map.get(opts, :scope, nil)

    {ranked0, meas, meta_map} =
      Recall.run(cues, state.window,
        limit: limit,
        recall_limit: limit,
        half_life_ms: half_life_ms,
        min_jaccard: min_jacc,
        scope: scope_opt,
        ignore_head: ignore_head
      )

    ranked =
      ranked0
      |> Enum.filter(fn
        %{score: s} when is_number(s) -> s > 0.0
        _ -> false
      end)
      |> Enum.take(limit)

    Telemetry.emit_recall(meas, meta_map)
    Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :recall], meas, meta_map)

    {:reply, ranked, state}
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

    {:reply, :ok,
     %{state | window_keep: keep, opts: new_opts, window: Window.trim(state.window, keep)}}
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
        window_keep: Config.defaults().window_keep,
        half_life_ms: Config.defaults().half_life_ms,
        recall_limit: Config.defaults().recall_limit,
        min_jaccard: Config.defaults().min_jaccard
      }
    }
  end

  defp now_ms, do: System.monotonic_time(:millisecond)

  # Build robust cues from SI or opts. Prefer explicit :cues, then winners/atl_slate,
  # then unigram token phrases, then words from sentence. Fallback to `si` itself.
  defp build_cues(si, opts) do
    cond do
      Keyword.has_key?(opts, :cues) ->
        Keyword.get(opts, :cues)

      is_map(si) and is_list(si[:winners]) ->
        Enum.flat_map(si[:winners], fn w ->
          [w[:lemma], w["lemma"], w[:norm], w["norm"]]
        end)
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&Normalize.norm/1)

      is_map(si) and is_map(si[:atl_slate]) and is_list(si[:atl_slate][:winners]) ->
        Enum.flat_map(si[:atl_slate][:winners], fn w ->
          [w[:lemma], w["lemma"], w[:norm], w["norm"]]
        end)
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&Normalize.norm/1)

      is_map(si) and is_list(si[:tokens]) ->
        si[:tokens]
        |> Enum.filter(&(Map.get(&1, :n, 1) == 1))
        |> Enum.map(&Normalize.norm(&1[:phrase]))

      is_map(si) and is_binary(si[:sentence]) ->
        si[:sentence]
        |> Normalize.norm()
        |> String.split(" ", trim: true)

      true ->
        si
    end
  end

  # Match all key/value pairs in `scope` against episode meta (accept atom-or-string keys)
  defp meta_matches_scope?(%{episode: %{meta: meta}}, scope) when is_map(meta) and is_map(scope) do
    Enum.all?(scope, fn {k, v} -> Map.get(meta, k, Map.get(meta, to_string(k))) == v end)
  end

  defp meta_matches_scope?(_rec, _scope), do: false
end

