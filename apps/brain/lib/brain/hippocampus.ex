# apps/brain/lib/brain/hippocampus.ex
defmodule Brain.Hippocampus do
  @moduledoc """
  Hippocampus — in-memory episodic window with lightweight recall + DB persistence.

  Window shape:
    window :: [{ts_ms, episode}]
    episode :: %{slate: map(), meta: map(), norms: MapSet.t()}

  Key behavior:
    • encode/2 appends an episode into the rolling window and persists (best-effort)
    • recall/2 ranks memory (and optionally DB/hybrid via Brain.Hippocampus.DB)
    • attach_episodes/2 writes recall results into si.evidence[:episodes]

  Important:
    • Warm start rehydrates the in-memory window from Db.Episode.recent/1
    • Opts merging is tolerant: map or keyword are accepted; normalized to map
  """

  use GenServer
  require Logger

  alias Brain.Hippocampus.{Window, Recall, Normalize, Evidence, Config, Telemetry, DB, Writer}

  @type slate :: map()
  @type meta :: map()
  @type episode :: %{
          required(:slate) => slate(),
          required(:meta) => meta(),
          required(:norms) => MapSet.t()
        }
  @type window :: [{non_neg_integer(), episode()}]

  @typedoc """
  Ranked recall result.

  * `:score` is the backend-specific relevance score.
  * `:at` is the episode timestamp in milliseconds.
  * `:episode` contains the remembered `%{slate, meta, norms}` payload.
  """
  @type recall_result :: %{
          required(:score) => float(),
          required(:at) => non_neg_integer(),
          required(:episode) => episode()
        }

  @boot_warm_limit 200

  # ────────────────────────────────────────────────────────────────────────────
  # Public API
  # ────────────────────────────────────────────────────────────────────────────

  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  @spec encode(map(), map()) :: map()
  def encode(%{} = slate, meta \\ %{}) when is_map(meta) do
    :ok = GenServer.call(__MODULE__, {:encode, slate, meta})
    slate
  end

  @spec fact(atom()) :: term() | nil
  def fact(key) when is_atom(key),
    do: GenServer.call(__MODULE__, {:fact, key})

  @doc """
  Recall ranked episodes for cues from the configured source.

  Sources:

    * `:memory` - in-memory symbolic window scored by Jaccard overlap and recency.
    * `:db` - pgvector-backed DB recall via `Db.Episode`; requires `:embedding`.
    * `:hybrid` - memory recall plus DB recall when `:embedding` is supplied.

  Returns a list of `%{score, at, episode}` maps.
  """
  @spec recall(list() | map(), keyword()) :: [recall_result()]
  def recall(cues, opts \\ []) when is_list(cues) or is_map(cues),
    do: GenServer.call(__MODULE__, {:recall, cues, normalize_opts(opts)})

  @doc """
  Recall episodes and attach them under `si.evidence[:episodes]`.

  Attached episodes keep the same `%{score, at, episode}` recall result shape, with
  `episode.slate.winners` normalized for downstream inspection when possible.
  """
  @spec attach_episodes(map(), keyword()) :: map()
  def attach_episodes(si, opts \\ []) when is_map(si) or is_struct(si) do
    GenServer.call(__MODULE__, {:attach, si, normalize_opts(opts)})
  end

  @spec configure(keyword()) :: :ok
  def configure(opts) when is_list(opts),
    do: GenServer.call(__MODULE__, {:configure, normalize_opts(opts)})

  @spec reset() :: :ok
  def reset, do: GenServer.call(__MODULE__, :reset)

  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(__MODULE__, :snapshot)

  @spec status() :: map()
  def status, do: GenServer.call(__MODULE__, :status)

  # ────────────────────────────────────────────────────────────────────────────
  # GenServer
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def init(_state) do
    state = default_state()

    state =
      case warm_start_from_db(state) do
        {:ok, s} -> s
        _ -> state
      end

    {:ok, state}
  end

  @impl true
  def handle_call({:encode, slate, meta_in}, from, state) do
    at = System.system_time(:millisecond)

    norms =
      slate
      |> Normalize.extract_norms_from_any()
      |> Enum.reject(&Normalize.empty?/1)
      |> MapSet.new()

    meta = enrich_meta_with_emotion(meta_in)
    ep = %{slate: slate, meta: meta, norms: norms}

    window1 = Window.append_or_refresh_head(state.window, ep, state.window_keep)

    state1 =
      state
      |> Map.put(:window, window1)
      |> Map.put(:last, ep)
      |> Map.put(:last_at, at)

    maybe_persist(ep, state1)

    write_meas = %{
      count: 1,
      window_size: length(window1),
      norms_count: MapSet.size(norms)
    }

    write_meta = %{
      at: at,
      window_keep: state.window_keep,
      meta: meta
    }

    Telemetry.emit_write(write_meas, write_meta)
    Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :write], write_meas, write_meta)

    {:reply, :ok, state1}
  end

  @impl true
  def handle_call({:recall, cues, opts}, from, state) do
    {results, meas, meta} = do_recall(cues, state, opts)

    Telemetry.emit_recall(meas, meta)

    Telemetry.maybe_echo_to_caller(
      from,
      [:brain, :hippocampus, :recall],
      meas,
      meta
    )

    {:reply, results, state}
  end

  @impl true
  def handle_call({:attach, si, opts}, _from, state) do
    # For attach we usually want to include head as well unless caller overrides.
    opts = Map.put_new(opts, :ignore_head, false)

    {results, _meas, _meta} = do_recall(si, state, opts)

    episodes =
      results
      |> Enum.map(&Evidence.ensure_winners_for_evidence/1)

    evidence = Map.get(si, :evidence, %{}) || %{}
    si2 = Map.put(si, :evidence, Map.put(evidence, :episodes, episodes))

    {:reply, si2, state}
  end

  @impl true
  def handle_call({:configure, opts}, _from, state) do
    state1 = Map.update!(state, :opts, &Config.merge_opts(&1, opts))
    {:reply, :ok, state1}
  end

  @impl true
  def handle_call(:reset, _from, _state), do: {:reply, :ok, default_state()}

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call(:status, _from, state) do
    pid = Process.whereis(__MODULE__)

    {:reply,
     %{
       pid: pid,
       running?: is_pid(pid),
       window_keep: state.window_keep,
       window_len: length(state.window),
       last_at: state.last_at,
       opts: state.opts
     }, state}
  end

  @impl true
  def handle_call({:fact, key}, _from, state) do
    {:reply, find_fact_in_window(state.window, key), state}
  end

  defp find_fact_in_window(window, key) when is_list(window) do
    # window is newest-first; return first match
    Enum.find_value(window, fn {_at, ep} ->
      if fact_episode?(ep, key), do: fact_value(ep), else: nil
    end)
  end

  defp find_fact_in_window(_window, _key), do: nil

  defp fact_episode?(%{meta: meta, slate: slate}, :user_name) do
    tags = List.wrap(meta[:tags] || meta["tags"] || slate[:tags] || slate["tags"] || [])

    Enum.any?(tags, fn t ->
      s = if is_atom(t), do: Atom.to_string(t), else: to_string(t)
      String.downcase(s) == "user_name"
    end)
  end

  defp fact_episode?(_ep, _key), do: false

  defp fact_value(%{meta: meta} = ep) do
    meta[:value] || meta["value"] || fact_value_from_si(ep)
  end

  defp fact_value(_), do: nil

  defp fact_value_from_si(%{slate: slate}) do
    si = slate[:si] || slate["si"] || %{}

    get_in(si, [:episode, :meta, :value]) ||
      get_in(si, ["episode", "meta", "value"]) ||
      get_in(si, ["episode", "meta", :value]) ||
      get_in(si, [:episode, "meta", "value"])
  end

  defp fact_value_from_si(_), do: nil

  # ────────────────────────────────────────────────────────────────────────────
  # Warm start: rehydrate window from DB
  # ────────────────────────────────────────────────────────────────────────────

  defp warm_start_from_db(state) do
    if Code.ensure_loaded?(Db.Episode) and function_exported?(Db.Episode, :recent, 1) do
      try do
        rows = Db.Episode.recent(@boot_warm_limit)
        keep = Map.get(state, :window_keep, 300)
        window0 = Map.get(state, :window, [])

        window1 =
          rows
          |> Enum.reverse()
          |> Enum.reduce(window0, fn row, win ->
            ep = db_row_to_episode(row)
            Window.append_or_refresh_head(win, ep, keep)
          end)

        Logger.warning("[Hippocampus] warm_start loaded=#{length(rows)} keep=#{keep}")
        {:ok, Map.put(state, :window, window1)}
      rescue
        e ->
          Logger.warning("[Hippocampus] warm_start failed: #{Exception.message(e)}")
          {:error, :warm_start_failed}
      catch
        :exit, reason ->
          Logger.warning("[Hippocampus] warm_start exit: #{inspect(reason)}")
          {:error, :warm_start_exit}
      end
    else
      {:error, :no_db_recent}
    end
  end

  defp db_row_to_episode(row) do
    tokens = List.wrap(Map.get(row, :tokens) || [])

    norms =
      tokens
      |> Enum.reject(&Normalize.empty?/1)
      |> MapSet.new()

    si = Map.get(row, :si) || %{}
    tags = Map.get(row, :tags) || []

    meta_value =
      get_in(si, ["episode", "meta", "value"]) ||
        get_in(si, [:episode, :meta, :value]) ||
        get_in(si, ["episode", "meta", :value]) ||
        get_in(si, [:episode, "meta", "value"])

    %{
      slate: %{
        tags: tags,
        si: si,
        tokens: tokens
      },
      meta: %{
        source: :db,
        episode_id: Map.get(row, :id),
        inserted_at: Map.get(row, :inserted_at),
        tags: tags,
        value: meta_value
      },
      norms: norms
    }
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Recall
  # ────────────────────────────────────────────────────────────────────────────

  defp do_recall(cues0, state, opts) do
    cue_input = cues0
    cues = cue_map(cues0)

    source = Map.get(opts, :source) || Map.get(opts, :recall_source) || :memory
    limit = Map.get(opts, :limit) || Map.get(opts, :recall_limit) || state.opts.recall_limit

    ignore_head =
      case {Map.get(opts, :ignore_head, :auto), cue_input} do
        {:auto, cues} when is_list(cues) -> :never
        {v, _} -> v
      end

    min_jaccard = Map.get(opts, :min_jaccard, state.opts.min_jaccard)
    half_life_ms = Map.get(opts, :half_life_ms, state.opts.half_life_ms)
    scope = Map.get(opts, :scope)

    base_meta = %{
      source: source,
      limit: limit,
      ignore_head: ignore_head,
      min_jaccard: min_jaccard,
      half_life_ms: half_life_ms,
      scope: scope,
      window_len: length(state.window)
    }

    case normalize_source(source) do
      :db ->
        results =
          DB.recall(
            cues: cues,
            embedding: Map.get(opts, :embedding),
            limit: limit,
            half_life_ms: half_life_ms,
            min_jaccard: min_jaccard,
            scope: scope,
            window: state.window
          )

        meas = %{
          cue_count: length(Normalize.extract_norms_from_any(cues)),
          window_size: length(state.window),
          returned: length(results),
          top_score:
            case results do
              [%{score: s} | _] -> s
              _ -> 0.0
            end
        }

        {results, meas, base_meta}

      :hybrid ->
        {memory_results, memory_meas, memory_meta} =
          Recall.run(cues, state.window,
            limit: limit,
            half_life_ms: half_life_ms,
            min_jaccard: min_jaccard,
            scope: scope,
            ignore_head: ignore_head
          )

        db_results =
          case Map.get(opts, :embedding) do
            nil ->
              []

            embedding ->
              DB.recall(
                cues: cues,
                embedding: embedding,
                limit: limit,
                half_life_ms: half_life_ms,
                min_jaccard: min_jaccard,
                scope: scope,
                window: state.window,
                hybrid?: true
              )
          end

        results =
          (memory_results ++ db_results)
          |> Enum.sort_by(fn r -> {Map.get(r, :score, 0.0), Map.get(r, :at, 0)} end, :desc)
          |> Enum.take(limit)

        meas = %{
          cue_count:
            Map.get(memory_meas, :cue_count, length(Normalize.extract_norms_from_any(cues))),
          window_size: length(state.window),
          returned: length(results),
          memory_returned: length(memory_results),
          db_returned: length(db_results),
          top_score:
            case results do
              [%{score: s} | _] -> s
              _ -> 0.0
            end
        }

        meta = Map.merge(base_meta, Map.put(memory_meta, :hybrid_db?, db_results != []))
        {results, meas, meta}

      _ ->
        {results, meas, dbg_meta} =
          Recall.run(cues, state.window,
            limit: limit,
            half_life_ms: half_life_ms,
            min_jaccard: min_jaccard,
            scope: scope,
            ignore_head: ignore_head
          )

        {results, meas, Map.merge(base_meta, dbg_meta)}
    end
  end

  defp cue_map(%_{} = struct), do: Map.from_struct(struct)
  defp cue_map(%{} = map), do: map
  defp cue_map(other), do: other

  defp normalize_source(src) when is_atom(src), do: src

  defp normalize_source(src) when is_binary(src) do
    case String.downcase(String.trim(src)) do
      "memory" -> :memory
      "db" -> :db
      "hybrid" -> :hybrid
      _ -> :memory
    end
  end

  defp normalize_source(_), do: :memory

  # ────────────────────────────────────────────────────────────────────────────
  # Persistence boundary
  # ────────────────────────────────────────────────────────────────────────────

  defp maybe_persist(ep, state) do
    # state.opts is a MAP; Writer expects keyword (and now normalizes anyway, but keep it clean)
    kw_opts = Enum.into(state.opts || %{}, [])

    # Force persist for explicit facts if Writer is configured to gate persistence.
    kw_opts =
      if is_map(ep.meta) and (ep.meta[:kind] == :fact or "fact" in List.wrap(ep.meta[:tags])) do
        Keyword.put(kw_opts, :persist, true)
      else
        kw_opts
      end

    _ = Writer.maybe_persist(ep, kw_opts)
    :ok
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Meta enrichment (emotion header) — safe
  # ────────────────────────────────────────────────────────────────────────────

  defp enrich_meta_with_emotion(meta) when is_map(meta) do
    has_emotion? =
      case meta[:emotion] || meta["emotion"] do
        %{} -> true
        _ -> false
      end

    if has_emotion? do
      meta
    else
      snap =
        if Code.ensure_loaded?(Brain.MoodCore) and
             function_exported?(Brain.MoodCore, :snapshot, 0) do
          try do
            Brain.MoodCore.snapshot()
          rescue
            _ -> %{}
          catch
            :exit, _ -> %{}
          end
        else
          %{}
        end

      levels = Map.get(snap, :levels) || %{}
      indices = Map.get(snap, :mood) || Map.get(snap, :mood_indices) || %{}
      latents = Map.get(snap, :latents) || %{}
      tone_hint = Map.get(snap, :tone_hint) || Map.get(snap, :tone) || :neutral

      tone_from_meta = meta[:tone_reaction] || meta["tone_reaction"] || tone_hint || :neutral

      header = %{
        tone_reaction: tone_from_meta,
        latents: latents,
        mood_levels: levels,
        mood_indices: indices
      }

      meta
      |> Map.put(:emotion, header)
      |> Map.put(:tone_reaction, header.tone_reaction)
      |> Map.put(:latents, header.latents)
    end
  end

  defp enrich_meta_with_emotion(other), do: other

  # ────────────────────────────────────────────────────────────────────────────
  # Defaults / opts normalization
  # ────────────────────────────────────────────────────────────────────────────

  defp default_state do
    defaults = Config.defaults()

    %{
      window_keep: defaults.window_keep,
      window: [],
      last: nil,
      last_at: nil,
      opts: %{
        window_keep: defaults.window_keep,
        half_life_ms: defaults.half_life_ms,
        recall_limit: defaults.recall_limit,
        min_jaccard: defaults.min_jaccard,
        recall_source: defaults.recall_source
      }
    }
  end

  defp normalize_opts(opts) when is_list(opts), do: Map.new(opts)
  defp normalize_opts(%{} = opts), do: opts
  defp normalize_opts(_), do: %{}
end
