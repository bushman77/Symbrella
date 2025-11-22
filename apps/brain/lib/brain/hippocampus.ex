defmodule Brain.Hippocampus do
  @moduledoc """
  Hippocampus — in-memory episodic window with lightweight recall + telemetry.

  Public API:
    • encode/2 — record an episode and return the `slate` unchanged.
    • recall/2 — rank prior episodes with optional source:
        :memory (default), :db (pgvector), or :hybrid (merge).
    • attach_episodes/2 — write recall results to `si.evidence[:episodes]`.
    • configure/1, reset/0, snapshot/0 — runtime config and inspection.

  Notes
  -----
  • DB recall can use an `:embedding` in opts (list(float) or %Pgvector{}).
    If missing and :source is :db or :hybrid, we fall back to memory-only recall.
  • Recall `score` is **familiarity**. We optionally apply an **outcome uplift**
    (bounded) from `episode.meta[:outcome_score]`.

  Emotional integration
  ---------------------
  • On encode/2 we enrich `meta` with an emotional header when possible:

        %{
          emotion: %{
            tone_reaction: :warm | :cool | :deescalate | :neutral,
            latents:       %{threat: f, safety: f, reward: f, control: f},
            mood_levels:   %{da: f, "5ht": f, glu: f, ne: f},
            mood_indices:  %{exploration: f, inhibition: f, vigilance: f, plasticity: f}
          },
          tone_reaction: :warm | :cool | :deescalate | :neutral,
          latents:       %{...}
        }

    This is derived from `Brain.MoodCore.snapshot/0` plus any `:tone_reaction`
    or `:reaction` you pass in the meta.

  • On recall/2 we expose per-episode emotional priors on each result:

        %{..., emotion_priors: %{threat_prior: f, safety_prior: f, ...}}

    These can be aggregated by the amygdala/mood path to bias current reactions.
  """

  use GenServer

  alias Brain.Hippocampus.{Window, Recall, Normalize, Evidence, Config, Telemetry}
  alias Brain.Hippocampus.DB

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

  @doc "Summarized status for dashboards (pid/queue + full state)."
  @spec status() :: map()
  def status, do: GenServer.call(__MODULE__, :status)

  @doc """
  Recall prior episodes relevant to `cues` (list/stringy slate/si).

  Options (per-call override):
    • :limit, :half_life_ms, :min_jaccard, :scope
    • :ignore_head — :auto | :always | :never | false
    • :source — :memory | :db | :hybrid (defaults to configured `recall_source`)
    • :embedding — required if :source is :db or :hybrid (query vector)
  """
  @spec recall([String.t()] | map(), keyword()) :: [recall_r()]
  def recall(cues, opts \\ []) when is_list(cues) or is_map(cues),
    do: GenServer.call(__MODULE__, {:recall, cues, Map.new(opts)})

  @doc """
  Attach episodic evidence into `si.evidence[:episodes]`.

  Behavior:
    • Builds cues from `opts[:cues]` or from `si` (winners/atl_slate/tokens/sentence).
    • Calls `recall/2` with `ignore_head: false` unless provided.
    • Applies optional `:scope` filter locally (atom-or-string keys accepted).

  Fallback logic:
    • If recall returns episodes → attach them (plus scope filtering).
    • If recall returns no episodes:
        – With relaxed Jaccard (effective min_jaccard == 0.0) and a non-empty window:
            · Try to pick the **first window episode whose norms intersect the cues**,
              **and** whose meta matches `scope` (if provided).
            · If none match both scope and cues, attach nothing.
        – With effective `min_jaccard` > 0.0 → respect the empty result (attach none).
    • If the server is NOT running, synthesize a lightweight episode from cues.
  """
  @spec attach_episodes(map(), keyword()) :: map()
  def attach_episodes(si, opts \\ []) when is_map(si) or is_struct(si) do
    cues  = build_cues(si, opts)
    scope = Keyword.get(opts, :scope, nil)

    # Do not pass :scope into recall; we handle it locally.
    opts2 =
      opts
      |> Keyword.delete(:scope)
      |> Keyword.put_new(:ignore_head, false)

    episodes_unfiltered =
      if running?() do
        recall(cues, opts2)
        |> Enum.map(&Evidence.ensure_winners_for_evidence/1)
        |> Enum.map(&standardize_for_evidence/1)
      else
        [synthesize_episode_for_evidence(si, cues)]
      end

    episodes_scoped =
      case scope do
        s when is_map(s) and s != %{} ->
          Enum.filter(episodes_unfiltered, &meta_matches_scope?(&1, s))

        _ ->
          episodes_unfiltered
      end

    # For fallback decisions, look at:
    #   • per-call :min_jaccard override (if present)
    #   • else configured Hippo default (if running)
    effective_min_j =
      cond do
        Keyword.has_key?(opts, :min_jaccard) ->
          Keyword.get(opts, :min_jaccard)

        running?() ->
          case snapshot() do
            %{opts: %{min_jaccard: v}} when is_number(v) -> v
            _ -> 0.0
          end

        true ->
          0.0
      end

    episodes =
      cond do
        # We got scoped episodes → use them as-is.
        episodes_scoped != [] ->
          episodes_scoped

        # Server not running: just return whatever we synthesized.
        not running?() ->
          episodes_unfiltered

        # Strict min_jaccard: if recall couldn't find anything, don't override it.
        effective_min_j > 0.0 ->
          []

        # Relaxed Jaccard: allow a "best effort" fallback from the window,
        # but only if the episode:
        #   • matches scope (if provided), AND
        #   • shares at least one norm with the cues.
        true ->
          case snapshot().window do
            [] ->
              []

            window ->
              # Filter window by scope first, if any.
              window_scoped =
                case scope do
                  s when is_map(s) and s != %{} ->
                    Enum.filter(window, fn {_at, ep} ->
                      meta_matches_scope?(%{episode: ep}, s)
                    end)

                  _ ->
                    window
                end

              if window_scoped == [] do
                []
              else
                # Build a norms set from cues (similar to synth logic).
                cue_norms =
                  cues
                  |> List.wrap()
                  |> Enum.flat_map(fn
                    s when is_binary(s) ->
                      String.split(safe_norm(s), ~r/\s+/, trim: true)

                    m when is_map(m) ->
                      candidate =
                        Map.get(m, :lemma) || Map.get(m, "lemma") ||
                          Map.get(m, :norm) || Map.get(m, "norm") ||
                          Map.get(m, :word) || Map.get(m, "word")

                      if is_binary(candidate), do: [safe_norm(candidate)], else: []

                    _ ->
                      []
                  end)
                  |> Enum.reject(&(&1 == ""))
                  |> MapSet.new()

                if MapSet.size(cue_norms) == 0 do
                  []
                else
                  window_scoped
                  |> Enum.reduce_while(nil, fn {at, ep}, acc ->
                    norms = Map.get(ep, :norms, MapSet.new())
                    inter = MapSet.intersection(norms, cue_norms)

                    if MapSet.size(inter) > 0 do
                      {:halt, {at, ep}}
                    else
                      {:cont, acc}
                    end
                  end)
                  |> case do
                    nil ->
                      []

                    {at, ep} ->
                      [
                        %{score: 0.0, at: at, episode: ep}
                        |> Evidence.ensure_winners_for_evidence()
                        |> standardize_for_evidence()
                      ]
                  end
                end
              end
          end
      end

    evidence = Map.get(si, :evidence) || %{}
    Map.put(si, :evidence, Map.put(evidence, :episodes, episodes))
  end

  @doc """
  Configure runtime options. Accepts any of:
    :window_keep (or :keep), :half_life_ms, :recall_limit, :min_jaccard, :recall_source
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
    norms =
      slate
      |> Normalize.extract_norms_from_any()
      |> Enum.reject(&Normalize.empty?/1)
      |> MapSet.new()

    at   = now_ms()
    meta = enrich_meta_with_emotion(meta_in)

    ep = %{slate: slate, meta: meta, norms: norms}

    # 🔹 dedup-on-write:
    #   If the *head* episode has the same norms, refresh its timestamp
    #   (and slate/norms) but keep the original meta.
    window1 =
      case state.window do
        [{_old_at, %{norms: old_norms} = old_ep} | rest] ->
          if MapSet.equal?(old_norms, norms) do
            # keep old_ep.meta; only update slate + norms
            new_ep = %{old_ep | slate: slate, norms: norms}
            [{at, new_ep} | rest]
          else
            [{at, ep} | state.window]
          end

        _ ->
          [{at, ep} | state.window]
      end

    window2 = Window.trim(window1, state.window_keep)

    {new_last, last_at} =
      case window2 do
        [{at2, e} | _] when is_integer(at2) -> {{at2, e}, at2}
        _ -> {nil, nil}
      end

    case Application.get_env(:brain, :episodes_mode, :on) do
      :off ->
        # Still update the in-memory window, but mark as skipped in telemetry
        Telemetry.emit_write(%{window_size: length(window2)}, %{meta: meta, skipped: true})

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :write],
          %{window_size: length(window2)},
          %{meta: meta, skipped: true}
        )

        {:reply, :ok, %{state | window: window2, last: new_last, last_at: last_at}}

      _ ->
        Telemetry.emit_write(%{window_size: length(window2)}, %{meta: meta})

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :write],
          %{window_size: length(window2)},
          %{meta: meta}
        )

        # Optional persistence (guarded) — use the *original* episode we built
        maybe_persist(at, ep)

        {:reply, :ok, %{state | window: window2, last: new_last, last_at: last_at}}
    end
  end

  @impl true
  def handle_call(:status, _from, state) do
    qlen =
      case :erlang.process_info(self(), :message_queue_len) do
        {:message_queue_len, n} when is_integer(n) -> n
        _ -> 0
      end

    reply = %{
      process: __MODULE__,
      pid: self(),
      queue: qlen,
      current: :idle,
      state: state
    }

    {:reply, reply, state}
  end

  @impl true
  def handle_call({:recall, cues, opts}, from, state) do
    limit        = Map.get(opts, :limit, state.opts.recall_limit)

    half_life_ms =
      opts
      |> Map.get(:half_life_ms, state.opts.half_life_ms)
      |> Config.normalize_half_life()

    min_jacc =
      opts
      |> Map.get(:min_jaccard, state.opts.min_jaccard)
      |> Config.normalize_min_jaccard()

    ignore_head = Map.get(opts, :ignore_head, false)
    scope_opt   = Map.get(opts, :scope, nil)

    source =
      opts
      |> Map.get(:source, state.opts.recall_source)
      |> Config.normalize_source()

    outcome_w = outcome_weight()

    case source do
      :memory ->
        {ranked0, meas, meta_map} =
          Recall.run(cues, state.window,
            limit: limit,
            recall_limit: limit,
            half_life_ms: half_life_ms,
            min_jaccard: min_jacc,
            ignore_head: ignore_head
          )

        ranked1 =
          case scope_opt do
            s when is_map(s) and s != %{} ->
              Enum.filter(ranked0, &meta_matches_scope?(&1, s))

            _ ->
              ranked0
          end

        ranked =
          ranked1
          |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
          |> Enum.sort_by(& &1.score, :desc)
          |> Enum.take(limit)

        meas2 =
          meas
          |> maybe_set_returned(length(ranked1))
          |> Map.merge(%{source: :memory})

        Telemetry.emit_recall(meas2, meta_map)

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :recall],
          meas2,
          meta_map
        )

        {:reply, ranked, state}

      :db ->
        embedding = Map.get(opts, :embedding)

        if is_nil(embedding) do
          # Fallback to memory if embedding is not provided
          {ranked0, meas, meta_map} =
            Recall.run(cues, state.window,
              limit: limit,
              recall_limit: limit,
              half_life_ms: half_life_ms,
              min_jaccard: min_jacc,
              ignore_head: ignore_head
            )

          ranked1 =
            case scope_opt do
              s when is_map(s) and s != %{} ->
                Enum.filter(ranked0, &meta_matches_scope?(&1, s))

              _ ->
                ranked0
            end

          ranked =
            ranked1
            |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
            |> Enum.sort_by(& &1.score, :desc)
            |> Enum.take(limit)

          meas2 =
            meas
            |> maybe_set_returned(length(ranked1))
            |> Map.merge(%{source: :memory, fallback: :missing_embedding})

          Telemetry.emit_recall(meas2, meta_map)

          Telemetry.maybe_echo_to_caller(
            from,
            [:brain, :hippocampus, :recall],
            meas2,
            meta_map
          )

          {:reply, ranked, state}
        else
          db_opts =
            opts
            |> Map.put_new(:k, limit)
            |> Map.put_new(:tau_s, div(half_life_ms, 1000))
            |> Map.put_new(:min_sim, 0.35)
            |> Map.put_new(:cues, cues)

          db_ranked0 =
            db_opts
            |> Map.to_list()
            |> DB.recall()

          db_ranked1 =
            case scope_opt do
              s when is_map(s) and s != %{} ->
                Enum.filter(db_ranked0, &meta_matches_scope?(&1, s))

              _ ->
                db_ranked0
            end

          ranked =
            db_ranked1
            |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
            |> Enum.sort_by(& &1.score, :desc)
            |> Enum.take(limit)

          meas = %{k: length(db_ranked1), source: :db}

          Telemetry.emit_recall(meas, %{source: :db})

          Telemetry.maybe_echo_to_caller(
            from,
            [:brain, :hippocampus, :recall],
            meas,
            %{source: :db}
          )

          {:reply, ranked, state}
        end

      :hybrid ->
        # Memory path
        {mem_ranked0, _meas_mem, _meta_mem} =
          Recall.run(cues, state.window,
            limit: limit,
            recall_limit: limit,
            half_life_ms: half_life_ms,
            min_jaccard: min_jacc,
            ignore_head: ignore_head
          )

        mem_ranked1 =
          case scope_opt do
            s when is_map(s) and s != %{} ->
              Enum.filter(mem_ranked0, &meta_matches_scope?(&1, s))

            _ ->
              mem_ranked0
          end

        mem_ranked =
          mem_ranked1
          |> Enum.map(&apply_outcome_uplift(&1, outcome_w))

        # DB path (only if embedding provided)
        embedding = Map.get(opts, :embedding)

        {db_ranked1, db_ranked} =
          if is_nil(embedding) do
            {[], []}
          else
            db_opts =
              opts
              |> Map.put_new(:k, limit)
              |> Map.put_new(:tau_s, div(half_life_ms, 1000))
              |> Map.put_new(:min_sim, 0.35)
              |> Map.put_new(:cues, cues)

            db_ranked0 =
              db_opts
              |> Map.to_list()
              |> DB.recall()

            db_ranked1 =
              case scope_opt do
                s when is_map(s) and s != %{} ->
                  Enum.filter(db_ranked0, &meta_matches_scope?(&1, s))

                _ ->
                  db_ranked0
              end

            db_ranked =
              db_ranked1
              |> Enum.map(&apply_outcome_uplift(&1, outcome_w))

            {db_ranked1, db_ranked}
          end

        ranked =
          (mem_ranked ++ db_ranked)
          |> Enum.sort_by(& &1.score, :desc)
          |> Enum.take(limit)

        meas = %{
          mem_k: length(mem_ranked1),
          db_k: length(db_ranked1),
          source: :hybrid,
          had_embedding?: not is_nil(embedding)
        }

        Telemetry.emit_recall(meas, %{source: :hybrid})

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :recall],
          meas,
          %{source: :hybrid}
        )

        {:reply, ranked, state}
    end
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

    recall_source =
      opts
      |> Map.get(:recall_source, state.opts.recall_source)
      |> Config.normalize_source()

    new_opts =
      state.opts
      |> Map.put(:window_keep, keep)
      |> Map.put(:half_life_ms, half_life)
      |> Map.put(:recall_limit, recall_limit)
      |> Map.put(:min_jaccard, min_jaccard)
      |> Map.put(:recall_source, recall_source)

    {:reply, :ok,
     %{state | window_keep: keep, opts: new_opts, window: Window.trim(state.window, keep)}}
  end

  @impl true
  def handle_call(:reset, _from, _state), do: {:reply, :ok, default_state()}

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  ## ─────────────────────────────── Helpers ───────────────────────────────

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

  defp running? do
    case Process.whereis(__MODULE__) do
      pid when is_pid(pid) -> true
      _ -> false
    end
  end

  defp now_ms, do: System.monotonic_time(:millisecond)

  # ── Emotional enrichment on write ────────────────────────────────────────

  # Enrich meta with an emotional header from MoodCore, unless the caller
  # already supplied one. This lets us capture the climate at encode-time
  # without forcing any particular upstream shape.
  defp enrich_meta_with_emotion(meta) when is_map(meta) do
    has_emotion? =
      case meta[:emotion] || meta["emotion"] do
        %{} -> true
        _ -> false
      end

    # If caller already provided a full emotion header, don't touch it.
    if has_emotion? do
      meta
    else
      snap = safe_mood_snapshot()

      {levels, indices, latents, tone_hint} =
        case snap do
          %{} = s ->
            {
              Map.get(s, :levels) || %{},
              Map.get(s, :mood) || Map.get(s, :mood_indices) || %{},
              Map.get(s, :latents) || %{},
              Map.get(s, :tone_hint) || Map.get(s, :tone) || :neutral
            }

          _ ->
            {%{}, %{}, %{}, nil}
        end

      tone_from_meta =
        case meta[:tone_reaction] || meta["tone_reaction"] do
          nil ->
            case meta[:reaction] || meta["reaction"] do
              %{} = r -> r[:tone_reaction] || r["tone_reaction"]
              _ -> nil
            end

          val ->
            val
        end

      tone_reaction =
        tone_from_meta ||
          tone_hint ||
          :neutral

      # If we truly have no signal, leave meta as-is.
      if levels == %{} and indices == %{} and latents == %{} and is_nil(tone_reaction) do
        meta
      else
        norm_tone = normalize_tone(tone_reaction) || :neutral

        emotional_header = %{
          tone_reaction: norm_tone,
          latents: latents,
          mood_levels: levels,
          mood_indices: indices
        }

        meta
        |> Map.put_new(:emotion, emotional_header)
        |> Map.put_new(:tone_reaction, norm_tone)
        |> Map.put_new(:latents, latents)
      end
    end
  end

  defp enrich_meta_with_emotion(meta), do: meta

  defp safe_mood_snapshot do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :snapshot, 0) do
      try do
        Brain.MoodCore.snapshot()
      rescue
        _ -> nil
      catch
        _, _ -> nil
      end
    else
      nil
    end
  end

  # Build cues from SI or opts. Prefer explicit :cues, then winners/atl_slate,
  # then unigram token phrases/norms, then sentence words. Fallback to `si` itself.
  defp build_cues(si, opts) do
    cond do
      Keyword.has_key?(opts, :cues) ->
        Keyword.get(opts, :cues)

      is_map(si) and is_list(si[:winners]) ->
        si[:winners]
        |> Enum.flat_map(fn w -> [w[:lemma], w["lemma"], w[:norm], w["norm"]] end)
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&safe_norm/1)

      is_map(si) and is_map(si[:atl_slate]) and is_list(si[:atl_slate][:winners]) ->
        si[:atl_slate][:winners]
        |> Enum.flat_map(fn w -> [w[:lemma], w["lemma"], w[:norm], w["norm"]] end)
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&safe_norm/1)

      is_map(si) and is_list(si[:tokens]) ->
        si[:tokens]
        |> Enum.filter(&(Map.get(&1, :n, 1) == 1))
        |> Enum.map(fn t ->
          cond do
            is_binary(t[:norm]) -> safe_norm(t[:norm])
            is_binary(t["norm"]) -> safe_norm(t["norm"])
            is_binary(t[:phrase]) -> safe_norm(t[:phrase])
            is_binary(t["phrase"]) -> safe_norm(t["phrase"])
            true -> ""
          end
        end)
        |> Enum.reject(&(&1 == ""))

      is_map(si) and is_binary(si[:sentence]) ->
        si[:sentence]
        |> safe_norm()
        |> String.split(" ", trim: true)

      true ->
        si
    end
  end

  # Prefer Normalize.norm/1; fallback locally if unavailable.
  defp safe_norm(nil), do: ""

  defp safe_norm(v) when is_binary(v) do
    if Code.ensure_loaded?(Normalize) and function_exported?(Normalize, :norm, 1) do
      try do
        apply(Normalize, :norm, [v])
      rescue
        _ -> local_norm(v)
      catch
        _, _ -> local_norm(v)
      end
    else
      local_norm(v)
    end
  end

  defp safe_norm(v), do: v |> to_string() |> safe_norm()

  defp local_norm(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  # Scope filter: all pairs in `scope` must match meta (atom-or-string keys)
  defp meta_matches_scope?(%{episode: %{meta: meta}}, scope)
       when is_map(meta) and is_map(scope) do
    Enum.all?(scope, fn {k, v} -> Map.get(meta, k, Map.get(meta, to_string(k))) == v end)
  end

  defp meta_matches_scope?(_rec, _scope), do: false

  defp maybe_set_returned(meas, n) when is_map(meas) do
    case meas do
      %{returned: _} -> %{meas | returned: n}
      _ -> meas
    end
  end

  # Outcome uplift & evidence standardization
  defp apply_outcome_uplift(%{score: s} = rec, outcome_w) when is_number(s) do
    outcome =
      case rec do
        %{episode: %{meta: meta}} when is_map(meta) ->
          meta[:outcome_score] || meta["outcome_score"] || 0.0

        _ ->
          0.0
      end

    # Bound outcome in [-1, 1]. Positive outcome boosts; negative dampens slightly.
    o = clamp(outcome, -1.0, 1.0)

    factor =
      cond do
        o > 0.0 -> 1.0 + outcome_w * o
        o < 0.0 -> max(0.0, 1.0 + outcome_w * 0.5 * o)
        true -> 1.0
      end

    Map.put(rec, :score, clamp01(s * factor))
  end

  defp apply_outcome_uplift(rec, _w), do: rec

  defp outcome_weight do
    w = Application.get_env(:brain, :episodes_outcome_weight, 0.25)
    if is_number(w) and w >= 0, do: min(w, 1.0), else: 0.25
  end

  defp standardize_for_evidence(%{score: s, at: at, episode: ep} = rec) do
    meta           = ep[:meta] || %{}
    priors         = priors_from_meta(meta)
    emotion_priors = emotion_priors_from_meta(meta)
    hint           = hint_from_meta(meta)

    rec
    |> Map.put(:priors, priors)
    |> Map.put(:emotion_priors, emotion_priors)
    |> Map.put(:hint, hint)
    |> Map.put(:score, clamp01(s))
    |> Map.put(:at, at)
    |> Map.put(:episode, ep)
  end

  defp priors_from_meta(meta) when is_map(meta) do
    senses =
      case meta[:senses_prior] || meta["senses_prior"] do
        m when is_map(m) -> m
        _ -> %{}
      end

    intent =
      case meta[:intent_prior] || meta["intent_prior"] || meta[:intent] || meta["intent"] do
        m when is_map(m) -> m
        s when is_binary(s) -> %{s => 1.0}
        _ -> %{}
      end

    %{senses: senses, intent: intent}
  end

  defp priors_from_meta(_), do: %{senses: %{}, intent: %{}}

  # Emotion priors are per-episode emotional fingerprints derived from meta.
  # They are NOT aggregated here — the caller can fold over recall results to
  # build a prior for the current event.
  defp emotion_priors_from_meta(meta) when is_map(meta) do
    emotion =
      case meta[:emotion] || meta["emotion"] do
        %{} = m -> m
        _ -> %{}
      end

    latents =
      case emotion[:latents] || emotion["latents"] || meta[:latents] || meta["latents"] do
        %{} = l -> l
        _ -> %{}
      end

    tone_raw =
      emotion[:tone_reaction] || emotion["tone_reaction"] ||
        meta[:tone_reaction] || meta["tone_reaction"]

    %{
      threat_prior:  to_unit(Map.get(latents, :threat)  || Map.get(latents, "threat")),
      safety_prior:  to_unit(Map.get(latents, :safety)  || Map.get(latents, "safety")),
      reward_prior:  to_unit(Map.get(latents, :reward)  || Map.get(latents, "reward")),
      control_prior: to_unit(Map.get(latents, :control) || Map.get(latents, "control")),
      tone_reaction: normalize_tone(tone_raw)
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()
  end

  defp emotion_priors_from_meta(_), do: %{}

  defp hint_from_meta(meta) do
    emotion = emotion_priors_from_meta(meta)

    [
      (meta[:route] || meta["route"]) && "route=#{meta[:route] || meta["route"]}",
      (meta[:keyword] || meta["keyword"]) && "kw=#{meta[:keyword] || meta["keyword"]}",
      is_number(meta[:conflict] || meta["conflict"]) &&
        "conf=#{Float.round((meta[:conflict] || meta["conflict"]) * 100, 1)}%",
      is_number(meta[:outcome_score] || meta["outcome_score"]) &&
        "out=#{Float.round(meta[:outcome_score] || meta["outcome_score"], 2)}",
      is_atom(emotion[:tone_reaction]) &&
        "tone=#{emotion[:tone_reaction]}",
      is_number(emotion[:threat_prior]) &&
        "thr=#{Float.round(emotion[:threat_prior], 2)}",
      is_number(emotion[:safety_prior]) &&
        "safe=#{Float.round(emotion[:safety_prior], 2)}"
    ]
    |> Enum.filter(& &1)
    |> Enum.join(" · ")
  end

  defp clamp01(v) when is_number(v), do: clamp(v, 0.0, 1.0)
  defp clamp01(_), do: 0.0

  defp clamp(v, lo, hi) when is_number(v) and is_number(lo) and is_number(hi) and lo <= hi do
    cond do
      v < lo -> lo
      v > hi -> hi
      true -> v
    end
  end

  # Optional persistence (safe no-op if disabled/missing)
  defp maybe_persist(at, %{slate: slate, meta: meta, norms: norms}) do
    case Application.get_env(:brain, :episodes_persist, :off) do
      :db ->
        base_args = %{
          at: at,
          slate: slate,
          meta: meta,
          norms: MapSet.to_list(norms)
        }

        # Allow callers to sneak embedding/tags through meta (forward-compatible).
        embedding =
          case meta do
            %{} -> meta[:embedding] || meta["embedding"]
            _ -> nil
          end

        tags =
          case meta do
            %{} -> meta[:tags] || meta["tags"] || meta[:labels] || meta["labels"]
            _ -> nil
          end

        args =
          base_args
          |> maybe_put(:embedding, embedding)
          |> maybe_put(:tags, tags)

        if Code.ensure_loaded?(Db.Episode) do
          cond do
            function_exported?(Db.Episode, :enqueue_write, 1) ->
              safe_apply(Db.Episode, :enqueue_write, [args])

            function_exported?(Db.Episode, :insert, 1) ->
              safe_apply(Db.Episode, :insert, [args])

            true ->
              :ok
          end
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  defp maybe_persist(_at, _ep), do: :ok

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp safe_apply(mod, fun, args) do
    try do
      apply(mod, fun, args)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  ## ───────────────────────────── Synth Fallback ─────────────────────────────

  defp synthesize_episode_for_evidence(si, cues) do
    {at, ep} = build_synth_episode(si, cues)

    %{score: 0.0, at: at, episode: ep}
    |> Evidence.ensure_winners_for_evidence()
    |> standardize_for_evidence()
  end

  defp build_synth_episode(si, cues) do
    norms =
      cues
      |> List.wrap()
      |> Enum.flat_map(fn
        s when is_binary(s) ->
          String.split(safe_norm(s), ~r/\s+/, trim: true)

        m when is_map(m) ->
          candidate =
            Map.get(m, :lemma) || Map.get(m, "lemma") ||
              Map.get(m, :norm) || Map.get(m, "norm") ||
              Map.get(m, :word) || Map.get(m, "word")

          if is_binary(candidate), do: [safe_norm(candidate)], else: []

        _ ->
          []
      end)
      |> Enum.reject(&(&1 == ""))
      |> MapSet.new()

    meta = %{
      route: :synth,
      reason: :hippocampus_not_running,
      keyword: (si[:keyword] || si["keyword"] || si[:sentence] || "") |> to_string(),
      intent:  (si[:intent]  || si["intent"]  || :none)
    }

    at = now_ms()
    ep = %{slate: minimal_slate_from_si(si), meta: meta, norms: norms}
    {at, ep}
  end

  defp minimal_slate_from_si(si) do
    winners =
      (si[:lifg_choices] || [])
      |> Enum.map(fn ch ->
        %{
          id: ch[:id] || ch["id"],
          lemma: ch[:lemma] || ch["lemma"],
          token_index: ch[:token_index] || ch["token_index"]
        }
      end)

    %{by_norm: %{}, winners: winners}
  end

  ## ───────────────────────────── Small helpers ──────────────────────────────

  defp to_unit(nil), do: nil

  defp to_unit(v) when is_number(v) do
    v = v * 1.0

    cond do
      v < 0.0 -> 0.0
      v > 1.0 -> 1.0
      true -> v
    end
  end

  defp to_unit(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {n, _} -> to_unit(n)
      _ -> nil
    end
  end

  defp to_unit(_), do: nil

  defp normalize_tone(nil), do: nil

  defp normalize_tone(t) when is_atom(t) do
    case t do
      :warm -> :warm
      :cool -> :cool
      :deescalate -> :deescalate
      :neutral -> :neutral
      _ -> nil
    end
  end

  defp normalize_tone(t) when is_binary(t) do
    t
    |> String.trim()
    |> String.downcase()
    |> case do
      "warm" -> :warm
      "cool" -> :cool
      "deescalate" -> :deescalate
      "de-escalate" -> :deescalate
      "de_escalate" -> :deescalate
      "neutral" -> :neutral
      _ -> nil
    end
  end

  defp normalize_tone(_), do: nil
end

