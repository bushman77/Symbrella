defmodule Brain.Hippocampus do
  @moduledoc """
  Hippocampus — in-memory episodic window with lightweight recall + telemetry.

  Public API:
    • encode/2 — record an episode and return the `slate` unchanged (de-dup on head).
    • recall/2 — rank prior episodes with optional source:
        :memory (default), :db (pgvector), or :hybrid (merge).
    • attach_episodes/2 — write recall results to `si.evidence[:episodes]`.
    • configure/1, reset/0, snapshot/0 — runtime config and inspection.

  Notes
  -----
  • DB recall can use an `:embedding` in opts (list(float) or %Pgvector{}).
    If missing and :source is :db or :hybrid, we fall back to memory-only recall.
  • The recall `score` is treated as **familiarity**. We optionally apply an
    **outcome uplift** (bounded) based on `episode.meta[:outcome_score]`.
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

  @doc """
  Recall prior episodes relevant to `cues` (list/stringy slate/si).

  Options (per-call override):
    • :limit, :half_life_ms, :min_jaccard, :scope
    • :ignore_head — :auto | :always | :never | false
    • :source — :memory | :db | :hybrid (defaults to configured `recall_source`)
    • :embedding — required if :source is :db or :hybrid (query vector)

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
    • Pass-through opts such as `:source` and `:embedding` are honored.

  Returns `si` with `:evidence[:episodes]` set. Each item will include:
    • :score (familiarity after outcome uplift), :at, :episode
    • :priors (maps for :senses / :intent when derivable), :hint (short debug text)
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
      |> Enum.map(&standardize_for_evidence/1)

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
              [%{score: 0.0, at: at, episode: ep} |> Evidence.ensure_winners_for_evidence() |> standardize_for_evidence()]
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

        {window2, meta_final, at_written, ep_written} =
          case action do
            :refresh ->
              ep = %{slate: slate, meta: head_meta, norms: norms}
              {Window.append_or_refresh_head(state.window, ep, state.window_keep), head_meta, state.last_at || now_ms(), ep}

            :append ->
              ep = %{slate: slate, meta: meta_in, norms: norms}
              at = now_ms()
              {Window.trim([{at, ep} | state.window], state.window_keep), meta_in, at, ep}
          end

        Telemetry.emit_write(%{window_size: length(window2)}, %{meta: meta_final})

        Telemetry.maybe_echo_to_caller(
          from,
          [:brain, :hippocampus, :write],
          %{window_size: length(window2)},
          %{meta: meta_final}
        )

        # Optional persistence (safe no-op if disabled or schema missing)
        maybe_persist(at_written, ep_written)

        new_last =
          case window2 do
            [{at, e} | _] when is_integer(at) -> {at, e}
            _ -> nil
          end

        {:reply, :ok, %{state | window: window2, last: new_last, last_at: elem(new_last || {nil, nil}, 0)}}
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
            scope: scope_opt,
            ignore_head: ignore_head
          )

        ranked =
          ranked0
          |> Enum.filter(fn
            %{score: s} when is_number(s) -> s > 0.0
            _ -> false
          end)
          |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
          |> Enum.sort_by(& &1.score, :desc)
          |> Enum.take(limit)

        Telemetry.emit_recall(Map.merge(meas, %{source: :memory}), meta_map)
        Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :recall], Map.merge(meas, %{source: :memory}), meta_map)

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
              scope: scope_opt,
              ignore_head: ignore_head
            )

          ranked =
            ranked0
            |> Enum.filter(fn
              %{score: s} when is_number(s) -> s > 0.0
              _ -> false
            end)
            |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
            |> Enum.sort_by(& &1.score, :desc)
            |> Enum.take(limit)

          Telemetry.emit_recall(Map.merge(meas, %{source: :memory, fallback: :missing_embedding}), meta_map)
          Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :recall], Map.merge(meas, %{source: :memory}), meta_map)

          {:reply, ranked, state}
        else
          db_opts =
            opts
            |> Map.put_new(:k, limit)
            |> Map.put_new(:tau_s, div(half_life_ms, 1000))
            |> Map.put_new(:min_sim, 0.35)

          ranked =
            DB.recall(Map.to_list(db_opts))
            |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
            |> Enum.sort_by(& &1.score, :desc)
            |> Enum.take(limit)

          meas = %{k: length(ranked), source: :db}
          Telemetry.emit_recall(meas, %{source: :db})
          Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :recall], meas, %{source: :db})

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
            scope: scope_opt,
            ignore_head: ignore_head
          )

        mem_ranked =
          mem_ranked0
          |> Enum.filter(fn
            %{score: s} when is_number(s) -> s > 0.0
            _ -> false
          end)
          |> Enum.map(&apply_outcome_uplift(&1, outcome_w))

        # DB path (only if embedding provided)
        embedding = Map.get(opts, :embedding)

        db_ranked =
          if is_nil(embedding) do
            []
          else
            opts
            |> Map.put_new(:k, limit)
            |> Map.put_new(:tau_s, div(half_life_ms, 1000))
            |> Map.put_new(:min_sim, 0.35)
            |> Map.to_list()
            |> DB.recall()
            |> Enum.map(&apply_outcome_uplift(&1, outcome_w))
          end

        ranked =
          (mem_ranked ++ db_ranked)
          |> Enum.sort_by(& &1.score, :desc)
          |> Enum.take(limit)

        meas = %{mem_k: length(mem_ranked), db_k: length(db_ranked), source: :hybrid, had_embedding?: not is_nil(embedding)}
        Telemetry.emit_recall(meas, %{source: :hybrid})
        Telemetry.maybe_echo_to_caller(from, [:brain, :hippocampus, :recall], meas, %{source: :hybrid})

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
    %{
      window_keep: Config.defaults().window_keep,
      window: [],
      last: nil,
      last_at: nil,
      opts: %{
        window_keep: Config.defaults().window_keep,
        half_life_ms: Config.defaults().half_life_ms,
        recall_limit: Config.defaults().recall_limit,
        min_jaccard: Config.defaults().min_jaccard,
        recall_source: Config.defaults().recall_source
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
        |> Enum.map(&safe_norm/1)

      is_map(si) and is_map(si[:atl_slate]) and is_list(si[:atl_slate][:winners]) ->
        Enum.flat_map(si[:atl_slate][:winners], fn w ->
          [w[:lemma], w["lemma"], w[:norm], w["norm"]]
        end)
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&safe_norm/1)

      is_map(si) and is_list(si[:tokens]) ->
        si[:tokens]
        |> Enum.filter(&(Map.get(&1, :n, 1) == 1))
        |> Enum.map(&safe_norm(&1[:phrase]))

      is_map(si) and is_binary(si[:sentence]) ->
        si[:sentence]
        |> safe_norm()
        |> String.split(" ", trim: true)

      true ->
        si
    end
  end

  # Prefer Normalize.norm/1 if it exists; otherwise, use a local fallback that matches
  # the project’s normalization style (downcase, trim, collapse internal whitespace).
  # Use `apply/3` to avoid compile-time warnings when Normalize.norm/1 is not defined.
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

  # Match all key/value pairs in `scope` against episode meta (accept atom-or-string keys)
  defp meta_matches_scope?(%{episode: %{meta: meta}}, scope) when is_map(meta) and is_map(scope) do
    Enum.all?(scope, fn {k, v} -> Map.get(meta, k, Map.get(meta, to_string(k))) == v end)
  end

  defp meta_matches_scope?(_rec, _scope), do: false

  # ───────────── Outcome uplift & evidence standardization ─────────────

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
        o < 0.0 -> max(0.0, 1.0 + (outcome_w * 0.5) * o) # smaller penalty
        true    -> 1.0
      end

    score2 = clamp01(s * factor)
    Map.put(rec, :score, score2)
  end

  defp apply_outcome_uplift(rec, _w), do: rec

  defp outcome_weight do
    w = Application.get_env(:brain, :episodes_outcome_weight, 0.25)
    if is_number(w) and w >= 0, do: min(w, 1.0), else: 0.25
  end

  defp standardize_for_evidence(%{score: s, at: at, episode: ep} = rec) do
    priors = priors_from_meta(ep[:meta] || %{})
    hint   = hint_from_meta(ep[:meta] || %{})

    rec
    |> Map.put(:priors, priors)
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

  defp hint_from_meta(meta) do
    parts =
      [
        (meta[:route] || meta["route"]) && "route=#{meta[:route] || meta["route"]}",
        (meta[:keyword] || meta["keyword"]) && "kw=#{meta[:keyword] || meta["keyword"]}",
        (is_number(meta[:conflict] || meta["conflict"])) && "conf=#{Float.round((meta[:conflict] || meta["conflict"]) * 100, 1)}%",
        (is_number(meta[:outcome_score] || meta["outcome_score"])) && "out=#{Float.round(meta[:outcome_score] || meta["outcome_score"], 2)}"
      ]
      |> Enum.filter(& &1)

    Enum.join(parts, " · ")
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

  # ───────────── Optional persistence (safe no-op if disabled/missing) ─────────────

  defp maybe_persist(at, %{slate: slate, meta: meta, norms: norms}) do
    case Application.get_env(:brain, :episodes_persist, :off) do
      :db ->
        # Best-effort: only if Db.Episode is available and has insert/enqueue API.
        cond do
          Code.ensure_loaded?(Db.Episode) and function_exported?(Db.Episode, :enqueue_write, 1) ->
            try do
              Db.Episode.enqueue_write(%{at: at, slate: slate, meta: meta, norms: MapSet.to_list(norms)})
            rescue
              _ -> :ok
            catch
              _, _ -> :ok
            end

          Code.ensure_loaded?(Db.Episode) and function_exported?(Db.Episode, :insert, 1) ->
            try do
              _ = Db.Episode.insert(%{at: at, slate: slate, meta: meta, norms: MapSet.to_list(norms)})
            rescue
              _ -> :ok
            catch
              _, _ -> :ok
            end

          true ->
            :ok
        end

      _ ->
        :ok
    end
  end

  defp maybe_persist(_at, _ep), do: :ok
end

