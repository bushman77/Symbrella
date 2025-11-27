defmodule Core.BrainAdapter do
  @moduledoc """
  Runtime bridge for Core → Brain(+regions). Centralizes guarded calls so Core stays slim.

  Notes:
  - All functions are best-effort: they never crash the Core pipeline.
  - Uses module-atoms (:"Elixir.Brain.*") + apply/3 to avoid hard compile coupling.
  """

  @brain :"Elixir.Brain"
  @atl :"Elixir.Brain.ATL"
  @hippo :"Elixir.Brain.Hippocampus"
  @hippo_writer :"Elixir.Brain.Hippocampus.Writer"
  @amyg :"Elixir.Brain.Amygdala"

  @timeout 2_000

  @type cell_id :: binary()
  @type cell_item :: cell_id | map() | struct()

  @type pos ::
          :noun
          | :verb
          | :adj
          | :adv
          | :pron
          | :det
          | :adp
          | :num
          | :part
          | :intj
          | :conj
          | :aux
          | :punct
          | :sym
          | :x
          | atom()

  @type key :: cell_id | {binary(), pos} | {:mwe, [binary()]}
  @type syn_obj :: %{
          term: binary(),
          pos: pos | nil,
          weight: number() | nil,
          source: atom() | nil,
          key: key | nil
        }
  @type syn_result :: %{optional(key()) => [syn_obj()]}

  # ───────────────────────── Base availability ─────────────────────────

  @doc "Is the Brain process currently available?"
  @spec available?() :: boolean()
  def available?, do: is_pid(Process.whereis(@brain))

  # ───────────────────────── Existing API (kept) ─────────────────────────

  @doc """
  Activate a list of cells (rows, maps with :id, or ids). Payload is optional.
  Fire-and-forget cast; returns :ok.
  """
  @spec activate_cells([cell_item()], map()) :: :ok
  def activate_cells(items, payload \\ %{})
  def activate_cells([], _payload), do: :ok

  def activate_cells(items, payload) when is_list(items) and is_map(payload) do
    GenServer.cast(@brain, {:activate_cells, items, payload})
  end

  @doc "Fetch a full snapshot of Brain state."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(@brain, :snapshot, @timeout)

  @doc "Call a specific neuron by id with a request (routed by Brain)."
  @spec cell_call(cell_id(), term()) :: term()
  def cell_call(id, req), do: GenServer.call(@brain, {:cell, id, req}, @timeout)

  @doc "Cast a message to a specific neuron by id."
  @spec cell_cast(cell_id(), term()) :: :ok
  def cell_cast(id, msg), do: GenServer.cast(@brain, {:cell, id, msg})

  # ───────────────────────── Synonyms (P-201) ─────────────────────────

  @doc """
  Batch synonym retrieval via Brain. Accepts cell_ids, {lemma,pos} tuples,
  or MWEs as {:mwe, [tokens]}. Returns {:ok, %{key => [syn_obj]}} or {:error, reason}.

  This is a runtime call; if Brain isn't available, returns {:error, :unavailable}.
  """
  @spec synonyms_for_keys([key()], map() | keyword()) :: {:ok, syn_result()} | {:error, term()}
  def synonyms_for_keys(keys, opts \\ %{})
  def synonyms_for_keys([], _opts), do: {:ok, %{}}

  def synonyms_for_keys(keys, opts) when is_list(keys) do
    payload = %{keys: keys, opts: normalize_opts(opts)}
    safe_call({:synonyms_for_keys, payload}, @timeout)
  end

  # ───────────────────────── Core pipeline helpers ─────────────────────────

  @doc """
  Brain STM stage (for Core.brain_roundtrip/2).
  Returns a map (mergeable) or a non-map that Core will ignore.
  """
  @spec stm(map()) :: map() | term()
  def stm(%{} = si_map) do
    safe_apply(@brain, :stm, [si_map], :unavailable)
  end

  def stm(other), do: other

  @doc """
  ATL attach pairs (for Core.brain_roundtrip/2).
  """
  @spec atl_attach_lifg_pairs(map(), keyword()) :: map() | term()
  def atl_attach_lifg_pairs(%{} = si_map, opts) when is_list(opts) do
    safe_apply(@atl, :attach_lifg_pairs, [si_map, opts], :unavailable)
  end

  def atl_attach_lifg_pairs(other, _opts), do: other

  @doc """
  Episodes attach (best-effort). Mirrors the old Core implementation.
  """
  @spec maybe_attach_episodes(map(), keyword()) :: map()
  def maybe_attach_episodes(%{} = si, opts) when is_list(opts) do
    enabled? = episodes_enabled?(opts)
    hippo_up? = is_pid(Process.whereis(@hippo))

    cond do
      not enabled? or not hippo_up? ->
        si

      true ->
        pass =
          []
          |> put_if_present(:source, Keyword.get(opts, :recall_source))
          |> put_if_present(:embedding, Keyword.get(opts, :episode_embedding))

        si2 =
          case safe_apply(@hippo, :attach_episodes, [si, pass], si) do
            %{} = out -> out
            _ -> si
          end

        eps = get_in(si2, [:evidence, :episodes]) || []
        emit([:brain, :core, :episodes_attached], %{count: length(eps)}, %{mode: :pre_lifg})
        si2
    end
  rescue
    _ -> si
  catch
    _, _ -> si
  end

  def maybe_attach_episodes(si, _opts), do: si

  @doc """
  Amygdala hook: fast affective appraisal between episodes + LIFG.
  """
  @spec maybe_amygdala_react(map(), keyword()) :: map()
  def maybe_amygdala_react(%{} = si, opts) when is_list(opts) do
    if Code.ensure_loaded?(@amyg) and function_exported?(@amyg, :react, 2) do
      try do
        case apply(@amyg, :react, [si, opts]) do
          %{} = emotion -> Map.put(si, :emotion, emotion)
          _ -> si
        end
      rescue
        _ -> si
      catch
        _, _ -> si
      end
    else
      si
    end
  end

  def maybe_amygdala_react(si, _opts), do: si

  @doc """
  ATL ingest/reduce. Writes :atl_slate and a trace entry.
  """
  @spec maybe_ingest_atl(map(), keyword()) :: map()
  def maybe_ingest_atl(%{lifg_choices: choices, tokens: tokens} = si, _opts)
      when is_list(choices) and is_list(tokens) do
    if choices == [] do
      si
    else
      slate =
        case Process.whereis(@atl) do
          pid when is_pid(pid) -> safe_apply(@atl, :ingest, [choices, tokens], %{})
          _ -> safe_apply(@atl, :reduce, [choices, tokens], %{})
        end

      # If ATL isn't available, slate might be non-map. Preserve safety.
      if is_map(slate) do
        si
        |> Map.put(:atl_slate, slate)
        |> Map.update(:trace, [], fn tr ->
          [
            %{
              stage: :atl,
              ts_ms: System.system_time(:millisecond),
              winners: Map.get(slate, :winner_count, 0),
              concepts: slate |> Map.get(:by_norm, %{}) |> map_size()
            }
            | tr
          ]
        end)
      else
        si
      end
    end
  rescue
    _ -> si
  end

  def maybe_ingest_atl(si, _opts), do: si

  @doc """
  Hippocampus encode from :atl_slate. Writes :episode summary.
  """
  @spec maybe_encode_hippocampus(map()) :: map()
  def maybe_encode_hippocampus(%{atl_slate: slate} = si) when is_map(slate) do
    if Process.whereis(@hippo) do
      ep = safe_apply(@hippo, :encode, [slate], nil)

      if is_map(ep) do
        Map.put(si, :episode, Map.take(ep, [:ts_ms, :token_count, :winner_count]))
      else
        si
      end
    else
      si
    end
  rescue
    _ -> si
  end

  def maybe_encode_hippocampus(si), do: si

  @doc """
  Optional episode persistence (best-effort).
  """
  @spec maybe_persist_episode(map(), keyword()) :: map()
  def maybe_persist_episode(%{} = si, opts) when is_list(opts) do
    safe_apply(
      @hippo_writer,
      :maybe_persist,
      [
        si,
        [
          persist: Keyword.get(opts, :persist_episodes),
          embedding: Keyword.get(opts, :episode_embedding),
          user_id: Keyword.get(opts, :user_id)
        ]
      ],
      si
    )
    |> case do
      %{} = out -> out
      _ -> si
    end
  rescue
    _ -> si
  end

  def maybe_persist_episode(si, _opts), do: si

  @doc """
  Notify Brain activation (cast). Adds a small trace entry either way.
  """
  @spec notify_activation(map(), keyword()) :: map()
  def notify_activation(%{} = si, opts) when is_list(opts) do
    payload = %{
      delta: Keyword.get(opts, :delta, 0.1),
      decay: Keyword.get(opts, :decay, 0.98),
      via: :core
    }

    rows = Map.get(si, :active_cells, []) || []
    lifg_count = si |> Map.get(:lifg_choices, []) |> length()

    if rows != [] do
      _ = activate_cells(rows, payload)
    end

    Map.update(si, :trace, [], fn tr ->
      [{:activated, %{rows: length(rows), lifg_choices: lifg_count, shape: :activate_cells}} | tr]
    end)
  end

  def notify_activation(si, _opts), do: si

  # ───────────────────────── Internals ─────────────────────────

  defp episodes_enabled?(opts) do
    case Keyword.get(opts, :episodes, nil) do
      true -> true
      false -> false
      _ -> Application.get_env(:brain, :episodes_mode, :on) != :off
    end
  end

  @spec normalize_opts(map() | keyword() | term()) :: map()
  defp normalize_opts(opts) when is_list(opts), do: Map.new(opts)
  defp normalize_opts(%{} = opts), do: opts
  defp normalize_opts(_), do: %{}

  defp put_if_present(kvs, _k, nil), do: kvs
  defp put_if_present(kvs, k, v), do: Keyword.put(kvs, k, v)

  defp emit(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end

  defp safe_apply(mod, fun, args, default) when is_atom(mod) and is_atom(fun) and is_list(args) do
    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, fun, length(args)) ->
        try do
          apply(mod, fun, args)
        rescue
          _ -> default
        catch
          _, _ -> default
        end

      true ->
        default
    end
  end

  @spec safe_call(term(), non_neg_integer()) :: {:ok, term()} | {:error, term()}
  defp safe_call(msg, timeout) do
    if available?() do
      try do
        case GenServer.call(@brain, msg, timeout) do
          {:ok, _} = ok -> ok
          %{} = map -> {:ok, map}
          other -> {:ok, other}
        end
      catch
        :exit, {:noproc, _} -> {:error, :unavailable}
        :exit, reason -> {:error, reason}
      end
    else
      {:error, :unavailable}
    end
  end
end

