# apps/core/lib/core/brain_adapter.ex
defmodule Core.BrainAdapter do
  @moduledoc """
  Runtime bridge for Core → Brain (+regions).

  This module centralizes **guarded, best-effort** interactions with Brain-side
  processes so Core’s pipeline code can remain slim and resilient.

  Design goals:

  - **Runtime-only coupling:** Uses module atoms like `:"Elixir.Brain.Hippocampus"` and
    `apply/3` so Core does not depend on Brain modules at compile time.
  - **Best-effort semantics:** Functions that participate in the Core pipeline are
    written to **avoid crashing** the caller when Brain or a region is not running.
  - **Explicit exceptions:** A small number of “direct call” helpers (`snapshot/0`,
    `cell_call/2`) use `GenServer.call/3` directly and will exit if Brain is not available.
    Use `available?/0` to guard those calls.

  ## Telemetry

  Some functions emit telemetry (e.g., episodes attach). This module uses a small
  local `emit/3` helper that becomes a no-op if `:telemetry` is not available.
  """

  @brain :"Elixir.Brain"
  @atl :"Elixir.Brain.ATL"
  @hippo :"Elixir.Brain.Hippocampus"
  @hippo_writer :"Elixir.Brain.Hippocampus.Writer"
  @amyg :"Elixir.Brain.Amygdala"

  # Affect
  @affect_appraisal :"Elixir.Brain.AffectiveAppraisal"
  @mood_core :"Elixir.Brain.MoodCore"

  @timeout 2_000

  @typedoc "BrainCell identifier (typically a persisted id like \"good|noun|0\")."
  @type cell_id :: binary()

  @typedoc "Items that can reference a cell: ids, maps, or structs carrying an `:id`."
  @type cell_item :: cell_id | map() | struct()

  @typedoc """
  Part-of-speech tag (Core-level type union).

  This is intentionally permissive; downstream can treat unknown atoms as `:other`.
  """
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

  @typedoc """
  Lookup key used by the synonyms API.

  - `cell_id()` — direct sense/cell id
  - `{lemma, pos}` — lemma and POS tuple
  - `{:mwe, [tokens]}` — multiword expression key
  """
  @type key :: cell_id | {binary(), pos} | {:mwe, [binary()]}

  @typedoc """
  Synonym record returned by Brain.

  Keys are best-effort and may be `nil` depending on source/adapter implementation.
  """
  @type syn_obj :: %{
          term: binary(),
          pos: pos | nil,
          weight: number() | nil,
          source: atom() | nil,
          key: key | nil
        }

  @typedoc "Synonyms results keyed by the original request keys."
  @type syn_result :: %{optional(key()) => [syn_obj()]}

  # ───────────────────────── Base availability ─────────────────────────

  @doc """
  Returns `true` if the Brain process is currently available (registered and alive).

  This is the recommended guard before calling “direct-call” functions such as
  `snapshot/0` and `cell_call/2`.
  """
  @spec available?() :: boolean()
  def available?, do: pid_alive?(@brain)

  # ───────────────────────── Existing API (kept) ─────────────────────────

  @doc """
  Activates a list of cells (rows, maps with `:id`, or ids). Payload is optional.

  This is a fire-and-forget `GenServer.cast/2` to the Brain process.
  It returns `:ok` even if Brain is not running (the message is simply not delivered).
  """
  @spec activate_cells([cell_item()], map()) :: :ok
  def activate_cells(items, payload \\ %{})
  def activate_cells([], _payload), do: :ok

  def activate_cells(items, payload) when is_list(items) and is_map(payload) do
    GenServer.cast(@brain, {:activate_cells, items, payload})
  end

  @doc """
  Fetches a snapshot of Brain state via `GenServer.call/3`.

  This is a **direct** call. If Brain is not running, this call will exit.
  """
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(@brain, :snapshot, @timeout)

  @doc """
  Calls a specific neuron by id with a request (routed by Brain).

  This is a **direct** call. If Brain is not running, this call will exit.
  """
  @spec cell_call(cell_id(), term()) :: term()
  def cell_call(id, req), do: GenServer.call(@brain, {:cell, id, req}, @timeout)

  @doc """
  Casts a message to a specific neuron by id.

  This is fire-and-forget; returns `:ok` regardless of Brain availability.
  """
  @spec cell_cast(cell_id(), term()) :: :ok
  def cell_cast(id, msg), do: GenServer.cast(@brain, {:cell, id, msg})

  # ───────────────────────── Synonyms ─────────────────────────

  @doc """
  Retrieves synonyms via Brain for a batch of keys.

  Returns:
  - `{:ok, %{key => [syn_obj]}}` on success
  - `{:error, :unavailable}` when Brain is not available
  - `{:error, reason}` for other call failures

  This function is **best-effort** and will not crash the Core pipeline.
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
  Runs Brain STM stage (for `Core.brain_roundtrip/2`) in a best-effort manner.

  - When Brain exports `stm/1`, it is invoked.
  - Otherwise returns `:unavailable`.
  """
  @spec stm(map()) :: map() | term()
  def stm(%{} = si_map) do
    safe_apply(@brain, :stm, [si_map], :unavailable)
  end

  def stm(other), do: other

  @doc """
  Attaches ATL ↔ LIFG pairing information best-effort.

  - Calls `Brain.ATL.attach_lifg_pairs/2` if available.
  - Otherwise returns `:unavailable`.
  """
  @spec atl_attach_lifg_pairs(map(), keyword()) :: map() | term()
  def atl_attach_lifg_pairs(%{} = si_map, opts) when is_list(opts) do
    safe_apply(@atl, :attach_lifg_pairs, [si_map, opts], :unavailable)
  end

  def atl_attach_lifg_pairs(other, _opts), do: other

  @doc """
  Optionally attaches episodic recall evidence using Hippocampus (best-effort).
  """
  @spec maybe_attach_episodes(map(), keyword()) :: map()
  def maybe_attach_episodes(%{} = si, opts) when is_list(opts) do
    enabled? = episodes_enabled?(opts)

    cond do
      not enabled? ->
        si

      not pid_alive?(@hippo) ->
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
  Amygdala hook (best-effort).
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
  Affective appraisal hook (best-effort).
  """
  @spec maybe_apply_affective_appraisal(map(), keyword()) :: map()
  def maybe_apply_affective_appraisal(%{} = si, opts) when is_list(opts) do
    opt = Keyword.get(opts, :affective_appraisal, :inherit)
    env_on = Application.get_env(:brain, :affective_appraisal, :on) != :off

    enabled =
      case opt do
        :off -> false
        false -> false
        :inherit -> env_on
        _ -> true
      end

    cond do
      not enabled ->
        si

      not (Code.ensure_loaded?(@affect_appraisal) and
               function_exported?(@affect_appraisal, :appraise, 1)) ->
        si

      true ->
        appraisal =
          try do
            apply(@affect_appraisal, :appraise, [si])
          rescue
            _ -> nil
          catch
            _, _ -> nil
          end

        if is_map(appraisal) do
          _ = safe_apply(@mood_core, :apply_appraisal, [appraisal], :ok)
          Map.put(si, :appraisal, appraisal)
        else
          si
        end
    end
  end

  def maybe_apply_affective_appraisal(si, _opts), do: si

  @doc """
  ATL ingest/reduce (best-effort).

  If there are LIFG choices:
  - If ATL is running, calls `ATL.ingest/2`
  - Otherwise calls `ATL.reduce/2`
  """
  @spec maybe_ingest_atl(map(), keyword()) :: map()
  def maybe_ingest_atl(%{lifg_choices: choices, tokens: tokens} = si, _opts)
      when is_list(choices) and is_list(tokens) do
    if choices == [] do
      si
    else
      slate =
        case Process.whereis(@atl) do
          pid when is_pid(pid) ->
            if Process.alive?(pid) do
              safe_apply(@atl, :ingest, [choices, tokens], %{})
            else
              safe_apply(@atl, :reduce, [choices, tokens], %{})
            end

          _ ->
            safe_apply(@atl, :reduce, [choices, tokens], %{})
        end

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
  Encodes an episode from `si.atl_slate` using Hippocampus (best-effort).
  """
  @spec maybe_encode_hippocampus(map()) :: map()
  def maybe_encode_hippocampus(%{atl_slate: slate} = si) when is_map(slate) do
    if pid_alive?(@hippo) do
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
  Optionally persists an episode via Hippocampus.Writer (best-effort).
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
  Notifies Brain of activation (best-effort cast) and records a trace entry.
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

  defp pid_alive?(name) when is_atom(name) do
    case Process.whereis(name) do
      pid when is_pid(pid) -> Process.alive?(pid)
      _ -> false
    end
  end

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
