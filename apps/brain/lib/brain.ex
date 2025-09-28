
defmodule Brain do
  @moduledoc """
  Central coordinator for `Brain.Cell` processes.

  Responsibilities:
    * Keep a lightweight STM view (`active_cells`, `attention`, `history`).
    * Provide a small API for activating cells in bulk or per‑cell routing.
    * Lazily start `Brain.Cell` processes behind a `DynamicSupervisor`.
    * Surface `active_cells` into an incoming SI (Semantic Input) snapshot.
    * Run LIFG Stage‑1 and fan‑out boosts/inhibitions as **side effects**.

  Intentionally thin so downstream stages (e.g., LIFG orchestration) can depend
  on it without surprises.
  """

  use GenServer
  require Logger

  alias Db.BrainCell, as: Row
  alias Brain.LIFG

  @name __MODULE__
  @registry Brain.Registry
  @cell_sup Brain.CellSup

  @cell_timeout 2_000

  @type id :: String.t()
  @type activation :: number()
  @type state :: %{
          history: list(),
          active_cells: %{optional(id()) => activation()},
          attention: MapSet.t(),
          activation_log: list()
        }

  # ─────────────────────────── Public API ───────────────────────────

  @doc "Start the coordinator under a supervisor."
  @spec start_link(term()) :: GenServer.on_start()
  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc """
  Blocking: merge a short‑term memory snapshot into the given SI map.

  Returns the SI with `:active_cells` populated from Brain's current state.
  """
  @spec stm(map()) :: map()
  def stm(si) when is_map(si), do: GenServer.call(@name, {:stm, si})

  @doc """
  Non‑blocking: activate a batch of rows or ids with a given payload.

  Examples:
      Brain.activate_cells(["id1","id2"], %{delta: +1})
      Brain.activate_cells(si, %{delta: -0.25})  # when si has :active_cells

  The payload shape is delegated to the cell; by convention `%{delta: number}`.
  """
  @spec activate_cells([Row.t() | id()] | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload),
    do: GenServer.cast(@name, {:activate_cells, rows_or_ids, payload})

  @doc """
  Optional helper: route a synchronous call to a single cell (e.g., `:status`).

  Returns `{:ok, ...}` from the cell or `{:error, :not_found}` if the cell is not running.
  """
  @spec cell_status(id()) :: {:ok, term()} | {:error, :not_found}
  def cell_status(id) when is_binary(id), do: GenServer.call(@name, {:cell, id, :status})

  @doc "Optional helper: route an async cast to a single cell."
  @spec cell_cast(id(), term()) :: :ok
  def cell_cast(id, msg) when is_binary(id), do: GenServer.cast(@name, {:cell, id, msg})

  @doc "State snapshot (debug/testing)."
  @spec snapshot() :: state()
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Via tuple for a cell id, using `Brain.Registry`."
  @spec via(id()) :: {:via, Registry, {module(), id()}}
  def via(id) when is_binary(id), do: {:via, Registry, {@registry, id}}

  @doc """
  Run LIFG Stage‑1 and apply control signals as a side effect.

  `si_or_candidates` may be:
    * a flat list of candidate maps
    * a map with `:lifg_candidates` or `:candidates` (list)
    * a map with `:candidates_by_token` (map of token_index => [cands])

  Returns `{:ok, lifg_result}` where `lifg_result` has `:choices`, `:boosts`, `:inhibitions`, `:audit`.
  """
  @spec lifg_stage1(map() | [map()], [number()], keyword()) ::
          {:ok,
           %{
             choices: [map()],
             boosts: [{binary(), number()}],
             inhibitions: [{binary(), number()}],
             audit: map()
           }}
  def lifg_stage1(si_or_candidates, context_vec, opts \\ []) when is_list(context_vec) do
    GenServer.call(@name, {:lifg_stage1, si_or_candidates, context_vec, opts}, :infinity)
  end

  # ─────────────────────────── GenServer ────────────────────────────

  @impl true
  @spec init(:ok) :: {:ok, state()}
  def init(:ok) do
    {:ok,
     %{
       history: [],
       active_cells: %{},
       attention: MapSet.new(),
       activation_log: []
     }}
  end

  # ——— handle_call ———

  @impl true
  def handle_call({:lifg_stage1, si_or_cands, context_vec, opts}, _from, state) do
    candidates = lifg_candidates!(si_or_cands)

    lifg_opts =
      [scores: :top2, normalize: :softmax, parallel: :auto, margin_threshold: 0.12]
      |> Keyword.merge(opts)

    t0 = System.monotonic_time()

    case LIFG.disambiguate_stage1(candidates, context_vec, lifg_opts) do
      {:ok, out} ->
        # Side effect: deliver deltas to cells (auto-start if needed)
{boosts2, inhib2} = maybe_rescale_signals(out, lifg_opts)
apply_control_signals(boosts2, inhib2, lifg_opts)

        ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

:telemetry.execute(
  [:brain, :pipeline, :lifg_stage1, :stop],
  %{duration_ms: ms},
  Map.merge(
    Map.take(out.audit, [:groups, :ctx_dim, :normalize, :scores_mode, :parallel]),
    %{
      winners: length(out.choices),
      boosts: length(out.boosts),
      inhibitions: length(out.inhibitions)
    }
  )
)

        {:reply, {:ok, out}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:stm, si}, _from, state) when is_map(si) do
    {:reply, Map.put(si, :active_cells, state.active_cells), state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call({:cell, id, req}, _from, state) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> {:reply, GenServer.call(pid, req, @cell_timeout), state}
      [] -> {:reply, {:error, :not_found}, state}
    end
  end

  # ——— handle_cast ———

  @impl true
  def handle_cast({:activate_cells, rows_or_ids, payload}, state) do
    rows_or_ids
    |> extract_items()
    |> Enum.each(fn
      %Row{} = row -> ensure_start_and_cast(row, payload)
      id when is_binary(id) -> ensure_start_and_cast(id, payload)
      other -> Logger.warning("Brain.activate_cells: unknown item #{inspect(other)}")
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:activation_report, id, a}, state) when is_binary(id) and is_number(a) do
    {:noreply, %{state | active_cells: Map.put(state.active_cells, id, a)}}
  end

  @impl true
  def handle_cast({:cell, id, msg}, state) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> GenServer.cast(pid, msg)
      [] -> :ok
    end

    {:noreply, state}
  end

  # ─────────────────────────── Helpers ──────────────────────────────

  # ----- LIFG candidate extraction -----

  @doc false
  @spec lifg_candidates!(map() | [map()]) :: [map()]
  defp lifg_candidates!(list) when is_list(list), do: list
  defp lifg_candidates!(%{lifg_candidates: list}) when is_list(list), do: list
  defp lifg_candidates!(%{candidates: list}) when is_list(list), do: list

  defp lifg_candidates!(%{candidates_by_token: groups}) when is_map(groups) do
    Enum.flat_map(groups, fn {tidx, senses} ->
      Enum.map(senses, fn s -> Map.put(s, :token_index, tidx) end)
    end)
  end

  defp lifg_candidates!(other),
    do: raise(ArgumentError, "Cannot extract LIFG candidates from: #{inspect(other)}")

  # ----- Control signal fan‑out -----

  @doc false
  @spec apply_control_signals([{binary(), number()}], [{binary(), number()}], keyword()) :: :ok
  defp apply_control_signals(boosts, inhibitions, opts) do
    coalesce? = Keyword.get(opts, :coalesce, true)
    conc = Keyword.get(opts, :signal_concurrency, System.schedulers_online())
    delta_key = Keyword.get(opts, :delta_key, :delta) # if cells ever change their payload key

    signals =
      boosts
      |> Kernel.++(inhibitions)
      |> then(fn pairs -> if coalesce?, do: coalesce_pairs(pairs), else: pairs end)

    signals
    |> Task.async_stream(
      fn {id, delta} ->
        ensure_started(id, id)
        GenServer.cast(via(id), {:activate, %{delta_key => delta}})
      end,
      max_concurrency: conc,
      timeout: :infinity
    )
    |> Stream.run()

    :ok
  end

  @doc false
  @spec coalesce_pairs([{binary(), number()}]) :: [{binary(), number()}]
  defp coalesce_pairs(pairs) do
    pairs
    |> Enum.group_by(fn {id, _} -> id end, fn {_, d} -> d end)
    |> Enum.map(fn {id, ds} -> {id, Enum.sum(ds)} end)
  end

  # ----- Activation utilities -----

  @doc false
  @spec extract_items([Row.t() | id()] | map()) :: [Row.t() | id()]
  defp extract_items(list) when is_list(list), do: list

  defp extract_items(%{} = si) do
    case Map.get(si, :active_cells, []) do
      list when is_list(list) -> list
      other ->
        Logger.warning("Brain.extract_items: :active_cells not a list (got #{inspect(other)})")
        []
    end
  end

  @doc false
  @spec ensure_start_and_cast(Row.t(), map()) :: :ok
  defp ensure_start_and_cast(%Row{id: id} = row, payload) do
    ensure_started(id, row)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  @doc false
  @spec ensure_start_and_cast(id(), map()) :: :ok
  defp ensure_start_and_cast(id, payload) when is_binary(id) do
    ensure_started(id, id)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  @doc false
  @spec ensure_started(id(), Row.t() | id()) :: :ok
  defp ensure_started(id, arg) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [] ->
        case DynamicSupervisor.start_child(@cell_sup, {Brain.Cell, arg}) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
          other -> Logger.warning("Brain: start_child(#{id}) -> #{inspect(other)}")
        end

      _ -> :ok
    end
  end
@doc false
defp maybe_rescale_signals(%{choices: choices, boosts: b, inhibitions: i}, opts) do
  case Keyword.get(opts, :delta_model, :fixed) do
    :fixed ->
      {b, i}

    :margin_scaled ->
      # Base deltas (feel free to tune)
      base_boost = Keyword.get(opts, :base_boost, 0.2)
      base_inhib = Keyword.get(opts, :base_inhib, 0.1)
      clamp = fn x -> x |> min(0.5) |> max(-0.5) end

      # Build maps: winner margin & runner-ups
      by_token = Enum.group_by(choices, & &1.token_index)
      # winners (chosen_id) scaled by margin (+ tiny epsilon)
      boosts2 =
        for %{} = ch <- choices do
          delta = clamp.(base_boost * max(ch.margin, 0.05))
          {ch.chosen_id, delta}
        end

      # inhibitions: alt_ids scaled inversely to margin
      inhib2 =
        choices
        |> Enum.flat_map(fn ch ->
          for aid <- ch.alt_ids do
            delta = clamp.(-base_inhib * max(0.2, 1.0 - ch.margin))
            {aid, delta}
          end
        end)

      {boosts2, inhib2}
  end
end


end
