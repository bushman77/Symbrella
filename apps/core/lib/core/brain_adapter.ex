defmodule Core.BrainAdapter do
  @moduledoc """
  Runtime-only bridge to the Brain process (no compile-time deps).
  Talks to the registered process name (Elixir.Brain) via GenServer messages.

  Guardrails:
  - Core never touches Db directly; all meaning/synonym work routes through Brain.
  - This module is the single doorway for Core→Brain calls in P-201 scope (synonyms).
  """

  @brain :"Elixir.Brain"
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

  # --- Existing API (unchanged) ---

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

  # --- New API for P-201 ---

  @doc """
  Batch synonym retrieval via Brain. Accepts cell_ids, {lemma,pos} tuples,
  or MWEs as {:mwe, [tokens]}. Returns {:ok, %{key => [syn_obj]}} or {:error, reason}.

  This is a runtime call; if Brain isn't available, returns {:error, :unavailable}.
  """
  @spec synonyms_for_keys([key()], map() | keyword()) ::
          {:ok, syn_result()} | {:error, term()}
  def synonyms_for_keys(keys, opts \\ %{})
  def synonyms_for_keys([], _opts), do: {:ok, %{}}

  def synonyms_for_keys(keys, opts) when is_list(keys) do
    payload = %{keys: keys, opts: normalize_opts(opts)}
    safe_call({:synonyms_for_keys, payload}, @timeout)
  end

  @doc "Is the Brain process currently available?"
  @spec available?() :: boolean()
  def available?, do: is_pid(Process.whereis(@brain))

  # --- Internals ---

  @spec normalize_opts(map() | keyword() | term()) :: map()
  defp normalize_opts(opts) when is_list(opts), do: Map.new(opts)
  defp normalize_opts(%{} = opts), do: opts
  defp normalize_opts(_), do: %{}

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
