# apps/brain/lib/brain/cell.ex
defmodule Brain.Cell do
  @moduledoc """
  One cell process. Registers under Brain.Registry by its id.
  Accepts `{:activate, %{delta: int}}` casts and reports back to Brain.

  ## Invariants (guarding against DB leaks)
  • IDs must be shaped like: `"word or phrase|pos(|suffix)?"`
  • On invalid IDs, the process is **not started**; we raise and emit telemetry
    `[:brain, :cell, :id_rejected]` with %{id, reason}.
  """

  use GenServer
  require Logger
  alias Db.BrainCell, as: Row

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle assistant
  )

  @id_regex ~r/^[^|]+?\|(noun|verb|adjective|adverb|interjection|phrase|proper_noun|pronoun|determiner|preposition|conjunction|numeral|particle|assistant)(\|[A-Za-z0-9_]+)?$/u
  @basic_id_regex ~r/^[^|]+(\|[^|]+){1,2}$/u

  @doc """
  Start a `Brain.Cell`.

  Accepts either a `%Db.BrainCell{}` row or a binary id. Validates the id to
  prevent placeholder/unknown cells (e.g. `|unk|`, `|seed|`) from entering the system.
  """
  @spec start_link(Row.t() | binary()) :: GenServer.on_start()
  def start_link(arg) do
    id =
      arg
      |> extract_id()
      |> normalize_id()

    if bad_shape?(id) do
      return_reject!(id, :bad_shape)
    end

    id = validate_id!(id)

    GenServer.start_link(__MODULE__, %{id: id, data: arg, activation: 0}, name: Brain.via(id))
  end

  @doc "True if an ID is acceptable for a Brain cell (\"norm or phrase|pos|suffix?\")."
  @spec valid_id?(term) :: boolean()
  def valid_id?(id) do
    try do
      id
      |> to_string()
      |> normalize_id()
      |> then(fn s -> not bad_shape?(s) end)
      |> case do
        false -> false
        true ->
          validate_id!(normalize_id(to_string(id)))
          true
      end
    rescue
      _ -> false
    end
  end

  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call(:status, _from, %{id: id, activation: a} = st) do
    {:reply, {:ok, %{id: id, activation: a}}, st}
  end

  @impl true
  def handle_cast({:activate, payload}, %{id: id, activation: a} = st) do
    delta =
      payload
      |> Map.get(:delta, 1)
      |> coerce_number(1)

    a2 = a + delta
    GenServer.cast(Brain, {:activation_report, id, a2})
    {:noreply, %{st | activation: a2}}
  end

  # ──────────────────────── Helpers ─────────────────────────

  defp extract_id(%Row{id: id}), do: id
  defp extract_id(id) when is_binary(id), do: id
  defp extract_id(other), do: raise(ArgumentError, "Unsupported cell arg: #{inspect(other)}")

  defp normalize_id(id) do
    id
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp validate_id!(id) when is_binary(id) do
    segments = String.split(id, "|", trim: true)

    case segments do
      [_, _pos] -> :ok
      [_, _pos, _suffix] -> :ok
      _ -> return_reject!(id, :bad_shape_segments)
    end

    if Enum.any?(segments, &(&1 == "unk" or &1 == "seed")) do
      return_reject!(id, :placeholder_segment)
    end

    pos = Enum.at(segments, 1)

    unless pos in @allowed_pos do
      return_reject!(id, {:unknown_pos, pos})
    end

    unless Regex.match?(@id_regex, id) do
      return_reject!(id, :regex_mismatch)
    end

    id
  end

  defp return_reject!(id, reason) do
    :telemetry.execute([:brain, :cell, :id_rejected], %{count: 1}, %{id: id, reason: reason})

    Logger.warning("Brain.Cell id rejected",
      id: id,
      reason: inspect(reason)
    )

    raise ArgumentError,
          "Brain.Cell refused to start for id=#{inspect(id)} (reason=#{inspect(reason)})"
  end

  defp coerce_number(x, _default) when is_integer(x) or is_float(x), do: x

  defp coerce_number(x, _default) when is_binary(x) do
    case Integer.parse(x) do
      {n, ""} -> n
      _ ->
        case Float.parse(x) do
          {f, ""} -> f
          _ -> 0
        end
    end
  end

  defp coerce_number(_x, default), do: default

  # Test-only relaxation: allow "/" in IDs during Mix.env() == :test (or via explicit config).
  defp bad_shape?(id) when is_binary(id) do
    allow =
      Application.get_env(:brain, :allow_test_ids, false) or
        (Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env() == :test)

    cond do
      String.contains?(id, "/") and not allow ->
        true

      not Regex.match?(@basic_id_regex, id) ->
        true

      true ->
        false
    end
  end

  defp bad_shape?(_), do: true
end

