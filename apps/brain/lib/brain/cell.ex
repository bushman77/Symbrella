# apps/brain/lib/brain/cell.ex
defmodule Brain.Cell do
  @moduledoc """
  One cell process. Registers under Brain.Registry by its id.
  Accepts `{:activate, %{delta: int}}` casts and reports back to Brain.

  ## ID policy (guarding against DB leaks)

  Default (strict, prod):
    • IDs must be shaped like: `"word or phrase|pos(|suffix)?"`

  Test/dev escape hatch (explicit):
    • When `:brain, :allow_test_ids` is true OR Mix.env() == :test,
      allow *external* IDs that do not follow lexicon shape, e.g. `"THIS/strong"`.

  On invalid IDs, the process is **not started**; we raise and emit telemetry
  `[:brain, :cell, :id_rejected]` with %{id, reason}.
  """

  use GenServer
  require Logger
  alias Db.BrainCell, as: Row

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle assistant
  )

  # Lexicon-shaped IDs:
  #   "<phrase>|<pos>" or "<phrase>|<pos>|<suffix>"
  @id_regex ~r/^[^|]+?\|(noun|verb|adjective|adverb|interjection|phrase|proper_noun|pronoun|determiner|preposition|conjunction|numeral|particle|assistant)(\|[A-Za-z0-9_]+)?$/u
  @basic_id_regex ~r/^[^|]+(\|[^|]+){1,2}$/u

  # External IDs (test/dev only):
  #   e.g. "THIS/strong" (no pipes, slash allowed, no whitespace)
  @external_id_regex ~r/^[A-Za-z0-9._\-\/:]+$/u

  @doc """
  Start a `Brain.Cell`.

  Accepts either a `%Db.BrainCell{}` row or a binary id.

  Validation is strict by default to prevent placeholder/unknown cells from
  entering the system, but can be relaxed in test via `:brain, :allow_test_ids`
  (or automatically under Mix.env() == :test).
  """
  @spec start_link(Row.t() | binary()) :: GenServer.on_start()
  def start_link(arg) do
    id =
      arg
      |> extract_id()
      |> normalize_id()

    validate_id!(id)

    GenServer.start_link(__MODULE__, %{id: id, data: arg, activation: 0}, name: Brain.via(id))
  end

  @doc "True if an ID is acceptable for a Brain cell under the current policy."
  @spec valid_id?(term) :: boolean()
  def valid_id?(id) do
    try do
      id
      |> to_string()
      |> normalize_id()
      |> then(fn s ->
        validate_id!(s)
        true
      end)
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

  defp normalize_id(id) when is_binary(id) do
    id
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize_id(other), do: other |> to_string() |> normalize_id()

  # ──────────────────────── Validation ───────────────────────

  defp validate_id!(id) when is_binary(id) do
    id = normalize_id(id)

    case classify_id(id) do
      :lex ->
        validate_lex_id!(id)

      :external ->
        validate_external_id!(id)

      {:error, reason} ->
        return_reject!(id, reason)
    end
  end

  defp validate_id!(other), do: return_reject!(inspect(other), :non_binary_id)

  defp classify_id(id) when is_binary(id) do
    allow_external? = allow_external_ids?()

    cond do
      String.contains?(id, "|") ->
        :lex

      allow_external? ->
        # external ids must NOT contain "|" (keeps namespaces distinct)
        :external

      true ->
        {:error, :bad_shape}
    end
  end

  defp validate_lex_id!(id) when is_binary(id) do
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

    unless Regex.match?(@basic_id_regex, id) do
      return_reject!(id, :basic_regex_mismatch)
    end

    unless Regex.match?(@id_regex, id) do
      return_reject!(id, :regex_mismatch)
    end

    id
  end

  defp validate_external_id!(id) when is_binary(id) do
    # External policy:
    # - printable
    # - no whitespace (normalize_id already compresses, but we disallow spaces)
    # - limited charset
    # - recommended: include "/" (but not required if allow_external_ids? is enabled)
    cond do
      id == "" ->
        return_reject!(id, :empty_external_id)

      String.contains?(id, " ") ->
        return_reject!(id, :external_has_space)

      String.length(id) > 255 ->
        return_reject!(id, :external_too_long)

      not String.printable?(id) ->
        return_reject!(id, :external_not_printable)

      String.contains?(id, "|") ->
        return_reject!(id, :external_contains_pipe)

      not Regex.match?(@external_id_regex, id) ->
        return_reject!(id, :external_regex_mismatch)

      true ->
        id
    end
  end

  defp allow_external_ids? do
    Application.get_env(:brain, :allow_test_ids, false) or test_env?()
  end

  defp test_env? do
    Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env() == :test
  rescue
    _ -> false
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
      {n, ""} ->
        n

      _ ->
        case Float.parse(x) do
          {f, ""} -> f
          _ -> 0
        end
    end
  end

  defp coerce_number(_x, default), do: default
end

