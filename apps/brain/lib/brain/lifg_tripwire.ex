defmodule Brain.LIFG.Tripwire do
  @moduledoc ~S"""
  Telemetry tripwire for LIFG: if any char-gram-like tokens (whitespace in `phrase`
  without `mw: true`) reach this point, emit telemetry and optionally drop/raise.

  Usage:
      tokens = Brain.LIFG.Tripwire.check_and_filter(tokens, on_leak: :drop)

  Telemetry:
    [:lifg, :chargram, :leak]
      measurements: %{count: non_neg_integer(), examples: [String.t()]}
      metadata:     %{on_leak: :drop | :keep | :raise}
  """

  require Logger

  @spec check_and_filter([map()], keyword()) :: [map()]
  def check_and_filter(tokens, opts \\ []) when is_list(tokens) do
    on_leak = Keyword.get(opts, :on_leak, :drop)

    {leaks, ok} = Enum.split_with(tokens, &chargram?/1)
    count = length(leaks)

    if count > 0 do
      examples =
        leaks
        |> Enum.take(3)
        |> Enum.map(&Map.get(&1, :phrase))

      :telemetry.execute([:lifg, :chargram, :leak], %{count: count, examples: examples}, %{
        on_leak: on_leak
      })

      Logger.warning(
        "LIFG tripwire: detected #{count} char-gram token(s): #{Enum.map_join(examples, ", ", &inspect/1)} (action=#{on_leak})"
      )

      case on_leak do
        :drop -> ok
        :keep -> tokens
        :raise -> raise ArgumentError, "char-gram leak detected into LIFG"
      end
    else
      tokens
    end
  end

  defp chargram?(%{phrase: phrase} = t) when is_binary(phrase) do
    spaced? = Regex.match?(~r/\s/u, phrase)
    mw? = Map.get(t, :mw, false) === true
    spaced? and not mw?
  end

  defp chargram?(_), do: false
end
