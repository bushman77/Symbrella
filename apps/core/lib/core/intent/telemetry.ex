defmodule Core.Intent.Telemetry do
  @moduledoc """
  Intent telemetry helpers.
  Emits a single canonical event when an intent is selected.
  """

  @event [:core, :intent, :selected]

  @spec emit_selected(binary(), atom() | binary(), binary() | nil, number() | nil, atom()) :: :ok
  def emit_selected(text, label, keyword, confidence, source \\ :core) do
    # normalize label to atom if it came in as string
    label =
      cond do
        is_atom(label) ->
          label

        is_binary(label) ->
          try do
            String.to_existing_atom(label)
          rescue
            _ -> String.to_atom(label)
          end

        true ->
          :other
      end

    meas = %{confidence: normalize_confidence(confidence)}
    meta = %{intent: label, keyword: keyword || "", text: text || "", source: source}

    :telemetry.execute(@event, meas, meta)
  end

  defp normalize_confidence(nil), do: 0.0
  defp normalize_confidence(c) when is_integer(c), do: c * 1.0
  defp normalize_confidence(c) when is_float(c), do: c

  defp normalize_confidence(c) when is_binary(c) do
    case Float.parse(c) do
      {f, _} -> f
      _ -> 0.0
    end
  end
end
