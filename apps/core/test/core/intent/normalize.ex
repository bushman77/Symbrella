defmodule Core.Intent.Normalize do
  @moduledoc """
  Normalize mixed input maps into a safe, canonical intent struct-like map:

      %{
        intent:    :ask | :tell | ... | :unknown,
        keyword:   String.t() | nil,
        confidence: float in 0.0..1.0 | nil
      }

  Design:
  • Accepts atom or string keys/values.
  • NEVER creates new atoms; only maps existing atoms from a whitelist.
  • Tolerant of junk input; returns :unknown/defaults instead of raising.
  """

  @type t :: %{
          intent: atom(),
          keyword: String.t() | nil,
          confidence: number() | nil
        }

  @allowed_intents ~w(
    ask tell affirm deny greet bye meta why help debug
  )a

  @doc "Normalize a user/system intent map."
  @spec normalize(map() | nil) :: t
  def normalize(map) when is_map(map) do
    %{
      intent: normalize_intent(get_any(map, ["intent", :intent])),
      keyword: normalize_string(get_any(map, ["keyword", :keyword])),
      confidence: normalize_conf(get_any(map, ["confidence", :confidence]))
    }
  end

  def normalize(_), do: %{intent: :unknown, keyword: nil, confidence: nil}

  # ---- internals

  defp get_any(map, keys), do: Enum.find_value(keys, &Map.get(map, &1))

  defp normalize_intent(v) when is_atom(v) and v in @allowed_intents, do: v

  defp normalize_intent(v) when is_binary(v) do
    v = String.trim(v) |> String.downcase()

    case safe_existing_atom(v) do
      a when a in @allowed_intents -> a
      _ -> :unknown
    end
  end

  defp normalize_intent(_), do: :unknown

  defp safe_existing_atom(bin) do
    try do
      String.to_existing_atom(bin)
    rescue
      ArgumentError -> :__no_atom__
    end
  end

  defp normalize_string(nil), do: nil
  defp normalize_string(v) when is_binary(v), do: String.trim(v)
  defp normalize_string(v) when is_atom(v), do: v |> Atom.to_string() |> String.trim()
  defp normalize_string(_), do: nil

  defp normalize_conf(nil), do: nil
  defp normalize_conf(v) when is_number(v), do: clamp(v, 0.0, 1.0)

  defp normalize_conf(v) when is_binary(v) do
    case Float.parse(v) do
      {n, _} -> clamp(n, 0.0, 1.0)
      :error -> nil
    end
  end

  defp normalize_conf(_), do: nil

  defp clamp(n, lo, hi) when n < lo, do: lo
  defp clamp(n, lo, hi) when n > hi, do: hi
  defp clamp(n, _lo, _hi), do: n
end

