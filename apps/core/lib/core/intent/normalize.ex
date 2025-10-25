defmodule Core.Intent.Normalize do
  @moduledoc """
  Normalize mixed input maps into a safe, canonical intent map:

      %{
        intent:    :ask | :tell | :affirm | :deny | :greet | :bye | :meta | :why | :help | :debug | :unknown,
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

  # Common human-friendly aliases; all values are existing atoms (no new atom creation).
  @alias_map %{
    "hi" => :greet,
    "hello" => :greet,
    "hey" => :greet,
    "goodbye" => :bye,
    "bye" => :bye,
    "yes" => :affirm,
    "y" => :affirm,
    "no" => :deny,
    "n" => :deny,
    "question" => :ask,
    "ask" => :ask,
    "help" => :help,
    "why" => :why,
    "debug" => :debug
  }

  @doc "Normalize a user/system intent map."
  @spec normalize(map() | any()) :: t
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

  # Guard only checks type; validity is done inside (guards can't call Core.Intent.valid?/1).
  defp normalize_intent(v) when is_atom(v) do
    if Core.Intent.valid?(v), do: v, else: :unknown
  end

  defp normalize_intent(v) when is_binary(v) do
    v =
      v
      |> String.trim()
      |> strip_edge_punct()
      |> String.downcase()

    alias_intent = Map.get(@alias_map, v)

    cond do
      is_atom(alias_intent) and Core.Intent.valid?(alias_intent) ->
        alias_intent

      true ->
        case safe_existing_atom(v) do
          a when is_atom(a) ->
            if Core.Intent.valid?(a), do: a, else: :unknown

          _ ->
            :unknown
        end
    end
  end

  defp normalize_intent(_), do: :unknown

  # Remove runs of punctuation/whitespace at both edges (e.g., "?? Ask!! " -> "Ask").
  defp strip_edge_punct(s) when is_binary(s) do
    # \p{P} = punctuation, \p{Z} = separators (spaces, NBSP, etc.)
    String.replace(s, ~r/^[\p{P}\p{Z}]+|[\p{P}\p{Z}]+$/u, "")
  end

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
    s = String.trim(v)

    cond do
      String.ends_with?(s, "%") ->
        base = String.trim_trailing(s, "%")
        case Float.parse(base) do
          {n, _} -> clamp(n / 100.0, 0.0, 1.0)
          :error -> nil
        end

      true ->
        case Float.parse(s) do
          {n, _} -> clamp(n, 0.0, 1.0)
          :error -> nil
        end
    end
  end

  defp normalize_conf(_), do: nil

  defp clamp(n, lo, hi) when n < lo, do: lo
  defp clamp(n, lo, hi) when n > hi, do: hi
  defp clamp(n, _lo, _hi), do: n
end

