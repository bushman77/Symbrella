defmodule Brain.LIFG.BoundaryGuard do
  @moduledoc """
  Minimal boundary + char-gram guard (keeps MWEs).
  """

  @type token :: map()

  @spec sanitize([token()]) :: [token()]
  def sanitize(tokens), do: sanitize(tokens, nil)

  @spec sanitize([token()], String.t() | nil) :: [token()]
  def sanitize(tokens, sentence) when is_list(tokens) do
    tokens
    |> Enum.map(&mapify/1)
    |> Enum.map(&normalize_span_shape/1)   # coerce spans to {start, end}
    |> Enum.reject(&chargram?/1)           # explicit chargram markers
    |> Enum.reject(&whitespace_non_mwe?/1) # NEW: any space w/o mw: true is dropped
    |> Enum.filter(fn t ->
      mw? = truthy(Map.get(t, :mw) || Map.get(t, "mw"))

      case {Map.get(t, :span) || Map.get(t, "span"), sentence} do
        {{s, e}, snt} when is_integer(s) and is_integer(e) and is_binary(snt) ->
          e > s and (boundary_aligned?(snt, s, e) or mw?)
        _ ->
          true
      end
    end)
    |> sort_by_span_if_present()
  end

  # ---- internals -------------------------------------------------------

  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m),  do: m
  defp mapify(other),    do: %{phrase: to_string(other)}

  # Accept both {start, end} and {start, len}; convert to {start, end}.
  defp normalize_span_shape(t) do
    case Map.get(t, :span) || Map.get(t, "span") do
      {s, x} when is_integer(s) and is_integer(x) ->
        span =
          if x <= s do
            phrase = Map.get(t, :phrase) || Map.get(t, "phrase") || ""
            len = max(String.length(to_string(phrase)), 1)
            {s, s + len}
          else
            {s, x}
          end

        Map.put(t, :span, span)

      _ ->
        Map.delete(t, :span)
    end
  end

  defp chargram?(t) when is_map(t) do
    Map.get(t, :chargram) in [true, "true"] or
      Map.get(t, :kind)   in [:chargram, "chargram"] or
      Map.get(t, :source) in [:chargram, "chargram"]
  end

  defp chargram?(_), do: false

  # Drop tokens whose phrase contains spaces unless explicitly marked MWE.
  defp whitespace_non_mwe?(t) do
    phrase = to_string(Map.get(t, :phrase) || Map.get(t, "phrase") || "")
    mw? = truthy(Map.get(t, :mw) || Map.get(t, "mw"))
    String.contains?(phrase, " ") and not mw?
  end

  defp sort_by_span_if_present(list) when is_list(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, fn %{span: {s, _e}} -> s end)
    else
      list
    end
  end

  defp valid_span?(%{span: {s, e}}) when is_integer(s) and is_integer(e) and s >= 0 and e > s, do: true
  defp valid_span?(_), do: false

  # `e` is exclusive end; boundary chars around [s, e)
  defp boundary_aligned?(snt, s, e) when is_binary(snt) do
    left  = s - 1
    right = e
    left_ok  = s == 0 or not word_char?(String.slice(snt, left, 1))
    right_ok = right >= String.length(snt) or not word_char?(String.slice(snt, right, 1))
    left_ok and right_ok
  end

  defp word_char?(""), do: false
  defp word_char?(ch), do: Regex.match?(~r/^[\p{L}\p{N}]$/u, ch)

  defp truthy(true), do: true
  defp truthy("true"), do: true
  defp truthy(_), do: false
end

