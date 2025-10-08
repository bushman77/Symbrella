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
    |> Enum.reject(&chargram?/1)
    |> Enum.filter(fn t ->
      mw? = truthy(Map.get(t, :mw) || Map.get(t, "mw"))

      case {Map.get(t, :span) || Map.get(t, "span"), sentence} do
        {{s, l}, snt} when is_integer(s) and is_integer(l) and l > 0 and is_binary(snt) ->
          boundary_aligned?(snt, s, l) or mw?
        _ ->
          true
      end
    end)
    |> sort_by_span_if_present()
  end

  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m), do: m
  defp mapify(other), do: %{phrase: to_string(other)}

  defp chargram?(t) when is_map(t) do
    (Map.get(t, :chargram) in [true, "true"]) or
      (Map.get(t, :kind) in [:chargram, "chargram"]) or
      (Map.get(t, :source) in [:chargram, "chargram"])
  end
  defp chargram?(_), do: false

  defp sort_by_span_if_present(list) when is_list(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, &elem(Map.fetch!(&1, :span), 0))
    else
      list
    end
  end

  defp valid_span?(%{span: {s, l}}) when is_integer(s) and is_integer(l) and l > 0, do: true
  defp valid_span?(_), do: false

  defp boundary_aligned?(snt, s, l) when is_binary(snt) and is_integer(s) and is_integer(l) do
    left = s - 1
    right = s + l
    left_ok = s == 0 or not word_char?(String.slice(snt, left, 1))
    right_ok = right >= String.length(snt) or not word_char?(String.slice(snt, right, 1))
    left_ok and right_ok
  end
  defp boundary_aligned?(_snt, _s, _l), do: false

  defp word_char?(""), do: false
  defp word_char?(ch), do: Regex.match?(~r/^[\p{L}\p{N}]$/u, ch)

  defp truthy(true), do: true
  defp truthy("true"), do: true
  defp truthy(_), do: false
end

