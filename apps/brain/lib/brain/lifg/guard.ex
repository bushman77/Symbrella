defmodule Brain.LIFG.Guard do
  @moduledoc """
  Compatibility shim for Core.LIFG.Input.
  Ensures maps, index, and optional span-sort.
  """

  @spec sanitize(list()) :: list()
  def sanitize(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(&mapify/1)
    |> ensure_indexed()
    |> sort_by_span_if_present()
  end

  defp mapify(t) when is_map(t), do: t
  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(other), do: %{phrase: to_string(other)}

  defp ensure_indexed(list) when is_list(list) do
    list
    |> Enum.with_index()
    |> Enum.map(fn {t, i} ->
      Map.put_new(t, :index, Map.get(t, :index) || Map.get(t, "index") || i)
    end)
  end

  defp sort_by_span_if_present(list) when is_list(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, &elem(Map.fetch!(&1, :span), 0))
    else
      list
    end
  end

  defp valid_span?(%{span: {s, l}}) when is_integer(s) and is_integer(l) and l > 0, do: true
  defp valid_span?(_), do: false
end

