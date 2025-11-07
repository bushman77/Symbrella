defmodule Brain.LIFG.Cover do
  @moduledoc """
  Greedy non-overlapping cover resolver for LIFG choices.

  Prefers longer spans, then higher margin, then leftmost.
  """

  alias Brain.Utils.Safe

  @type choice :: %{
          token_index: non_neg_integer(),
          chosen_id: any(),
          margin: number()
        }

  @spec resolve_cover(map(), list(choice), keyword()) :: %{si: map(), cover: list(map())}
  def resolve_cover(si, choices, _opts \\ []) do
    si0 = Safe.to_plain(si)
    tokens = Safe.get(si0, :tokens, []) |> Enum.map(&Safe.to_plain/1)

    ranked =
      choices
      |> Enum.map(fn ch ->
        tok = Enum.at(tokens, ch.token_index) || %{}
        span = Safe.get(tok, :span, {0, 0})
        {i, j} = span
        len = max(j - i, 0)
        {ch, span, len}
      end)
      |> Enum.sort_by(fn {ch, _span, len} -> {-len, -ch.margin, ch.token_index} end)

    {cover, _taken} =
      Enum.reduce(ranked, {[], []}, fn {ch, span, _len}, {keep, taken} ->
        if overlaps_any?(span, taken) do
          {keep, taken}
        else
          #{_i, _j} = span
          {[
             %{token_index: ch.token_index, span: span, id: ch.chosen_id, margin: ch.margin}
             | keep
           ], [span | taken]}
        end
      end)

    %{si: si0, cover: Enum.reverse(cover)}
  end

  defp overlaps_any?({_i1, _j1} = s, spans),
    do: Enum.any?(spans, &overlap?(s, &1))

  defp overlap?({i1, j1}, {i2, j2}) do
    # Closed-open intervals: [i, j)
    not (j1 <= i2 or j2 <= i1)
  end
end

