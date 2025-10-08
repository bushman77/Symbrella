# lib/brain/attention.ex
defmodule Brain.Attention do
  @moduledoc """
  Pure helpers to score candidate salience from attention context.

  Expected `ctx` shape (open):
    %{goal_terms: [<<>>], boosts: %{source_atom => weight}, words: MapSet.t(), ...}
  """

  @spec salience(map(), map()) :: float()
  def salience(%{} = cand, %{} = ctx) do
    words = Map.get(ctx, :goal_terms, []) |> MapSet.new()

    w =
      cond do
        words == MapSet.new() -> 0.0
        match_any?(cand, words) -> 1.0
        true -> 0.0
      end

    src   = Map.get(cand, :source)
    bonus = get_in(ctx, [:boosts, src]) || 0.0
    min(max(w + bonus, 0.0), 1.0)
  end

  defp match_any?(cand, words) do
    [cand[:id], cand[:lemma], cand[:word], cand[:phrase]]
    |> Enum.flat_map(&explode/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.any?(&MapSet.member?(words, &1))
  end

  defp explode(nil), do: []
  defp explode(t) when is_tuple(t), do: [inspect(t)]
  defp explode(b) when is_binary(b), do: [b]
  defp explode(_), do: []
end

