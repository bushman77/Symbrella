defmodule Brain.Hippocampus.Evidence do
  @moduledoc "Helpers to ensure `slate.winners` exist for evidence/inspection."

  @spec ensure_winners_for_evidence(%{episode: %{slate: map()}} | map()) :: map()
  def ensure_winners_for_evidence(%{episode: %{slate: slate}} = rec) when is_map(slate) do
    winners =
      cond do
        is_list(Map.get(slate, :winners))  -> slate.winners
        is_list(Map.get(slate, "winners")) -> slate["winners"]
        is_list(Map.get(slate, :tokens))   -> slate.tokens   |> Enum.map(&token_to_winner/1) |> Enum.reject(&is_nil/1)
        is_list(Map.get(slate, "tokens"))  -> slate["tokens"] |> Enum.map(&token_to_winner/1) |> Enum.reject(&is_nil/1)
        true -> []
      end

    put_in(rec, [:episode, :slate, :winners], winners)
  end

  def ensure_winners_for_evidence(other), do: other

  defp token_to_winner(%{} = t) do
    cond do
      is_binary(t[:lemma])  -> %{lemma: t[:lemma]}
      is_binary(t["lemma"]) -> %{lemma: t["lemma"]}
      is_binary(t[:norm])   -> %{lemma: t[:norm]}
      is_binary(t["norm"])  -> %{lemma: t["norm"]}
      is_binary(t[:word])   -> %{lemma: t[:word]}
      is_binary(t["word"])  -> %{lemma: t["word"]}
      is_binary(t[:id])     -> parse_id_winner(t[:id])
      is_binary(t["id"])    -> parse_id_winner(t["id"])
      true -> nil
    end
  end

  defp token_to_winner(s) when is_binary(s), do: %{lemma: String.downcase(String.trim(s))}
  defp token_to_winner(_), do: nil

  defp parse_id_winner(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> %{lemma: String.downcase(w)}
      _       -> nil
    end
  end
end

