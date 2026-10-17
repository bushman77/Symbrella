defmodule Core.Cognition.AssociativeReasoning do
  @moduledoc """
  Hippocampus-led retrieval proposals for typed symbolic reasoning.

  This is a *cue-to-proof-index* boundary, not an inference engine. A pair of
  associated A–B and B–C records suggests a two-hop reasoning opportunity
  for A–C. The proposed IDs are never evidence and never enter a chat prompt.

  Core.Reasoning.PopulationChain must still read every relevant stored premise,
  re-derive all parent proofs, and reject every detected contradiction. A cue
  may prioritize a candidate but may not hide an un-cued conflicting record.
  """

  alias Brain.Hippocampus.SemanticAssociations
  alias Db.ConsolidatedConclusion

  @max_cues 12

  @type proposal ::
          %{kind: :direct_proof_pointer, fingerprint: String.t()} |
          %{kind: :two_hop_candidate, parent_proof_ids: [integer()]}

  @doc """
  Get one bounded retrieval proposal for public-place population reasoning.

  The result is an opaque candidate, not an authorized conclusion. This pure
  invocation boundary is also usable by a future internal curiosity worker:
  it never sends chat messages or invokes Qwen.
  """
  @spec propose([String.t()]) :: proposal() | nil
  def propose([a, c] = subjects) when is_binary(a) and is_binary(c) do
    if Application.get_env(:core, :semantic_associative_recall_enabled?, false) and
         distinct_subjects?(subjects) do
      rows =
        subjects
        |> SemanticAssociations.cues(@max_cues)
        |> Enum.flat_map(fn fingerprint ->
          case Db.get_by(ConsolidatedConclusion, fingerprint: fingerprint) do
            %ConsolidatedConclusion{
              kind: "population_comparison",
              provenance: "source_supported_symbolic_derivation"
            } = row -> [row]
            _ -> []
          end
        end)
        |> Enum.uniq_by(& &1.id)

      direct =
        Enum.find(rows, fn row ->
          pair_subjects(row) == Enum.sort(subjects)
        end)

      cond do
        direct != nil ->
          %{kind: :direct_proof_pointer, fingerprint: direct.fingerprint}

        true ->
          # A typed two-hop cue is only an association: the complete verified
          # graph and any competing evidence will be checked by PopulationChain.
          for left <- rows, right <- rows, left.id != right.id,
              middle <- [other_subject(left, a)], is_binary(middle),
              middle != c, middle != a,
              pair_subjects(right) == Enum.sort([middle, c]),
              left.reference_year == right.reference_year do
            %{kind: :two_hop_candidate,
              parent_proof_ids: Enum.sort([left.id, right.id])}
          end
          |> Enum.sort_by(& &1.parent_proof_ids)
          |> List.first()
      end
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  def propose(_), do: nil

  defp pair_subjects(%ConsolidatedConclusion{premises: [a, b]})
       when is_map(a) and is_map(b) do
    case {a["subject"], b["subject"]} do
      {x, y} when is_binary(x) and is_binary(y) -> Enum.sort([x, y])
      _ -> []
    end
  end

  defp pair_subjects(_), do: []

  defp other_subject(%ConsolidatedConclusion{premises: [a, b]}, subject)
       when is_map(a) and is_map(b) do
    case {a["subject"], b["subject"]} do
      {^subject, other} when is_binary(other) -> other
      {other, ^subject} when is_binary(other) -> other
      _ -> nil
    end
  end

  defp other_subject(_, _), do: nil

  defp distinct_subjects?([a, b]) do
    byte_size(String.trim(a)) in 3..100 and
      byte_size(String.trim(b)) in 3..100 and
      String.downcase(String.trim(a)) != String.downcase(String.trim(b))
  end
end
