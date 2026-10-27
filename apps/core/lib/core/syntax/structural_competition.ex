defmodule Core.Syntax.StructuralCompetition do
  @moduledoc """
  Read-only bridge from Core's independently detected sentence signatures
  into Brain.LIFG.StructuralCompetition.

  Revalidate candidates against the original Core.Syntax detector before
  publishing any linguistic preference. The output remains provisional and
  never changes word-sense selections, event memory or response authorization.
  """

  alias Core.Pipeline.Trace

  @event [:core, :lifg, :structural_competition]

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    # Prevent a foreign or stale sentence-signature entry from becoming an
    # LIFG adjudication without Core's own exact evidence.
    independently_detected = Core.Syntax.Signatures.detect(si)

    valid =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.filter(&(&1 in independently_detected))

    result =
      si
      |> Map.put(:sentence_signatures, valid)
      |> Brain.LIFG.StructuralCompetition.resolve()

    updated =
      si
      |> Map.put(:structural_competition, result)
      |> Trace.append(:structural_competition,
        decision: result.status,
        reason: :complete_sentence_signature_competition,
        meta: %{
          source: result.source,
          preferred: preferred_summary(result.preferred),
          candidates: Enum.map(result.candidates, &candidate_summary/1),
          discrimination_margin: result.discrimination_margin,
          factual_authority?: false
        }
      )

    Core.Telemetry.emit(@event,
      %{candidate_count: length(result.candidates)},
      %{
        status: result.status,
        frame_run_id: Map.get(si, :frame_run_id),
        preferred: preferred_summary(result.preferred),
        candidates: Enum.map(result.candidates, &candidate_summary/1),
        factual_authority?: false
      }
    )

    updated
  end

  def attach(other), do: other

  @doc """
  Return only an unchanged, independently revalidated structural outcome.
  An injected signature, altered LIFG selection or altered result fails closed.
  """
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    attached = Map.get(si, :structural_competition)
    signatures = Map.get(si, :sentence_signatures)

    if is_map(attached) and is_list(signatures) and
         Enum.all?(signatures, &(&1 in Core.Syntax.Signatures.detect(si))) do
      expected = Brain.LIFG.StructuralCompetition.resolve(si)

      if attached == expected and expected.status == :preferred and
           is_map(expected.preferred),
        do: {:ok, expected},
        else: :not_applicable
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp preferred_summary(nil), do: nil
  defp preferred_summary(candidate), do: candidate_summary(candidate)

  defp candidate_summary(candidate) do
    Map.take(candidate, [
      :construction, :span, :compatibility,
      :matched_roles, :conflicting_roles, :unresolved_roles
    ])
  end
end
