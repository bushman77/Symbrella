defmodule Core.Response.HippocampalSemanticAssociationTest do
  @moduledoc """
  Hippocampal associations cue existing evidence, never certify it.

  Tests use the real hippocampus GenServer and Core DB sandbox. The pointer
  is an independent episodic retrieval index, not a second knowledge store.
  """
  use ExUnit.Case, async: false

  alias Brain.Hippocampus.SemanticAssociations
  alias Core.Cognition.MemoryBoundary
  alias Core.Response.{
    AuthoritativeAnswerPlan, Consolidation, PopulationComparison, ResponseFrame
  }
  alias Db.ConsolidatedConclusions

  setup do
    old_association = Application.get_env(:core, :semantic_associative_recall_enabled?)
    old_consolidation = Application.get_env(:core, :symbolic_consolidation_enabled?)

    Application.put_env(:core, :semantic_associative_recall_enabled?, true)
    Application.put_env(:core, :symbolic_consolidation_enabled?, true)

    on_exit(fn ->
      restore(:semantic_associative_recall_enabled?, old_association)
      restore(:symbolic_consolidation_enabled?, old_consolidation)
    end)

    assert is_pid(Process.whereis(Brain.Hippocampus))
    uid = System.unique_integer([:positive])
    %{names: ["HippoAtlas#{uid}", "HippoHarbor#{uid}"]}
  end

  defp restore(key, nil), do: Application.delete_env(:core, key)
  defp restore(key, value), do: Application.put_env(:core, key, value)

  defp event(names) do
    %{type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified}
  end

  defp context(names, evidence? \\ false) do
    frame = event(names)

    initial = %{
      session_id: "hippocampal-new-session",
      user_text: "Which place has the larger population?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}, mood: %{}
    }

    if evidence? do
      [a, b] = names

      evidence = %{
        status: :retrieved, resolution: :source_supported,
        relation: :population, provider: :wikidata,
        support: %{
          kind: :population_group, status: :source_supported,
          subjects: names,
          results: [
            %{subject: a, value: 120_000, census_year: 2021,
              source_url: "https://www.wikidata.org/wiki/Q473209",
              source_type: :wikidata, status: :source_supported},
            %{subject: b, value: 100_000, census_year: 2021,
              source_url: "https://www.wikidata.org/wiki/Q16740",
              source_type: :wikidata, status: :source_supported}
          ]
        },
        sources: [], attempts: [], attempted_count: 2
      }

      initial
      |> Map.put(:web_evidence, evidence)
      |> PopulationComparison.attach()
      |> then(fn c -> Map.put(c, :response_frame, ResponseFrame.build(c)) end)
    else
      initial
    end
  end

  test "source-backed proof creates a private hippocampal cue that retrieves semantic knowledge", %{
    names: names
  } do
    original = context(names, true)
    plan = AuthoritativeAnswerPlan.build(original)
    assert %{kind: :verified_population_comparison} = plan

    assert {:ok, proof} = Consolidation.capture(original, plan)
    assert [proof.fingerprint | _] = SemanticAssociations.cues(names)

    recalled = MemoryBoundary.recall(context(Enum.reverse(names)))
    assert recalled.hippocampal_association.fingerprint == proof.fingerprint
    assert recalled.web_evidence.support.kind == :population_comparison
    assert recalled.web_evidence.support.difference == 20_000
    assert recalled.web_evidence.support.winner == hd(names)

    public = MemoryBoundary.for_expression(recalled)
    refute Map.has_key?(public, :hippocampal_association)
    assert public.web_evidence.support.difference == 20_000

    # An already stored conclusion is reused, not re-captured as new data.
    assert length(ConsolidatedConclusions.recent(proof.pair_key)) == 1
  end

  test "a fabricated pointer alone can never become a supported conclusion", %{names: names} do
    forged = String.duplicate("a", 64)
    assert :ok = SemanticAssociations.remember(forged, names)
    assert forged in SemanticAssociations.cues(names)

    recalled = MemoryBoundary.recall(context(names))
    refute Map.has_key?(recalled, :hippocampal_association)
    refute Map.has_key?(recalled, :web_evidence)
  end

  test "unrelated episodic associations do not authorize a different comparison", %{names: names} do
    original = context(names, true)
    {:ok, proof} = Consolidation.capture(original, AuthoritativeAnswerPlan.build(original))

    unrelated = ["HippoWestUnused", "HippoEastUnused"]
    refute proof.fingerprint in SemanticAssociations.cues(unrelated)

    recalled = MemoryBoundary.recall(context(unrelated))
    refute Map.has_key?(recalled, :hippocampal_association)
    refute Map.has_key?(recalled, :web_evidence)
  end

  test "an untagged conversational memory cannot impersonate a semantic pointer" do
    fake = String.duplicate("b", 64)
    results = [
      %{episode: %{meta: %{kind: "conversation", key: "population_comparison",
                            value: fake, tags: ["conversation"]}}},
      %{episode: %{meta: %{kind: "semantic_association",
                            key: "population_comparison",
                            value: "invalid", tags: ["semantic_association"]}}}
    ]

    assert SemanticAssociations.extract_fingerprints(results) == []
  end

  test "disabled hippocampal cueing preserves the legacy Core consolidation path", %{
    names: names
  } do
    original = context(names, true)
    assert {:ok, _proof} = Consolidation.capture(original, AuthoritativeAnswerPlan.build(original))

    Application.put_env(:core, :semantic_associative_recall_enabled?, false)
    recalled = MemoryBoundary.recall(context(names))

    assert recalled.web_evidence.support.kind == :population_comparison
    refute Map.has_key?(recalled, :hippocampal_association)
  end
end
