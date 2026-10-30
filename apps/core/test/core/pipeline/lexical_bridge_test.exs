defmodule Core.Pipeline.LexicalBridgeTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.LexicalBridge

  @canoe [
    %{id: "canoe|noun|1", norm: "canoe", definition: "A narrow boat propelled by paddles."},
    %{id: "canoe|noun|2", norm: "canoe", definition: "A large luxury car."}
  ]

  @boat [
    %{id: "boat|noun|1", norm: "boat", definition: "A craft used on or in the water."},
    %{id: "boat|noun|9", norm: "boat", definition: "Acronym of best of all time."}
  ]

  @watercourse [
    %{id: "watercourse|noun|1", norm: "watercourse",
      definition: "A channel through which water flows."}
  ]

  @bank [
    %{id: "bank|noun|10", norm: "bank",
      definition: "An edge of river, lake, or other watercourse."},
    %{id: "bank|noun|1", norm: "bank",
      definition: "An institution where one can borrow money."}
  ]

  @loan [
    %{id: "loan|noun|2", norm: "loan",
      definition: "A sum of money that a person borrows and later repays."},
    %{id: "loan|noun|5", norm: "loan",
      definition: "An area of uncultivated ground near a village."}
  ]

  defp lookup(sets, notify \\ nil) do
    fn term ->
      if is_pid(notify), do: send(notify, {:dictionary_query, term})
      {:ok, Map.get(sets, term, [])}
    end
  end

  test "financial loan and bank share money with exact supporting sense IDs" do
    sets = %{"loan" => @loan, "bank" => @bank}
    assert {:ok, report} =
             LexicalBridge.audit("loan", "bank", lookup: lookup(sets))

    assert report.mode == :observation_only
    assert report.status == :candidate_bridges
    assert report.verified_intended_sense? == false
    assert report.changes_lifg? == false

    assert Enum.any?(report.candidates, fn candidate ->
             candidate.kind == :shared_definition_term and
               candidate.witness == "money" and
               candidate.source_sense_id == "loan|noun|2" and
               candidate.target_sense_id == "bank|noun|1"
           end)

    refute Enum.any?(report.candidates, &(&1.source_sense_id == "loan|noun|5"))
  end

  test "canoe and bank bridge only through attested boat and watercourse senses" do
    sets = %{
      "canoe" => @canoe, "boat" => @boat,
      "watercourse" => @watercourse, "bank" => @bank
    }

    assert {:ok, report} =
             LexicalBridge.audit("canoe", "bank",
               via: ["boat", "watercourse"], lookup: lookup(sets))

    assert report.status == :candidate_bridges
    assert report.via == ["boat", "watercourse"]
    assert report.senses_by_term ==
             %{"canoe" => 2, "boat" => 2, "watercourse" => 1, "bank" => 2}

    assert Enum.any?(report.candidates, fn candidate ->
             candidate.kind == :two_intermediate_gloss_bridge and
               candidate.source_sense_id == "canoe|noun|1" and
               candidate.target_sense_id == "bank|noun|10" and
               candidate.via == [
                 %{lemma: "boat", sense_id: "boat|noun|1"},
                 %{lemma: "watercourse", sense_id: "watercourse|noun|1"}
               ] and
               candidate.witnesses == %{
                 source_definition_mentions: "boat",
                 intermediate_definitions_share: "water",
                 target_definition_mentions: "watercourse"
               }
           end)

    refute Enum.any?(report.candidates, &(&1.source_sense_id == "canoe|noun|2"))
    refute Enum.any?(report.candidates, &(&1.target_sense_id == "bank|noun|1"))
    refute Enum.any?(report.candidates, fn candidate ->
      Enum.any?(candidate.via, &(&1.sense_id == "boat|noun|9"))
    end)
  end

  test "no intermediary gloss intersection abstains instead of inventing a link" do
    sets = %{
      "canoe" => @canoe,
      "boat" => [%{id: "boat|noun|1", norm: "boat", definition: "A mechanical vessel."}],
      "watercourse" => @watercourse,
      "bank" => @bank
    }

    assert {:ok, report} =
             LexicalBridge.audit("canoe", "bank",
               via: ["boat", "watercourse"], lookup: lookup(sets))

    assert report.status == :unresolved
    assert report.candidates == []
    assert report.verified_intended_sense? == false
  end

  test "only explicit terms are retrieved and lookup errors are not masked" do
    sets = %{
      "canoe" => @canoe, "boat" => @boat,
      "watercourse" => @watercourse, "bank" => @bank
    }
    assert {:ok, _} =
             LexicalBridge.audit("canoe", "bank",
               via: ["boat", "watercourse"], lookup: lookup(sets, self()))

    assert_receive {:dictionary_query, "canoe"}
    assert_receive {:dictionary_query, "boat"}
    assert_receive {:dictionary_query, "watercourse"}
    assert_receive {:dictionary_query, "bank"}
    refute_receive {:dictionary_query, _}

    failing = fn
      "boat" -> {:error, :ltm_query_failed}
      term -> {:ok, Map.get(sets, term, [])}
    end

    assert {:error, :ltm_query_failed} =
             LexicalBridge.audit("canoe", "bank",
               via: ["boat", "watercourse"], lookup: failing)
  end

  test "bounds requests without silently truncating dictionary evidence" do
    assert {:error, :invalid_intermediates} =
             LexicalBridge.audit("canoe", "bank", via: ["boat"])
    assert {:error, :duplicate_terms} =
             LexicalBridge.audit("canoe", "bank", via: ["boat", "CANOE"])
    assert {:error, :invalid_term} =
             LexicalBridge.audit("canoe", "bank; DROP TABLE", lookup: fn _ -> flunk("no query") end)

    too_many = for n <- 1..193 do
      %{id: "bank|noun|#{n}", norm: "bank", definition: "Meaning #{n}"}
    end

    assert {:error, :sense_limit_exceeded} =
             LexicalBridge.audit("loan", "bank",
               lookup: lookup(%{"loan" => @loan, "bank" => too_many}))
  end

  test "provenance audit labels shared usage qualifiers without discarding the match" do
    sets = %{
      "canoe" => [
        %{id: "canoe|noun|2", norm: "canoe", definition: "(slang) A luxury car."}
      ],
      "bank" => [
        %{id: "bank|noun|6", norm: "bank", definition: "(slang, uncountable) Money; profit."}
      ]
    }

    assert {:ok, report} =
             LexicalBridge.audit("canoe", "bank", lookup: lookup(sets))

    assert report.candidate_count == 1
    assert report.evidence_quality_counts == %{includes_leading_qualifier: 1}

    assert [%{
             source_sense_id: "canoe|noun|2",
             target_sense_id: "bank|noun|6",
             witness: "slang",
             evidence_quality: :includes_leading_qualifier,
             evidence_audit: %{
               source_region: :leading_qualifier,
               target_region: :leading_qualifier,
               source_senses_with_witness: 1,
               target_senses_with_witness: 1,
               comparison_scope: :retrieved_senses_only,
               semantic_relevance: :not_measured
             }
           }] = report.candidates

    refute report.verified_intended_sense?
    refute report.changes_lifg?
  end

  test "ordinary shared definition terms remain unverified including false positives" do
    sets = %{
      "loan" => [
        %{id: "loan|noun|2", norm: "loan", definition: "A natural person borrows money."},
        %{id: "loan|noun|5", norm: "loan", definition: "An area of uncultivated ground."}
      ],
      "watercourse" => [
        %{id: "watercourse|noun|1", norm: "watercourse",
          definition: "Any natural or artificial channel through which water flows."}
      ]
    }

    assert {:ok, report} =
             LexicalBridge.audit("loan", "watercourse", lookup: lookup(sets))

    assert report.candidate_count == 1
    assert report.evidence_quality_counts == %{definition_body_overlap_unverified: 1}

    assert [%{
             witness: "natural",
             evidence_quality: :definition_body_overlap_unverified,
             evidence_audit: %{
               source_region: :definition_body,
               target_region: :definition_body,
               semantic_relevance: :not_measured
             }
           }] = report.candidates
  end

  test "multi-hop bridge has source-specific evidence locations but no truth claim" do
    sets = %{
      "canoe" => @canoe, "boat" => @boat,
      "watercourse" => @watercourse, "bank" => @bank
    }

    assert {:ok, report} =
             LexicalBridge.audit("canoe", "bank",
               via: ["boat", "watercourse"], lookup: lookup(sets))

    assert report.evidence_quality_counts == %{multi_hop_gloss_overlap_unverified: 1}
    assert [%{
             evidence_quality: :multi_hop_gloss_overlap_unverified,
             evidence_audit: %{
               source_lemma_region: :definition_body,
               first_intermediate_region: :definition_body,
               second_intermediate_region: :definition_body,
               target_lemma_region: :definition_body,
               semantic_relevance: :not_measured
             }
           }] = report.candidates
    refute report.verified_intended_sense?
  end


  test "one-sided finance metadata cannot negate a loan-bank definition bridge" do
    source = %{
      id: "loan|noun|2", norm: "loan",
      definition: "A sum of borrowed money.",
      semantic_atoms: ["ety:1", "cat:en:finance", "cat:en:law", "pos_raw:noun"]
    }

    target = %{
      id: "bank|noun|1", norm: "bank",
      definition: "An institution that lends money.",
      semantic_atoms: ["ety:1", "tag:countable", "pos_raw:noun"]
    }

    assert {:ok, report} =
             LexicalBridge.audit("loan", "bank",
               lookup: lookup(%{"loan" => [source], "bank" => [target]}))

    assert report.candidate_count == 1
    assert report.category_evidence_counts == %{one_sided_category: 1}
    assert [%{witness: "money", category_evidence: categories}] = report.candidates
    assert categories.status == :one_sided_category
    assert categories.source_categories == ["finance", "law"]
    assert categories.target_categories == []
    assert categories.shared_categories == []
    refute categories.semantic_compatibility_proven?
    refute categories.semantic_incompatibility_proven?
    refute report.changes_lifg?
  end

  test "matching dictionary categories are positive metadata only, not sense proof" do
    source = %{
      id: "loan|noun|2", norm: "loan",
      definition: "Borrowing money.",
      semantic_atoms: ["cat:en:finance", "ety:1", "tag:slang", "pos_raw:noun"]
    }

    target = %{
      id: "bank|noun|1", norm: "bank",
      definition: "A place that lends money.",
      semantic_atoms: ["cat:en:finance", "ety:2", "tag:slang", "pos_raw:noun"]
    }

    assert {:ok, report} =
             LexicalBridge.audit("loan", "bank",
               lookup: lookup(%{"loan" => [source], "bank" => [target]}))

    assert report.category_evidence_counts == %{shared_category: 1}
    assert [%{category_evidence: categories}] = report.candidates
    assert categories.shared_categories == ["finance"]
    refute categories.semantic_compatibility_proven?
    refute report.verified_intended_sense?
  end

  test "disjoint categories and shared etymology cannot become contradictions" do
    source = %{
      id: "loan|noun|2", norm: "loan", definition: "Money borrowed.",
      semantic_atoms: ["cat:en:finance", "ety:1", "pos_raw:noun"]
    }

    target = %{
      id: "bank|noun|11", norm: "bank", definition: "A reserve of money.",
      semantic_atoms: ["cat:en:hydrology", "ety:1", "pos_raw:noun"]
    }

    assert {:ok, report} =
             LexicalBridge.audit("loan", "bank",
               lookup: lookup(%{"loan" => [source], "bank" => [target]}))

    assert report.category_evidence_counts == %{disjoint_category_labels_unverified: 1}
    assert [%{category_evidence: categories}] = report.candidates
    assert categories.source_categories == ["finance"]
    assert categories.target_categories == ["hydrology"]
    assert categories.shared_categories == []
    refute categories.semantic_incompatibility_proven?
    refute report.changes_lifg?
  end

end
