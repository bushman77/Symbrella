defmodule Core.Semantic.RelationalEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.RelationalEvidence

  @past_evidence [:contiguous_surface, :attested_nominal_subject,
                  :attested_nominal_object, :attested_simple_past_verb]

  defp si(words, signature \\ :finite_past_svo_candidate) do
    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, index} -> %{index: index, phrase: word, n: 1} end)

    rows =
      for {word, index, pos} <- [
            {Enum.at(words, 1), 1, "noun"},
            {Enum.at(words, 2), 2, "verb"},
            {Enum.at(words, 4), 4, "noun"}
          ] do
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos, token_index: index}
      end

    %{
      sentence: Enum.join(words, " "),
      tokens: tokens,
      active_cells: rows,
      sentence_signatures: [
        %{
          id: signature, source: :core_syntax, span: {0, 4},
          roles: %{subject_head: 1, predicate: 2, object_head: 4},
          evidence: @past_evidence
        }
      ],
      lifg_choices: [%{token_index: 1, id: "original|noun|1"}],
      sense_candidates: %{1 => [%{id: "original|noun|1"}]}
    }
  end

  defp lookup(dict, receiver \\ nil) do
    fn word ->
      if is_pid(receiver), do: send(receiver, {:lookup, word})
      {:ok, Map.get(dict, word, [])}
    end
  end

  @bank [
    %{id: "bank|noun|1", norm: "bank",
      definition: "An institution for borrowing money.",
      semantic_atoms: ["pos_raw:noun"]},
    %{id: "bank|noun|10", norm: "bank",
      definition: "An edge of a watercourse.",
      semantic_atoms: ["cat:en:hydrology"]},
    %{id: "bank|noun|6", norm: "bank",
      definition: "(slang) Money; profit.",
      semantic_atoms: ["tag:slang"]}
  ]

  @loan [
    %{id: "loan|noun|2", norm: "loan",
      definition: "An amount of borrowed money repaid later.",
      semantic_atoms: ["cat:en:finance"]},
    %{id: "loan|noun|5", norm: "loan",
      definition: "A meadow near a village.",
      semantic_atoms: []}
  ]

  test "attested SVO subject and object gate a loan-bank sense report" do
    original = si(["The", "bank", "granted", "a", "loan"])
    report =
      RelationalEvidence.audit(original,
        lookup: lookup(%{"bank" => @bank, "loan" => @loan}, self()))

    assert report.status == :candidate_relational_evidence
    assert report.evaluated_frames == 1
    assert [%{signature_id: :finite_past_svo_candidate} = frame] = report.frames
    assert frame.subject.word == "bank"
    assert frame.object.word == "loan"
    assert frame.predicate.word == "granted"
    assert frame.predicate.semantic_role == :grammatical_predicate_only

    assert Enum.any?(frame.dictionary_bridge.candidates, fn c ->
      c.source_sense_id == "bank|noun|1" and
        c.target_sense_id == "loan|noun|2" and c.witness == "money"
    end)

    assert_receive {:lookup, "bank"}
    assert_receive {:lookup, "loan"}
    refute_receive {:lookup, _}

    assert original.lifg_choices == [%{token_index: 1, id: "original|noun|1"}]
    assert original.sense_candidates == %{1 => [%{id: "original|noun|1"}]}
    refute report.intended_sense_verified?
    refute report.predicate_meaning_verified?
    refute report.semantic_relation_verified?
    refute report.modified_lifg?
    refute report.response_changed?
  end

  test "canoe-bank chain is scoped to sentence arguments with exact IDs" do
    canoe = [
      %{id: "canoe|noun|1", norm: "canoe", definition: "A narrow boat."},
      %{id: "canoe|noun|2", norm: "canoe", definition: "(slang) A large car."}
    ]
    boat = [
      %{id: "boat|noun|1", norm: "boat", definition: "A craft operating on water."}
    ]
    watercourse = [
      %{id: "watercourse|noun|1", norm: "watercourse",
        definition: "A channel where water flows."}
    ]
    dict = %{
      "canoe" => canoe, "boat" => boat,
      "watercourse" => watercourse, "bank" => @bank
    }
    original = si(["The", "canoe", "approached", "the", "bank"])
    report = RelationalEvidence.audit(original,
      lookup: lookup(dict), via: ["boat", "watercourse"])

    assert report.status == :candidate_relational_evidence
    assert [%{dictionary_bridge: bridge}] = report.frames
    assert Enum.any?(bridge.candidates, fn c ->
      c.source_sense_id == "canoe|noun|1" and
        c.target_sense_id == "bank|noun|10" and
        Enum.map(c.via, & &1.sense_id) ==
          ["boat|noun|1", "watercourse|noun|1"]
    end)
    refute report.semantic_relation_verified?
    assert original.active_cells == si(["The", "canoe", "approached", "the", "bank"]).active_cells
  end

  test "incidental natural overlap remains a candidate, never a relation proof" do
    loan = [
      %{id: "loan|noun|2", norm: "loan",
        definition: "A natural person may borrow money."}
    ]
    watercourse = [
      %{id: "watercourse|noun|1", norm: "watercourse",
        definition: "A natural channel through which water flows."}
    ]
    report =
      si(["The", "loan", "entered", "a", "watercourse"])
      |> RelationalEvidence.audit(
        lookup: lookup(%{"loan" => loan, "watercourse" => watercourse}))

    assert report.status == :candidate_relational_evidence
    assert [%{dictionary_bridge: bridge}] = report.frames
    assert [%{witness: "natural",
               evidence_quality: :definition_body_overlap_unverified}] = bridge.candidates
    refute bridge.relevance_verified?
    refute report.semantic_relation_verified?
  end

  test "unsupplied grammatical frame abstains instead of parsing loan at bank" do
    original = %{
      sentence: "The customer asked for a loan at the bank.",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "customer", n: 1},
        %{index: 2, phrase: "asked", n: 1},
        %{index: 3, phrase: "for", n: 1},
        %{index: 4, phrase: "a", n: 1},
        %{index: 5, phrase: "loan", n: 1},
        %{index: 6, phrase: "at", n: 1},
        %{index: 7, phrase: "the", n: 1},
        %{index: 8, phrase: "bank", n: 1}
      ],
      active_cells: [],
      sentence_signatures: []
    }

    report = RelationalEvidence.audit(original,
      lookup: fn _ -> flunk("no lookup without a validated clause") end)

    assert report.status == :unresolved
    assert report.reason == :no_attested_complete_clause
    assert report.frames == []
  end

  test "signature type or missing POS witness cannot smuggle in relation" do
    original = si(["The", "bank", "granted", "a", "loan"])

    without_verb = %{original |
      active_cells: Enum.reject(original.active_cells, &(&1.pos == "verb"))}
    assert RelationalEvidence.audit(without_verb,
      lookup: fn _ -> flunk("no lookup when verb POS absent") end).status == :unresolved

    forged = %{original |
      sentence_signatures: [
        %{hd(original.sentence_signatures) | source: :untrusted_user}
      ]}
    assert RelationalEvidence.audit(forged,
      lookup: fn _ -> flunk("no lookup for untrusted frame") end).status == :unresolved

    unsupported = %{original |
      sentence_signatures: [
        %{hd(original.sentence_signatures) | id: :finite_past_candidate}
      ]}
    assert RelationalEvidence.audit(unsupported,
      lookup: fn _ -> flunk("no lookup for partial grammatical evidence") end).status == :unresolved
  end

  test "checked dictionary errors propagate as diagnostic failure" do
    original = si(["The", "bank", "granted", "a", "loan"])
    report = RelationalEvidence.audit(original,
      lookup: fn _ -> {:error, :ltm_query_failed} end)
    assert report.status == :dictionary_lookup_error
    assert [%{dictionary_bridge: %{status: :lookup_error,
                                  reason: :ltm_query_failed}}] = report.frames
    refute report.modified_lifg?
  end
end
