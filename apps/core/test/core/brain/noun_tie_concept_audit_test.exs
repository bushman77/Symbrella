defmodule Core.Brain.NounTieConceptAuditTest do
  use ExUnit.Case, async: false

  alias Core.Brain.NounTieConceptAudit
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @evidence [:contiguous_surface, :attested_nominal_subject,
             :attested_nominal_object, :attested_finite_present_verb]
  @form_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp input(probs \\ %{"meal|noun|1" => 0.5, "meal|noun|2" => 0.5}) do
    choices = [
      %{token_index: 1, id: "baker|noun|1",
        definition: "Someone who bakes bread.",
        candidate_probs: %{"baker|noun|1" => 1.0}},
      %{token_index: 2, id: "cooks|verb|1", lemma: "cooks"},
      %{token_index: 4, id: "meal|noun|1",
        definition: "Food prepared and eaten at a sitting.",
        candidate_probs: probs}
    ]

    %{
      source: :user,
      experience_origin: :user_turn,
      session_id: "noun-tie-audit-fixture",
      sentence: "The baker cooks a meal.",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "baker", n: 1},
        %{index: 2, phrase: "cooks", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "meal.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @evidence,
          roles: %{subject_determiner: 0, subject_head: 1,
                   predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: "baker|noun|1", norm: "baker", pos: "noun",
          definition: "Someone who bakes bread."},
        %{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "Food prepared and eaten at a sitting."},
        %{id: "cooks|verb|1", norm: "cooks", pos: "verb",
          semantic_atoms: @form_tags,
          definition: "An inflected form of cook."}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp two_noun_senses(second \\ "A time when food is served for eating.") do
    [
      %{id: "meal|noun|1", norm: "meal", pos: "noun",
        definition: "Food prepared and eaten at a sitting."},
      %{id: "meal|noun|2", norm: "meal", pos: "noun",
        definition: second}
    ]
  end

  defp patient(result), do: Enum.find(result.roles, &(&1.role == :patient))

  test "equal probabilities alone do not verify a shared meaning, but real shared gloss tokens are reported" do
    si = input()
    original = si
    result = NounTieConceptAudit.inspect(si,
      fetch_noun_senses: fn "meal" -> two_noun_senses() end)

    assert result.status == :read_only_audit
    assert result.equal_scores_mean_shared_meaning? == false
    assert result.promoted_sense? == false
    assert result.changed_lifg? == false

    row = patient(result)
    assert row.status == :ambiguous_with_unanimous_terms
    assert row.tied_count == 2
    assert row.tied_noun_sense_ids == ["meal|noun|1", "meal|noun|2"]
    assert "food" in row.unanimous_gloss_tokens
    assert row.exact_noun_sense_verified? == false
    assert si == original
  end

  test "real disagreement across tied definitions shows partial evidence, not a unanimous concept" do
    result = NounTieConceptAudit.inspect(input(),
      fetch_noun_senses: fn "meal" ->
        two_noun_senses("Ground maize grains fed to livestock.")
      end)

    row = patient(result)
    assert row.status == :ambiguous_with_no_unanimous_terms
    assert row.unanimous_gloss_tokens == []
    assert row.exact_noun_sense_verified? == false
  end

  test "near ties and missing dictionary IDs fail without a fabricated concept" do
    almost = input(%{"meal|noun|1" => 0.51, "meal|noun|2" => 0.49})
    result = NounTieConceptAudit.inspect(almost,
      fetch_noun_senses: fn _ -> raise "near tie must not trigger dictionary lookup" end)
    assert patient(result).reason == :no_exact_top_tie

    missing = NounTieConceptAudit.inspect(input(),
      fetch_noun_senses: fn _ -> [hd(two_noun_senses())] end)
    assert patient(missing).reason == :missing_or_duplicate_dictionary_ids
  end

  test "stale selected definition and duplicate tied IDs fail closed" do
    result = NounTieConceptAudit.inspect(input(),
      fetch_noun_senses: fn _ ->
        two_noun_senses()
        |> Enum.map(fn
          %{id: "meal|noun|1"} = row -> %{row | definition: "Different gloss."}
          row -> row
        end)
      end)
    assert patient(result).reason == :selected_definition_mismatch

    duplicate = NounTieConceptAudit.inspect(input(),
      fetch_noun_senses: fn _ ->
        [hd(two_noun_senses()), hd(two_noun_senses())]
      end)
    assert patient(duplicate).reason == :missing_or_duplicate_dictionary_ids
  end

  test "unverified SVO and assistant-origin claims cannot inspect noun senses" do
    si = input()
    for invalid <- [%{si | source: :assistant},
                    %{si | atl_slate: %{winners: []}},
                    %{si | sentence_meaning: nil}] do
      assert %{status: :skipped, reason: :unvalidated_svo} =
        NounTieConceptAudit.inspect(invalid,
          fetch_noun_senses: fn _ -> raise "untrusted source fetched dictionary" end)
    end
  end
end
