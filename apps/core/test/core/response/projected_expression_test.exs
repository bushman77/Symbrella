defmodule Core.Response.ProjectedExpressionTest do
  use ExUnit.Case, async: false

  alias Core.Response.{ProjectedExpression, LlmPrompt, LlmSynthesis}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [
    :contiguous_surface, :attested_nominal_subject,
    :attested_nominal_object, :attested_finite_present_verb
  ]
  @morphology ["tag:form-of", "tag:indicative",
               "tag:present", "tag:singular", "tag:third-person"]

  defp statement do
    choices = [
      %{token_index: 1, id: "gardener|noun|1", lemma: "gardener"},
      %{token_index: 2, id: "plants|verb|1", lemma: "plants"},
      %{token_index: 4, id: "tree|noun|1", lemma: "tree"}
    ]

    %{
      sentence: "The gardener plants a tree.",
      source: :user,
      experience_origin: :user_turn,
      intent: :statement,
      confidence: 0.68,
      session_id: "projected-expression-fixture",
      trace: [],
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "gardener", n: 1},
        %{index: 2, phrase: "plants", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "tree.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          evidence: @svo_evidence, span: {0, 4},
          roles: %{subject_determiner: 0, subject_head: 1,
            predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: "plants|verb|1", norm: "plants", pos: "verb",
          semantic_atoms: @morphology,
          definition: "An inflected form of plant."}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices}
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp cohort_trace do
    [%{
      stage: :semantic_sense_shadow,
      meta: %{
        mode: :shadow_only,
        semantic_cohort: %{
          status: :candidate_cohorts,
          chose_exact_sense?: false,
          cohorts: [
            %{anchor_term: "plant", members: [
              %{id: "plant|verb|1",
                distinguishing_definition_terms: ["soil", "seed", "substrate"]},
              %{id: "plant|verb|2",
                distinguishing_definition_terms: ["furnish", "supply"]}]}
          ]
        }
      }
    }]
  end

  test "genuine projected SVO statement becomes a bounded, nonfactual expression frame" do
    si = statement()
    unchanged = si
    frame = ProjectedExpression.from_si(si)

    assert frame.kind == :user_reported_action
    assert frame.actor == "the gardener"
    assert frame.predicate_surface == "plants"
    assert frame.predicate_lemma == "plant"
    assert frame.patient == "a tree"
    assert frame.event_occurrence_verified? == false
    assert frame.exact_verb_sense_verified? == false
    assert frame.response_is_memory_acknowledgment? == false
    assert frame.cohort.status == :no_verified_cohort
    assert si == unchanged
  end

  test "a cohort provides weak expression guidance, not an exact sense winner" do
    frame =
      statement()
      |> Map.put(:trace, cohort_trace())
      |> ProjectedExpression.from_si()

    assert frame.cohort.shared_terms == ["plant"]
    assert frame.cohort.exact_sense_known? == false

    prompt = ProjectedExpression.prompt(frame)
    assert prompt =~ ~s(actor="the gardener")
    assert prompt =~ ~s(patient="a tree")
    assert prompt =~ "no single exact meaning is proven"
    assert prompt =~ "not a request to memorize"
    assert prompt =~ "without merely"
    refute prompt =~ "place a seed in soil"
    refute prompt =~ "plant|verb|1"
  end

  test "LLM system prompt receives only bounded revalidated structure" do
    frame = ProjectedExpression.from_si(statement())

    prompt =
      LlmPrompt.build_system_prompt(
        %{intent: :statement, projected_expression: frame},
        %{tone: :neutral, mode: :scribe, action: :answer},
        %{}, []
      )

    assert prompt =~ "USER-REPORTED ACTION"
    assert prompt =~ "not a request to memorize"
    assert prompt =~ ~s(inflected predicate="plants")
    assert prompt =~ "Do not turn an ordinary assertion into a task"
    refute prompt =~ "plant|verb|1"
  end

  test "single and repeated model echoes are rewritten as source-grounded paraphrases" do
    frame = ProjectedExpression.from_si(statement())
    original = "The gardener plants a tree."

    for draft <- [original, original <> "\n\n" <> original,
                   "the gardener plants a tree!"] do
      assert {:repaired, paraphrase} =
        ProjectedExpression.repair_echo(original, draft, frame)

      assert paraphrase == "I understand you're describing the gardener planting a tree."
      refute paraphrase == original
      refute paraphrase =~ "soil"
      refute paraphrase =~ "verified"
    end
  end

  test "model cannot assert an unsupported exact-sense detail from a competing cohort" do
    si = statement() |> Map.put(:trace, cohort_trace())
    frame = ProjectedExpression.from_si(si)

    assert "soil" in frame.cohort.distinguishing_terms
    assert "supply" in frame.cohort.distinguishing_terms

    assert {:repaired, :unverified_sense_detail, paraphrase} =
      ProjectedExpression.review_draft(
        "The gardener plants a tree.",
        "The gardener places a seed in the soil.",
        frame
      )

    assert paraphrase ==
      "I understand you're describing the gardener planting a tree."

    assert {:repaired, :exact_report_echo, ^paraphrase} =
      ProjectedExpression.review_draft(
        si.sentence, si.sentence, frame)

    # Source-mentioned details are not treated as an unsupported guess.
    assert ProjectedExpression.review_draft(
      "The gardener plants a tree.",
      "You're describing the gardener working with a tree.",
      frame) == :unchanged
  end

  test "a substantive reply is preserved even if it quotes the original statement" do
    frame = ProjectedExpression.from_si(statement())

    for response <- [
      "I see how the gardener and tree relate in that example.",
      "The gardener plants a tree. That's an action involving the tree.",
      "Do you mean a garden tree or a street tree?"
    ] do
      assert ProjectedExpression.repair_echo(
        "The gardener plants a tree.", response, frame) == :unchanged
    end
  end

  test "unproven, forged, assistant, question, and non-statement inputs fail closed" do
    si = statement()

    for changed <- [
      %{si | intent: :unknown},
      %{si | intent: :memory_write},
      %{si | source: :assistant},
      %{si | atl_slate: %{winners: []}},
      %{si | sentence_meaning: nil},
      %{si | sentence: "Does the gardener plant a tree?"},
      %{si | active_cells: []}
    ] do
      assert ProjectedExpression.from_si(changed) == nil
    end
  end

  defmodule EchoLlm do
    def chat(messages, _opts) when is_list(messages) do
      # Return the actual user message twice to reproduce the observed bug.
      user = messages |> List.last() |> Map.fetch!("content")
      {:ok, %{content: user <> "\n\n" <> user}}
    end
  end

  test "free-form LLM synthesis passes grounded projection and repairs its real repeated echo" do
    old_client = Application.get_env(:core, :llm_client)
    old_retrieval = Application.get_env(:core, :web_retrieval)

    Application.put_env(:core, :llm_client, EchoLlm)
    Application.put_env(:core, :web_retrieval, enabled?: false)

    try do
      si = statement()
      frame = ProjectedExpression.from_si(si)
      text = si.sentence

      features = %{
        intent: :statement, confidence: 0.68,
        selected_action: :answer_user,
        session_id: "projected-echo-#{System.unique_integer([:positive])}",
        projected_expression: frame
      }
      decision = %{tone: :neutral, mode: :scribe, action: :answer}

      assert {:ok, answer, reflection} =
        LlmSynthesis.generate(text, features, decision, %{}, nil)

      assert answer ==
               "I understand you're describing the gardener planting a tree."
      assert reflection.projected_expression_repair.attempted?
      assert reflection.projected_expression_repair.applied?
      assert reflection.projected_expression_repair.promoted_sense? == false
      assert reflection.projected_expression_repair.stored_memory? == false
      refute answer =~ "in soil"
      refute answer =~ "plant|verb|1"

      # Exercise the actual Core.Response.Attach boundary used by
      # Core.resolve_input/2, not just direct LLM synthesis.
      attached =
        si
        |> Map.put(:session_id,
          "projected-attach-#{System.unique_integer([:positive])}")
        |> Core.Response.Attach.maybe_build_response_plan(response: :auto)

      assert attached.response_text ==
        "I understand you're describing the gardener planting a tree."
      assert attached.response_meta.response_source == :llm
      assert get_in(attached,
        [:response_meta, :response_reflection,
         :projected_expression_repair, :source]) == :revalidated_sentence_roles
      assert attached.lifg_choices == si.lifg_choices
      assert attached.atl_slate == si.atl_slate
    after
      if old_client == nil,
        do: Application.delete_env(:core, :llm_client),
        else: Application.put_env(:core, :llm_client, old_client)

      if old_retrieval == nil,
        do: Application.delete_env(:core, :web_retrieval),
        else: Application.put_env(:core, :web_retrieval, old_retrieval)
    end
  end

  test "without verified statement evidence the echo repair is disabled" do
    assert ProjectedExpression.repair_echo(
      "The gardener plants a tree.",
      "The gardener plants a tree.",
      nil
    ) == :unchanged
  end
end
