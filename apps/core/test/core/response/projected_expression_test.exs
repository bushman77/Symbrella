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

  defp statement, do: statement("gardener", "plants", "plant", "tree")

  # Structurally faithful fixture for two live utterances; these are
  # synthetic ID/BrainCell attestations, not a dictionary gold label.
  defp statement(actor, surface_verb, base, patient) do
    actor_id = "#{actor}|noun|1"
    verb_id = "#{surface_verb}|verb|1"
    patient_id = "#{patient}|noun|1"

    choices = [
      %{token_index: 1, id: actor_id, lemma: actor},
      %{token_index: 2, id: verb_id, lemma: surface_verb},
      %{token_index: 4, id: patient_id, lemma: patient}
    ]

    %{
      sentence: "The #{actor} #{surface_verb} a #{patient}.",
      source: :user,
      experience_origin: :user_turn,
      intent: :statement,
      confidence: 0.68,
      session_id: "projected-expression-fixture",
      trace: [],
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: actor, n: 1},
        %{index: 2, phrase: surface_verb, n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: patient <> ".", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          evidence: @svo_evidence, span: {0, 4},
          roles: %{subject_determiner: 0, subject_head: 1,
            predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: actor_id, norm: actor, pos: "noun",
          definition: "The subject of the action."},
        %{id: patient_id, norm: patient, pos: "noun",
          definition: "The object of the action."},
        %{id: verb_id, norm: surface_verb, pos: "verb",
          semantic_atoms: @morphology,
          definition: "An inflected form of #{base}."}
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
    assert {:ok, _} = Core.Brain.SynapticExperience.extract(si)
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

  test "live observed present-perfect substitutions are rejected using only source-tense evidence" do
    cases = [
      {statement(), "The gardener has planted a tree.",
        "I understand you're describing the gardener planting a tree."},
      {statement("baker", "cooks", "cook", "meal"),
        "The baker has cooked a meal.",
        "I understand you're describing the baker cooking a meal."},
      {statement("child", "throws", "throw", "ball"),
        "The child has thrown a ball.",
        "I understand you're describing the child throwing a ball."},
      {statement(), "The gardener planted a tree.",
        "I understand you're describing the gardener planting a tree."},
      {statement(), "The gardener will plant a tree.",
        "I understand you're describing the gardener planting a tree."},
      {statement(), "The gardener is planting a tree.",
        "I understand you're describing the gardener planting a tree."}
    ]

    for {si, changed_tense, expected} <- cases do
      assert {:ok, _} = Core.Brain.SynapticExperience.extract(si)
      frame = ProjectedExpression.from_si(si)
      assert is_map(frame)

      assert {:repaired, :unsupported_temporal_shift, ^expected} =
        ProjectedExpression.review_draft(si.sentence, changed_tense, frame)

      assert {:repaired, ^expected} =
        ProjectedExpression.repair_temporal_shift(
          si.sentence, changed_tense, frame)
    end
  end

  test "tense repair is narrow and cannot invent unsupported reinterpretations" do
    si = statement()
    frame = ProjectedExpression.from_si(si)

    for draft <- [
      "You're describing the gardener planting a tree.",
      "The gardener plants a tree. That describes an action.",
      "You mentioned a gardener and a tree.",
      "I wonder whether the gardener has planted a tree.",
      "The gardener has planted a tree before, but that is another story.",
      "The gardener is planting a tree in your hypothetical example.",
      "The gardener has cooked a tree."
    ] do
      assert ProjectedExpression.review_draft(
        si.sentence, draft, frame) == :unchanged
    end

    assert ProjectedExpression.review_draft(
      si.sentence, "The gardener has planted a tree.", nil
    ) == :unchanged

    assert ProjectedExpression.review_draft(
      si.sentence, "The gardener has planted a tree.",
      Map.put(frame, :kind, :invalid)
    ) == :unchanged

    assert ProjectedExpression.review_draft(
      "Has the gardener planted a tree?",
      "The gardener has planted a tree.",
      frame
    ) == :unchanged
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
    use GenServer

    def start_link(test_pid) do
      GenServer.start_link(__MODULE__, test_pid, name: __MODULE__)
    end

    @impl true
    def init(test_pid), do: {:ok, %{owner: test_pid, draft: :echo}}

    def chat(messages, opts) do
      GenServer.call(__MODULE__, {:chat, messages, opts})
    end

    def next_draft(text) when is_binary(text) do
      GenServer.call(__MODULE__, {:next_draft, text})
    end

    @impl true
    def handle_call({:next_draft, text}, _from, state) do
      {:reply, :ok, %{state | draft: text}}
    end

    @impl true
    def handle_call({:chat, messages, _opts}, _from, state) do
      user = messages |> List.last() |> Map.fetch!("content")
      send(state.owner, {:projected_echo_llm_prompt, hd(messages)["content"]})
      draft =
        case state.draft do
          :echo -> user <> "\n\n" <> user
          text when is_binary(text) -> text
        end

      {:reply, {:ok, %{content: draft}}, state}
    end
  end

  test "free-form LLM synthesis passes grounded projection and repairs its real repeated echo" do
    old_client = Application.get_env(:core, :llm_client)
    old_retrieval = Application.get_env(:core, :web_retrieval)

    Application.put_env(:core, :llm_client, EchoLlm)
    Application.put_env(:core, :web_retrieval, enabled?: false)
    start_supervised!({EchoLlm, self()})

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
      assert reflection.status == :repair
      assert reflection.applied? == true
      assert :projected_statement_echo in reflection.issues
      assert reflection.draft_sha256 != reflection.final_sha256
      assert reflection.final_text == answer
      assert_receive {:projected_echo_llm_prompt, prompt}
      assert prompt =~ "USER-REPORTED ACTION"
      assert prompt =~ ~s(actor="the gardener")
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
      assert get_in(attached,
        [:response_meta, :response_reflection,
         :projected_expression_repair, :trigger]) == :exact_report_echo
      assert attached.response_meta.response_reflection.status == :repair
      assert attached.response_meta.response_reflection.applied?
      assert Enum.sort(Map.keys(
        attached.response_meta.response_reflection.projected_expression_repair
      )) == Enum.sort([
        :attempted?, :applied?, :trigger, :source,
        :original_reflection_status, :promoted_sense?, :stored_memory?
      ])
      refute Map.has_key?(attached.response_meta.response_reflection, :draft_text)
      refute Map.has_key?(attached.response_meta.response_reflection, :final_text)
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

  test "actual LLM and response attachment repair gardener and baker completed-event paraphrases" do
    old_client = Application.get_env(:core, :llm_client)
    old_retrieval = Application.get_env(:core, :web_retrieval)

    Application.put_env(:core, :llm_client, EchoLlm)
    Application.put_env(:core, :web_retrieval, enabled?: false)
    start_supervised!({EchoLlm, self()})

    try do
      for {si, model_draft, expected} <- [
        {statement(), "The gardener has planted a tree.",
         "I understand you're describing the gardener planting a tree."},
        {statement("baker", "cooks", "cook", "meal"),
         "The baker has cooked a meal.",
         "I understand you're describing the baker cooking a meal."}
      ] do
        :ok = EchoLlm.next_draft(model_draft)

        frame = ProjectedExpression.from_si(si)
        assert is_map(frame)

        features = %{
          intent: :statement, confidence: 0.68,
          selected_action: :answer_user,
          session_id: "temporal-review-#{System.unique_integer([:positive])}",
          projected_expression: frame
        }
        decision = %{tone: :neutral, mode: :scribe, action: :answer}

        assert {:ok, ^expected, reflection} =
          LlmSynthesis.generate(si.sentence, features, decision, %{}, nil)

        assert reflection.status == :repair
        assert reflection.applied? == true
        assert reflection.projected_expression_repair.trigger ==
          :unsupported_temporal_shift
        assert :projected_tense_aspect_drift in reflection.issues
        assert reflection.draft_sha256 != reflection.final_sha256
        assert reflection.projected_expression_repair.promoted_sense? == false
        assert reflection.projected_expression_repair.stored_memory? == false

        assert_receive {:projected_echo_llm_prompt, system_prompt}
        assert system_prompt =~ "Preserve the source clause's simple-present"

        :ok = EchoLlm.next_draft(model_draft)

        attached =
          si
          |> Map.put(:session_id,
            "temporal-attach-#{System.unique_integer([:positive])}")
          |> Core.Response.Attach.maybe_build_response_plan(response: :auto)

        assert attached.response_text == expected
        assert attached.response_meta.response_source == :llm
        assert attached.response_meta.response_reflection.status == :repair
        assert get_in(attached, [:response_meta, :response_reflection,
          :projected_expression_repair, :trigger]) == :unsupported_temporal_shift
        assert attached.lifg_choices == si.lifg_choices
        assert attached.atl_slate == si.atl_slate
      end
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
