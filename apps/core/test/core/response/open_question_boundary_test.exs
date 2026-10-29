defmodule Core.Response.OpenQuestionBoundaryTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.OpenQuestionBoundary

  setup do
    previous = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if previous == :unset,
        do: Application.delete_env(:core, :llm_inference_enabled?),
        else: Application.put_env(:core, :llm_inference_enabled?, previous)
    end)

    :ok
  end

  defp si(sentence, extras \\ %{}) do
    Map.merge(%{
      source: :user,
      sentence: sentence,
      text: sentence,
      intent: :ask_info,
      confidence: 0.7,
      session_id: "open-question-#{System.unique_integer([:positive])}"
    }, extras)
  end

  defp realize(sentence, extras \\ %{}) do
    source = si(sentence, extras)

    OpenQuestionBoundary.generate(
      sentence,
      %{risk_bucket: :low, intent: :ask_info},
      %{action: :answer},
      source
    )
  end

  test "live-chat examples are categorized by conversational operation, not unknown word senses" do
    for sentence <- [
      "what is new in the world of you",
      "What's new with you?",
      "What is new with Symbrella?",
      "what have you been up to?"
    ] do
      assert {:ok, response, metadata} = realize(sentence)
      assert response =~ "what my current capabilities are"
      assert metadata.requested_operation == :assistant_update_request
      assert metadata.missing_evidence == [:runtime_capabilities]
      assert metadata.topic_grounded? == false
      assert metadata.external_facts_verified? == false
      assert metadata.llm_called? == false
    end

    for sentence <- [
      "what do you think of the iran - US",
      "what do you think of the iranian american war",
      "what do you think of the U.S. -Iran war",
      "what do you think if the U.S. iranian war",
      "What do you think about the garden?",
      "What's your take on local housing?",
      "What is your opinion on this proposal?",
      "How do you feel about these events?"
    ] do
      assert {:ok, response, metadata} = realize(sentence)
      assert response =~ "asking for an assessment"
      assert response =~ "verified evidence"
      assert metadata.requested_operation == :assessment_request
      assert metadata.topic_grounded? == false
      assert metadata.factual_claims? == false
      assert metadata.llm_called? == false
      refute response =~ "I can identify"
    end

    for sentence <- [
      "What's happening with the election?",
      "What is new in the city?",
      "What's the latest on monetary policy?",
      "What has been happening with the energy sector?",
      "What is going on with housing?"
    ] do
      assert {:ok, response, metadata} = realize(sentence)
      assert response =~ "recent information"
      assert response =~ "dated, attributable sources"
      assert metadata.requested_operation == :current_information_request
      assert metadata.missing_evidence == [:specific_claim, :dated_sources]
      assert metadata.factual_claims? == false
    end
  end

  test "response planner selects request-boundary over generic lexical repair when LLM is off" do
    for sentence <- [
      "what is new in the world of you",
      "what do you think of the iran - US",
      "what do you think of the iranian american war",
      "what do you think of the U.S. -Iran war",
      "what do you think if the U.S. iranian war"
    ] do
      {_tone, response, metadata} = Response.plan(si(sentence), %{})

      assert metadata.response_source == :symbolic_request_boundary,
        "unexpected planning route for #{inspect(sentence)}: #{inspect(metadata.response_source)}"
      assert metadata.response_reflection.status == :open_question_evidence_boundary
      assert metadata.response_reflection.llm_called? == false
      assert metadata.response_reflection.topic_grounded? == false
      assert metadata.response_reflection.external_facts_verified? == false
      assert metadata.response_reflection.factual_claims? == false
      assert metadata.response_reflection.provenance == :current_turn_request_construction
      assert metadata.response_reflection.requested_operation in [
        :assistant_update_request, :assessment_request, :current_information_request
      ]
      # Explanations expose the operation and missing evidence, not raw topic spans.
      refute Map.has_key?(metadata.response_reflection, :requested_topic_surface)
      assert metadata.curiosity_probe == nil
      refute response =~ "I can identify"
      refute response =~ "Symbolic response unavailable"
      refute response =~ "I read that as likely about the US-Iran conflict"
      refute response =~ "I don't have live news access in this runtime"
    end
  end

  test "forged SI, high-risk turn, unsupported action and disallowed intent abstain" do
    sentence = "What do you think of ocean currents?"
    base = si(sentence)

    assert :not_applicable =
      OpenQuestionBoundary.generate(sentence,
        %{risk_bucket: :low}, %{action: :answer},
        %{base | sentence: "Different sentence"})

    assert :not_applicable =
      OpenQuestionBoundary.generate(sentence,
        %{risk_bucket: :low}, %{action: :answer},
        %{base | source: :assistant})

    assert :not_applicable =
      OpenQuestionBoundary.generate(sentence,
        %{risk_bucket: :high}, %{action: :answer}, base)

    assert :not_applicable =
      OpenQuestionBoundary.generate(sentence,
        %{risk_bucket: :low}, %{action: :suppress}, base)

    assert :not_applicable =
      OpenQuestionBoundary.generate(sentence,
        %{risk_bucket: :low}, %{action: :answer},
        %{base | intent: :memory_write})
  end

  test "non-question statements, definition queries, unrelated structures and huge topics abstain" do
    for sentence <- [
      "The bird is flying into the tree.",
      "What is the bird doing?",
      "What is the meaning of bank?",
      "hello symbrella",
      "tell me what you think",
      "what do you think of",
      "I think of going home",
      "What do you think of " <> String.duplicate("a", 150),
      "What is new with " <> String.duplicate("a", 150)
    ] do
      assert :not_applicable == OpenQuestionBoundary.recognize(sentence),
        "incorrectly classified #{inspect(sentence)}"
    end
  end
end
