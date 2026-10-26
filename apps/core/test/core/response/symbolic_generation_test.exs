defmodule Core.Response.SymbolicGenerationTest do
  use ExUnit.Case, async: false

  alias Core.Semantic.SpeechActs
  alias Core.Response
  alias Core.Response.SymbolicGeneration

  @hello_choice %{
    token_index: 0, id: "hello|interjection|1", pos: "interjection",
    lemma: "hello", source: :active_cells, score: 0.16190361976623535
  }

  setup do
    original = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if original == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, original)
      end
    end)

    :ok
  end

  test "a canonical lexical greeting and identity vocative generate a supported reply" do
    si =
      %{
        sentence: "Hello Symbrella.",
        intent: :greet,
        confidence: 0.76,
        text: "Hello Symbrella.",
        semantic_choices: [@hello_choice],
        lifg_choices: [@hello_choice],
        comprehension: %{intent: :greet, degraded?: true, unresolved: ["symbrella"]},
        session_id: "symbolic-greeting-#{System.unique_integer([:positive])}"
      }
      |> SpeechActs.attach()

    assert si.speech_act_frame.addressee == :self
    assert si.speech_act_frame.vocative.resolution == :assistant_identity
    assert si.speech_act_frame.lexical_evidence.level == :canonical_lifg_selection

    {_tone, reply, meta} = Response.plan(si, %{})
    assert reply == "Hello."
    assert meta.response_source == :symbolic_generation
    assert meta.response_fallback_reason == nil
    assert meta.curiosity_probe == nil
    assert meta.response_reflection.status == :symbolic_realized
    assert meta.response_reflection.speaker == :user
    assert meta.response_reflection.addressee == :self
    assert meta.response_reflection.vocative.resolution == :assistant_identity
    assert meta.response_reflection.lexical_witness.id == "hello|interjection|1"
    assert meta.response_reflection.provenance == :canonical_lifg_choice
    assert meta.response_reflection.semantic_verified? == false
    assert meta.response_reflection.llm_called? == false
  end

  test "wrong or missing canonical witness never licenses Hello" do
    for choices <- [
          nil,
          [],
          [%{@hello_choice | id: "hello|noun|1"}],
          [%{@hello_choice | token_index: 1}],
          [%{@hello_choice | pos: "noun"}],
          [%{@hello_choice | score: 0.0}]
        ] do
      si =
        SpeechActs.attach(%{sentence: "Hello Symbrella.", semantic_choices: choices})

      assert si.speech_act_frame.lexical_evidence.level == :insufficient

      assert :not_applicable =
               SymbolicGeneration.generate(
                 "Hello Symbrella.",
                 %{risk_bucket: :low, intent: :greet},
                 %{action: :answer},
                 si
               )
    end
  end

  test "stale lexical evidence alone is insufficient without a bound speech-act frame" do
    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.",
               %{risk_bucket: :low, intent: :greet},
               %{action: :answer},
               %{semantic_choices: [@hello_choice]}
             )
  end

  test "unsupported Hello yields the explicit no-LLM result" do
    si =
      %{intent: :unknown, confidence: 0.8, sentence: "Hello Symbrella.",
        text: "Hello Symbrella.",
        session_id: "symbolic-no-evidence-#{System.unique_integer([:positive])}"}
      |> SpeechActs.attach()

    {_tone, reply, meta} = Response.plan(si, %{})
    assert reply =~ "Symbolic response unavailable"
    assert meta.response_source == :model_unavailable
    assert meta.response_fallback_reason == :llm_disabled
  end

  test "surface-only greetings remain available with their weaker provenance" do
    for {input, expected} <- [
          {"hi", "Hello."},
          {"Good morning, Symbrella!", "Good morning."},
          {"Good afternoon Symbrella", "Good afternoon."},
          {"good eveing symbrella", "Good evening."}
        ] do
      si =
        %{intent: :unknown, confidence: 0.85, sentence: input, text: input,
          session_id: "symbolic-other-#{System.unique_integer([:positive])}"}
        |> SpeechActs.attach()

      {_tone, reply, meta} = Response.plan(si, %{})
      assert reply == expected, input
      assert meta.response_source == :symbolic_generation
      assert meta.response_reflection.provenance == :surface_greeting
      assert meta.response_reflection.lexical_witness == nil
    end
  end

  test "other addressees, reported speech and mixed turns do not become self-greetings" do
    features = %{risk_bucket: :low, intent: :greet}
    decision = %{action: :answer}

    other =
      SpeechActs.attach(%{
        sentence: "Hello Bradley.",
        semantic_choices: [@hello_choice]
      })

    assert other.speech_act_frame.speech_act == :greeting
    assert other.speech_act_frame.addressee == :other
    assert other.speech_act_frame.vocative.resolution == :unresolved_other
    assert :not_applicable =
             SymbolicGeneration.generate("Hello Bradley.", features, decision, other)

    for sentence <- [
          "Symbrella said hello.",
          "Hello Symbrella, how are you?",
          "Hello Symbrella and Bradley.",
          "The word hello is an interjection."
        ] do
      si = SpeechActs.attach(%{sentence: sentence, semantic_choices: [@hello_choice]})
      assert is_nil(si.speech_act_frame), sentence
      assert :not_applicable =
               SymbolicGeneration.generate(sentence, features, decision, si)
    end
  end

  test "input text, selected action and guardrail must match the bound frame" do
    si =
      SpeechActs.attach(%{
        sentence: "Hello Symbrella.",
        semantic_choices: [@hello_choice]
      })

    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Bradley.", %{risk_bucket: :low, intent: :greet},
               %{action: :answer}, si)

    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.", %{risk_bucket: :high, intent: :greet},
               %{action: :answer}, si)

    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.", %{risk_bucket: :low, intent: :greet},
               %{action: :suppress}, si)
  end
end
