defmodule Core.Response.SymbolicGenerationTest do
  use ExUnit.Case, async: false

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

  test "the canonical LIFG sense grounds Hello in the no-LLM planner" do
    si = %{
      intent: :greet,
      confidence: 0.76,
      text: "Hello Symbrella.",
      semantic_choices: [@hello_choice],
      lifg_choices: [@hello_choice],
      comprehension: %{
        intent: :greet, uncertain: ["hello", "symbrella"],
        unresolved: ["symbrella"], degraded?: true
      },
      session_id: "symbolic-greeting-#{System.unique_integer([:positive])}"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "Hello."
    assert meta.response_source == :symbolic_generation
    assert meta.response_fallback_reason == nil
    assert meta.curiosity_probe == nil
    assert meta.response_reflection.status == :symbolic_realized
    assert meta.response_reflection.provenance == :canonical_lifg_choice
    assert meta.response_reflection.lexical_witness.id == "hello|interjection|1"
    assert meta.response_reflection.semantic_verified? == false
    assert meta.response_reflection.llm_called? == false
  end

  test "Hello requires actual canonical lexical support, not a fake lexical or surface cue" do
    features = %{risk_bucket: :low, intent: :greet}
    decision = %{action: :answer}

    for si <- [
          %{},
          %{lifg_choices: [@hello_choice]},
          %{semantic_choices: []},
          %{semantic_choices: [%{@hello_choice | id: "hello|noun|1"}]},
          %{semantic_choices: [%{@hello_choice | token_index: 1}]},
          %{semantic_choices: [%{@hello_choice | pos: "noun"}]},
          %{semantic_choices: [%{@hello_choice | score: 0.0}]}
        ] do
      assert :not_applicable =
               SymbolicGeneration.generate("Hello Symbrella.", features, decision, si)
    end
  end

  test "an unsupported Hello emits the deliberate no-LLM coverage gap" do
    si = %{intent: :unknown, confidence: 0.8, text: "Hello Symbrella.",
           session_id: "symbolic-no-evidence-#{System.unique_integer([:positive])}"}

    {_tone, reply, meta} = Response.plan(si, %{})

    assert reply =~ "Symbolic response unavailable"
    assert meta.response_source == :model_unavailable
    assert meta.response_fallback_reason == :llm_disabled
  end

  test "other existing narrow greetings remain surface-only, with honest provenance" do
    for {input, expected} <- [
          {"hi", "Hello."},
          {"Good morning, Symbrella!", "Good morning."},
          {"Good afternoon Symbrella", "Good afternoon."},
          {"good eveing symbrella", "Good evening."}
        ] do
      si = %{intent: :unknown, confidence: 0.85, text: input,
             session_id: "symbolic-other-#{System.unique_integer([:positive])}"}

      {_tone, reply, meta} = Response.plan(si, %{})

      assert reply == expected, input
      assert meta.response_source == :symbolic_generation
      assert meta.response_reflection.provenance == :surface_greeting
      assert meta.response_reflection.lexical_witness == nil
    end
  end

  test "mixed turns, unrelated actions and high-risk input cannot be greeted away" do
    for text <- [
          "Hello Symbrella, can you debug this?",
          "Hello Symbrella, I started a new project."
        ] do
      assert :not_applicable =
               SymbolicGeneration.generate(text, %{risk_bucket: :low, intent: :greet},
                 %{action: :answer}, %{semantic_choices: [@hello_choice]})
    end

    assert :not_applicable =
             SymbolicGeneration.generate("Hello Symbrella.",
               %{risk_bucket: :high, intent: :greet},
               %{action: :answer}, %{semantic_choices: [@hello_choice]})

    assert :not_applicable =
             SymbolicGeneration.generate("Hello Symbrella.",
               %{risk_bucket: :low, intent: :greet},
               %{action: :suppress}, %{semantic_choices: [@hello_choice]})
  end
end
