defmodule Core.Response.SymbolicGenerationTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.SymbolicGeneration

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

  test "standalone greetings are realized as grounded speech acts without LLM fallback" do
    for {input, expected} <- [
          {"Hello Symbrella.", "Hello."},
          {"hi", "Hello."},
          {"Good morning, Symbrella!", "Good morning."},
          {"Good afternoon Symbrella", "Good afternoon."},
          {"good eveing symbrella", "Good evening."}
        ] do
      si = %{
        intent: :unknown,
        confidence: 0.85,
        text: input,
        session_id: "symbolic-greeting-#{System.unique_integer([:positive])}"
      }

      {_tone, reply, meta} = Response.plan(si, %{})

      assert reply == expected, input
      assert meta.response_source == :symbolic_generation
      assert meta.response_fallback_reason == nil
      assert meta.curiosity_probe == nil
      assert meta.response_reflection.status == :symbolic_realized
    end
  end

  test "mixed greetings and unsupported conversation remain visibly unsupported" do
    for input <- [
          "Hello Symbrella, can you debug this?",
          "Hello Symbrella, I started a new project."
        ] do
      assert :not_applicable =
               SymbolicGeneration.generate(
                 input,
                 %{risk_bucket: :low, intent: :greeting},
                 %{tone: :warm}
               )
    end
  end

  test "high-risk turns cannot be replaced by a greeting" do
    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.",
               %{risk_bucket: :high, intent: :greeting},
               %{tone: :warm}
             )
  end
end
