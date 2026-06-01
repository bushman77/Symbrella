defmodule Core.Response.ModesTest do
  use ExUnit.Case, async: true

  alias Core.Response.Modes

  describe "compose/4 – safety fallbacks" do
    test "abuse + deescalate uses respectful boundary copy" do
      text = Modes.compose(:abuse, :deescalate, :editor, %{})

      assert text =~ "I'll keep this respectful and useful"
      assert text =~ "Tell me what you want changed in Symbrella"
    end

    test "abuse + firm uses constructive boundary copy" do
      text = Modes.compose(:abuse, :firm, :editor, %{})

      assert text =~ "Let's keep it constructive"
      assert text =~ "Name the file or task you want changed"
    end
  end

  describe "compose/4 – deterministic fallback only" do
    test "uses conversational fallback copy for non-technical unknowns" do
      text =
        Modes.compose(:unknown, :neutral, :unknown_mode, %{
          variant_seed: 1,
          next_step: "Ask one targeted question."
        })

      refute text =~ "Ready. Point me at the module"
      refute text =~ "clean drop-in"
      refute text =~ "module"
      refute text =~ "file"
      assert text =~ "I’m with you"
      assert text =~ "Suggested next step: Ask one targeted question."
    end

    test "keeps technical fallback copy for technical requests" do
      text =
        Modes.compose(:unknown, :neutral, :unknown_mode, %{
          text: "this compile error points at a Phoenix module",
          variant_seed: 1,
          next_step: "Ask one targeted question."
        })

      assert text =~ "Give me the next concrete target"
      assert text =~ "Suggested next step: Ask one targeted question."
    end

    test "opinion questions are not treated as casual greeting fallback" do
      text =
        Modes.compose(:ask, :neutral, :scribe, %{
          text: "hey what do you think of war?",
          variant_seed: 0,
          next_step: nil
        })

      assert text =~ "War"
      assert text =~ "anti-suffering"
      refute text =~ "Hey. I’m here with you."
      refute text =~ "module"
      refute text =~ "file"
    end

    test "personal housing update avoids generic comprehension fallback" do
      text =
        Modes.compose(:unknown, :neutral, :unknown_mode, %{
          text: "good afternoon symbrella im close to getting my own place",
          variant_seed: 3,
          next_step: nil
        })

      assert text =~ "Good afternoon"
      assert text =~ "own place"
      refute text =~ "simpler words"
      refute text =~ "Suggested next step"
      refute text =~ "module"
      refute text =~ "file"
    end

    test "war alternatives typo gets substantive fallback" do
      text =
        Modes.compose(:ask, :neutral, :scribe, %{
          text: "what are alternnatives  too war",
          variant_seed: 0,
          next_step: nil
        })

      assert text =~ "Alternatives to war"
      assert text =~ "diplomacy"
      assert text =~ "ceasefires"
      refute text =~ "what are you trying to talk through"
      refute text =~ "Suggested next step"
    end

    test "does not emit normal-mode canned menus or drop-in invitations" do
      text =
        Modes.compose(:refactor, :warm, :collaborator, %{
          variant_seed: 0,
          file_hint: "apps/core/lib/core/response.ex"
        })

      refute text =~ "Welcome"
      refute text =~ "full file"
      refute text =~ "paste-ready"
      refute text =~ "drop-in"
      assert text =~ "Relevant target: `apps/core/lib/core/response.ex`."
    end

    test "health support fallback avoids engineering language" do
      text = Modes.compose(:health_support, :warm, :supportive_care, %{})

      assert text =~ "pharmacist or prescriber"
      assert text =~ "missed medication dose"
      assert text =~ "should not tell you how to change the dose"

      refute text =~ "module"
      refute text =~ "file"
      refute text =~ "failing output"
      refute text =~ "engineering move"
    end
  end
end
