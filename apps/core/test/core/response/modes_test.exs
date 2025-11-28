defmodule Core.Response.ModesTest do
  use ExUnit.Case, async: true

  alias Core.Response.Modes

  describe "compose/4 – coach mode for bug intents" do
    test "bug + deescalate uses the reassuring 'test failures are frustrating' copy" do
      text = Modes.compose(:bug, :deescalate, :coach, %{})

      assert text =~ "Test failures are frustrating but fixable"
      assert text =~ "one failure at a time"
    end

    test "bug + warm uses the 'let's get this test passing' copy" do
      text = Modes.compose(:bug, :warm, :coach, %{})

      assert text =~ "Let's get this test passing"
      assert text =~ "start from the failing output"
    end

    test "bug + neutral still uses the bug-specific coach copy" do
      text = Modes.compose(:bug, :neutral, :coach, %{})

      assert text =~ "Let's get this test passing"
      assert text =~ "start from the failing output"
    end

    test "non-bug helpful intents keep the generic coach copy" do
      text = Modes.compose(:question, :neutral, :coach, %{})

      assert text =~ "Let's pick a small next step."
      assert text =~ "A) I act. B) clarify one detail."
    end

    test "next_step hint gets appended correctly" do
      text =
        Modes.compose(:bug, :warm, :coach, %{
          next_step: "Run `mix test` only for the failing file."
        })

      assert text =~ "Let's get this test passing."
      assert text =~ "Suggested next step: Run `mix test` only for the failing file."
    end
  end

  describe "compose/4 – pair_programmer warm collaborator" do
    test "warm pair_programmer copy mentions plan and full file option" do
      text = Modes.compose(:refactor, :warm, :pair_programmer, %{})

      assert text =~ "concise plan"
      assert text =~ "full file"
      assert text =~ "paste-ready"
    end
  end

  describe "compose/4 – abuse + tones (firm guardian)" do
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

  describe "compose/4 – explainer mode (calm explainer)" do
    test "explainer mode returns compact bullet outline" do
      text = Modes.compose(:explain, :warm, :explainer, %{})

      assert text =~ "Here's the short version of how this works"
      assert text =~ "1) What changes, at a glance"
      assert text =~ "5) When to prefer a full file"
    end
  end
end
