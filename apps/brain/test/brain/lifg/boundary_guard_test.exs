# apps/brain/test/brain/lifg/boundary_guard_test.exs
defmodule Brain.LIFGBoundaryGuardTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Guard
  alias Brain.LIFG.BoundaryGuard

  @moduletag :lifg

  describe "Guard.sanitize/1 (slate-level guard)" do
    test "drops tokens that do not start/end on word boundaries" do
      sentence = "hello world"

      tokens = [
        # good: starts at 0, ends after 'hello'
        %{norm: "hello", span: {0, 5}},
        # bad: starts inside a word and ends inside a word
        %{norm: "ello wor", span: {1, 9}},
        # good: starts at space+1, ends after 'world'
        %{norm: "world", span: {6, 11}}
      ]

      slate = %{sentence: sentence, tokens: tokens}

      %{tokens: filtered} = Guard.sanitize(slate)

      norms = Enum.map(filtered, & &1.norm)

      assert "hello" in norms
      assert "world" in norms
      refute "ello wor" in norms
    end

    test "keeps mw: true tokens even when they span multiple words" do
      sentence = "hello world"

      tokens = [
        # multiword expression spanning two words, but marked mw: true
        %{norm: "hello world", span: {0, 11}, mw: true}
      ]

      %{tokens: filtered} = Guard.sanitize(%{sentence: sentence, tokens: tokens})

      assert Enum.any?(filtered, fn t ->
               Map.get(t, :norm) == "hello world" and (Map.get(t, :mw) || Map.get(t, "mw"))
             end)
    end

    test "tokens are sorted by start position after Guard.sanitize/1" do
      sentence = "a b c"

      tokens = [
        %{norm: "b", span: {2, 3}},
        %{norm: "a", span: {0, 1}},
        %{norm: "c", span: {4, 5}}
      ]

      %{tokens: filtered} = Guard.sanitize(%{sentence: sentence, tokens: tokens})

      starts =
        filtered
        |> Enum.map(fn t ->
          case Map.get(t, :span) || Map.get(t, "span") do
            {s, _e} -> s
            %{start: s, stop: _} -> s
          end
        end)

      assert starts == Enum.sort(starts)
    end
  end

  describe "BoundaryGuard.sanitize/2 (raw token list guard)" do
    test "drops tokens that do not start/end on word boundaries" do
      sentence = "hello world"

      tokens = [
        # good: starts at 0, ends after 'hello'
        %{phrase: "hello", span: {0, 5}},
        # bad: starts inside 'hello' and ends inside 'world'
        %{phrase: "ello wor", span: {1, 9}},
        # good: starts at 6, ends after 'world'
        %{phrase: "world", span: {6, 11}}
      ]

      filtered = BoundaryGuard.sanitize(tokens, sentence)
      phrases = Enum.map(filtered, & &1.phrase)

      assert "hello" in phrases
      assert "world" in phrases
      refute "ello wor" in phrases
    end

    test "keeps mw: true tokens even when they span multiple words" do
      sentence = "hello world"

      tokens = [
        %{phrase: "hello world", span: {0, 11}, mw: true}
      ]

      filtered = BoundaryGuard.sanitize(tokens, sentence)

      assert Enum.any?(filtered, fn t ->
               t.phrase == "hello world" and (t.mw == true or t.mw == "true")
             end)
    end

    test "tokens are sorted by start position after BoundaryGuard.sanitize/2" do
      sentence = "a b c"

      tokens = [
        %{phrase: "b", span: {2, 3}},
        %{phrase: "a", span: {0, 1}},
        %{phrase: "c", span: {4, 5}}
      ]

      filtered = BoundaryGuard.sanitize(tokens, sentence)

      starts =
        filtered
        |> Enum.map(fn %{span: {s, _e}} -> s end)

      assert starts == Enum.sort(starts)
    end
  end
end

