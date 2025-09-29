defmodule Brain.LIFG.BoundaryGuardTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.BoundaryGuard

  test "keeps only boundary-aligned tokens for a simple sentence" do
    sentence = "kick the bucket"

    tokens = [
      %{phrase: "kick", mw: false},
      %{phrase: "the", mw: false},
      %{phrase: "bucket", mw: false},
      # substring -> drop
      %{phrase: "ick", mw: false},
      # substring -> drop
      %{phrase: "k", mw: false},
      # substring -> drop
      %{phrase: "th", mw: false},
      # explicit MWE -> keep
      %{phrase: "kick the bucket", mw: true}
    ]

    sanitized = BoundaryGuard.sanitize(tokens, sentence)
    phrases = Enum.map(sanitized, & &1.phrase)

    assert "kick" in phrases
    assert "the" in phrases
    assert "bucket" in phrases
    assert "kick the bucket" in phrases

    refute "ick" in phrases
    refute "k" in phrases
    refute "th" in phrases
  end

  test "unicode words respected (naïve café)" do
    sentence = "A naïve café appears."

    tokens = [
      %{phrase: "naïve", mw: false},
      %{phrase: "café", mw: false},
      # substring -> drop
      %{phrase: "afé", mw: false}
    ]

    sanitized = BoundaryGuard.sanitize(tokens, sentence)
    phrases = Enum.map(sanitized, & &1.phrase)

    assert "naïve" in phrases
    assert "café" in phrases
    refute "afé" in phrases
  end

  test "punctuation boundaries are okay (Mr. K walks.)" do
    sentence = "Mr. K walks."

    tokens = [
      %{phrase: "Mr", mw: false},
      %{phrase: "K", mw: false},
      %{phrase: "walks", mw: false},
      # substring -> drop
      %{phrase: "r.", mw: false}
    ]

    sanitized = BoundaryGuard.sanitize(tokens, sentence)
    phrases = Enum.map(sanitized, & &1.phrase)

    assert "Mr" in phrases
    assert "K" in phrases
    assert "walks" in phrases
    refute "r." in phrases
  end
end
