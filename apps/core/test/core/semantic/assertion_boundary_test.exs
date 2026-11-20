defmodule Core.Semantic.AssertionBoundaryTest do
  @moduledoc """
  P-040: each AssertionBoundary profile must equal its source function.
  The oracles are verbatim copies (fd241a5) of SVOEvent.admissible_text?/1,
  ParticipantSpatial.admissible?/1 and the EpisodicEvents.statement/1 gate.
  """
  use ExUnit.Case, async: true

  alias Core.Semantic.AssertionBoundary

  defp svo_oracle(text) do
    t = String.trim(text)

    t != "" and byte_size(t) <= 256 and
      not String.contains?(t, ["?", "\"", "“", "”", ";", ":", "\n"]) and
      not Regex.match?(
        ~r/\b(?:not|never|no|maybe|perhaps|possibly|probably|might|could|would|should|if|unless|apparently|supposedly|reportedly)\b/iu,
        t
      ) and
      not Regex.match?(~r/^(?:i think|i guess|imagine|suppose|according to|remember that)\b/iu, t)
  end

  defp spatial_oracle(text) do
    String.length(text) in 1..256 and
      not String.contains?(text, ["?", "\n", "\"", "“", "”", ";", ":"]) and
      not Regex.match?(
        ~r/\b(?:not|never|maybe|perhaps|possibly|might|could|would|should|if|unless|apparently|reportedly|allegedly|supposedly)\b/iu,
        text
      )
  end

  defp episodic_oracle(raw) do
    s = raw |> String.trim() |> String.trim_trailing(".")
    s = Regex.replace(~r/^(?:actually[, ]+|to be clear[, ]+)/iu, s, "")

    not (Regex.match?(
           ~r/^(?:if|maybe|perhaps|suppose|imagine|i think|i guess|what if|could|would|please|remember that)\b/iu,
           s
         ) or String.contains?(s, ~s(")))
  end

  @words ~w(not never no maybe perhaps possibly probably might could would should if
            unless apparently supposedly reportedly allegedly please imagine suppose)

  defp sentences do
    base = [
      "The dog chased the cat.",
      "The cat is sitting on the mat",
      "",
      "   ",
      "Is the dog here?",
      "He said \"hi\"",
      "The dog; the cat",
      "Note: the dog ran",
      "line one\nline two",
      "Actually, maybe the dog ran.",
      "To be clear, the dog ran.",
      "I think the dog ran.",
      "According to Sam the dog ran.",
      "Remember that the dog ran.",
      "What if the dog ran",
      "Nothing happened.",
      "The dog nonetheless ran.",
      "Notably the dog ran.",
      "The NOT dog",
      "the cat wouldn't move",
      "The café is open",
      String.duplicate("a", 256),
      String.duplicate("a", 257),
      " " <> String.duplicate("a", 256),
      String.duplicate("é", 200)
    ]

    generated =
      for w <- @words, form <- [w, String.capitalize(w), String.upcase(w)] do
        ["#{form} the dog chased the cat", "The dog #{form} chased the cat."]
      end

    base ++ List.flatten(generated)
  end

  test ":svo_v1 equals SVOEvent.admissible_text?/1" do
    for s <- sentences(), do: assert(AssertionBoundary.admissible?(s, :svo_v1) == svo_oracle(s), inspect(s))
  end

  test ":spatial_v1 equals ParticipantSpatial.admissible?/1" do
    for s <- sentences(),
        do: assert(AssertionBoundary.admissible?(s, :spatial_v1) == spatial_oracle(s), inspect(s))
  end

  test ":episodic_prefix_v1 equals the EpisodicEvents.statement/1 gate" do
    for s <- sentences(),
        do:
          assert(
            AssertionBoundary.admissible?(s, :episodic_prefix_v1) == episodic_oracle(s),
            inspect(s)
          )
  end

  test "disagreements/0 exposes the known inconsistencies" do
    by_word = Map.new(AssertionBoundary.disagreements(), &{&1.word, &1})

    assert by_word["no"].anywhere == [:svo_v1]
    assert by_word["probably"].anywhere == [:svo_v1]
    assert by_word["allegedly"].anywhere == [:spatial_v1]
    assert by_word["please"].sentence_initial == [:episodic_prefix_v1]
    # A word every lexical profile blocks anywhere would not be listed;
    # "not" is absent from the episodic gate, so it IS a disagreement.
    assert by_word["not"].anywhere == [:svo_v1, :spatial_v1]
  end

  test "unknown profile or non-binary input is not admissible and never raises" do
    refute AssertionBoundary.admissible?("The dog ran", :unknown)
    refute AssertionBoundary.admissible?(nil, :svo_v1)
    refute AssertionBoundary.admissible?(42, :spatial_v1)
    assert AssertionBoundary.profiles() == [:svo_v1, :spatial_v1, :episodic_prefix_v1]
  end
end
