defmodule Core.Semantic.MetalinguisticClaimTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.MetalinguisticClaim, as: Claim

  @source %{
    kind: :uninterpreted,
    status: :no_verified_semantics,
    source_term: "zorple",
    source_turn_sha256: String.duplicate("a", 64),
    observed_at_ms: 1_000,
    seq: 1,
    factual_authority?: false
  }

  defp input(sentence, extra \\ %{}) do
    Map.merge(%{sentence: sentence, metalinguistic_source: @source}, extra)
  end

  test "one bounded sentence pattern yields a replayable speaker claim" do
    for text <- [
          "it is a greeting",
          "it's a greeting.",
          "its a greeting",
          "that is a greeting.",
          "that's a greeting",
          "this is a greeting",
          "this's a greeting"
        ] do
      si = input(text) |> Claim.attach(metalinguistic_claims?: true)
      assert {:ok, claim} = Claim.verified(si)
      assert claim.relation == :classification
      assert claim.source_term == "zorple"
      assert claim.class_term == "greeting"
      assert claim.source_seq == 1
      assert claim.epistemic == %{
        user_claimed: true,
        lexically_verified?: false,
        durably_persisted?: false
      }
      refute claim.factual_authority?
      assert claim.status == :speaker_claimed
    end
  end

  test "absent antecedent, quoted, reported, questions, and multiword labels abstain" do
    assert Claim.detect(%{sentence: "it's a greeting"}) == nil

    for text <- [
          "he said it's a greeting",
          ~s("it's a greeting"),
          "it's a greeting?",
          "it's a friendly greeting.",
          "its a greeting and farewell",
          "no, it's a greeting",
          "this is the greeting",
          "it's a 123",
          "it's a " <> String.duplicate("q", 41),
          "by zorple I mean greeting"
        ] do
      assert Claim.detect(input(text)) == nil, text
    end
  end

  test "the candidate is independent of affect, LIFG activation and confidence" do
    before = Claim.detect(input("it's a greeting"))
    altered = input("it's a greeting", %{
      intent: :unknown, confidence: 1.0,
      mood: %{vigilance: 1.0, inhibition: 1.0},
      emotion: %{threat: 1.0},
      lifg_choices: [%{id: "zorple|noun|1", score: 999.0}]
    })
    assert Claim.detect(altered) == before
  end

  test "replay rejects modified claims, forged context and disabled feature" do
    si = input("it's a greeting")
    assert Claim.attach(si, metalinguistic_claims?: false) == si
    verified = Claim.attach(si, metalinguistic_claims?: true)
    assert {:ok, _} = Claim.verified(verified)
    tampered = put_in(verified, [:metalinguistic_claim, :class_term], "farewell")
    assert Claim.verified(tampered) == :not_applicable
    assert Claim.detect(input("it's a greeting", %{metalinguistic_source: %{}})) == nil
    assert Claim.detect(input("it's a greeting", %{
      metalinguistic_source: %{@source | seq: 0}
    })) == nil
  end

  test "recognized lexical sources cannot become unknown-word markers" do
    # Independently selected lexical evidence must not be discarded simply
    # because the surface was syntactically one word.
    si = %{
      sentence: "dog!",
      tokens: [%{index: 0, n: 1, kind: :word, phrase: "dog"}],
      active_cells: [%{norm: "dog", pos: "noun", id: "dog|noun|1"}],
      speech_act_frame: nil
    } |> Core.Semantic.ProvisionalInterpretation.attach()

    assert si.provisional_interpretation.operation == :ambiguous_topic
    assert Claim.isolated_marker(si, 1_000) == nil
  end

  test "source expiration, overlong or untrusted marker fields abstain" do
    assert Claim.valid_source?(@source, 301_000)
    refute Claim.valid_source?(@source, 301_001)
    refute Claim.valid_source?(@source, 999)
    assert Claim.detect(input("it's a greeting", %{
      metalinguistic_source: Map.put(@source, :extra, "untrusted")
    })) == nil
    assert Claim.detect(input("it's a greeting", %{
      metalinguistic_source: %{@source | source_term: String.duplicate("x", 41)}
    })) == nil
  end
end
