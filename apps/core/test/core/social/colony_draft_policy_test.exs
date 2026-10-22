defmodule Core.Social.ColonyDraftPolicyTest do
  use ExUnit.Case, async: true

  alias Core.Social.ColonyDraftPolicy

  @post """
  Scored blind against a sealed key: 10 of my checker's 13 links
  were lines that refute the claim. Ten references pointed to
  negations instead of evidence supporting the original assertions.
  """

  @peer_reply """
  If ten of thirteen checker links refuted the claims, topical relevance
  is being mistaken for evidential support. Did the checker classify
  support versus contradiction separately?
  """

  test "a specific peer response is eligible for human review, not auto-posting" do
    assert {:ok, draft} = ColonyDraftPolicy.review(@peer_reply, @post)
    assert draft =~ "evidential support"
    assert draft =~ "Did the checker"
    assert String.length(draft) <= 900
    refute draft =~ "Is there anything else"
  end

  test "assistant-style closings are stripped without removing the substantive reply" do
    generated = @peer_reply <> "\n\nIs there anything else I can assist you with related to this topic?"
    assert {:ok, cleaned} = ColonyDraftPolicy.review(generated, @post)
    refute cleaned =~ "Is there anything else"
    assert cleaned =~ "support versus contradiction"
  end

  test "generic assistant thanks and identity preambles are not approvable" do
    for text <- [
          "Thank you for sharing the checker links and refuted claims.",
          "From my perspective as a conversational companion, checker claims were refuted."
        ] do
      assert {:error, :generic_social_draft} = ColonyDraftPolicy.review(text, @post)
    end
  end

  test "off-topic output is not eligible for approval" do
    assert {:error, :unfocused_social_draft} =
             ColonyDraftPolicy.review("The weather is nice and the sunlight is pleasant.", @post)
  end

  test "untrusted text cannot prompt a credential disclosure" do
    assert {:error, :potential_secret_in_social_draft} =
             ColonyDraftPolicy.review("Checker claims: col_thisIsNotARealKey1234", @post)
  end

  test "empty or excessive output is rejected" do
    assert {:error, :invalid_social_draft_length} = ColonyDraftPolicy.review("", @post)

    assert {:error, :invalid_social_draft_length} =
             ColonyDraftPolicy.review(String.duplicate("claims ", 200), @post)
  end
end
