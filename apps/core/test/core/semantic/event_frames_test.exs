defmodule Core.Semantic.EventFramesTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.EventFrames

  test "attaches forgot-medication frame with sleep inability consequence" do
    si = %{
      sentence: "good morning symbrella, i forgot my quetiapine and now i can not sleep",
      intent: :health_support,
      confidence: 0.85,
      topic_domain: :health_sleep_medication,
      lifg_choices: [
        %{token_index: 3, lemma: "quetiapine", id: "quetiapine|entity|medication", pos: "entity"}
      ],
      trace: []
    }

    out = EventFrames.attach(si)

    assert Map.drop(out.symbolic_frame, [:confidence]) == %{
             type: :health_support_event,
             subject: :user,
             event: :forgot_medication,
             medication: "quetiapine",
             consequence: :sleep_inability,
             temporal_context: :now,
             domain: :health_support,
             polarity: :negative
           }

    assert out.symbolic_frame.confidence == 0.95

    assert [%{stage: :event_frame, decision: :attached} | _] = out.trace
  end

  test "does not attach frame for unrelated text" do
    si = %{sentence: "can you explain working memory", intent: :ask, trace: []}

    refute Map.has_key?(EventFrames.attach(si), :symbolic_frame)
  end
end
