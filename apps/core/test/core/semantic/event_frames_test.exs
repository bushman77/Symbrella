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

  test "preserves the speaker as the agent of an obligation" do
    si = %{
      sentence: "I've got to leave.",
      tokens: [
        %{index: 0, phrase: "I've"},
        %{index: 1, phrase: "got"},
        %{index: 2, phrase: "to"},
        %{index: 3, phrase: "leave."}
      ],
      sentence_signatures: [
        %{id: :have_verb_to_infinitive,
          roles: %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3}}
      ],
      lifg_choices: [
        %{token_index: 1, id: "got|verb|1"},
        %{token_index: 3, id: "leave|noun|4"}
      ],
      trace: []
    }

    result = EventFrames.attach(si)
    assert result.symbolic_frame.subject == :speaker
    assert result.symbolic_frame.action == "leave"
    assert result.symbolic_frame.modality == :obligation
    assert result.symbolic_frame.uncertain_roles == [:complement]

    assert get_in(Core.Response.Context.symbolic_frame(result), [:event, :subject]) == :speaker

    context =
      Core.Response.Context.from_response_parts(
        "I've got to leave.",
        %{symbolic_frame: result.symbolic_frame,
          turn_context: Core.Response.Context.from_si(result)},
        %{},
        %{},
        %{}
      )

    assert get_in(context, [:symbolic_frame, :event, :subject]) == :speaker
  end

  test "does not attach frame for unrelated text" do
    si = %{sentence: "can you explain working memory", intent: :ask, trace: []}

    refute Map.has_key?(EventFrames.attach(si), :symbolic_frame)
  end
end
