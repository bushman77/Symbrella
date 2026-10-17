defmodule SymbrellaWeb.HomeLiveHistoryTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatHistory

  setup %{conn: conn} do
    ChatHistory.clear()

    if Code.ensure_loaded?(Brain.Hippocampus) do
      Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
      Brain.Hippocampus.reset()
    end

    {:ok, conn: Plug.Test.init_test_session(conn, %{
      "symbrella_chat_session_id" => ChatHistory.session_id()
    })}
  end

  test "renders cached chat history on mount", %{conn: conn} do
    ChatHistory.append([
      %{id: "u-cached", role: :user, text: "cached user message"},
      %{id: "b-cached", role: :assistant, text: "cached assistant reply"}
    ])

    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "cached user message"
    assert html =~ "cached assistant reply"
    refute html =~ "Welcome to Symbrella chat"
  end

  test "restores cached assistant explain payload after remount", %{conn: conn} do
    ChatHistory.append(%{
      id: "b-explain",
      role: :assistant,
      text: "cached assistant reply",
      explain_text: "cached assistant reply\n\nintent=:question conf=0.91 tone=:warm",
      explain_payload: %{
        title: "Explain",
        subtitle: "intent=:question · conf=0.91 · tone=:warm",
        sections: [
          %{
            title: "Intent & tone",
            items: [
              %{label: "Intent", body: "question"},
              %{label: "Confidence", body: "0.91"},
              %{label: "Tone", body: "warm"}
            ]
          }
        ]
      }
    })

    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("button[phx-value-id='b-explain']", "Explain")
    |> render_click()

    html = render(view)
    assert html =~ "Intent &amp; tone"
    assert html =~ "Confidence"
    assert html =~ "0.91"
  end

  test "cached Explain preserves verified phrasal meaning and actual degraded state", %{conn: conn} do
    alias SymbrellaWeb.HomeLive.ExplainEvidence
    alias SymbrellaWeb.HomeLive.HTML.Modal

    input = %{
      intent: :command,
      confidence: 0.82,
      selected_action: :answer_user,
      lifg_choices: [
        %{id: "power|verb|1", token_index: 0, score: 0.04},
        %{id: "computer|noun|3", token_index: 2, score: 0.20}
      ],
      semantic_choices: [
        %{id: "power|verb|1", token_index: 0, score: 0.04},
        %{id: "computer|noun|3", token_index: 2, score: 0.20}
      ],
      lexical_constructions: [
        %{id: "power down|verb|1", phrase: "power down",
          definition: "To switch off (something electrical).",
          source: :verified_discontinuous_mwe,
          dictionary_verified?: true,
          context_score: 0.145,
          constituent_indices: [0, 3], object_index: 2}
      ],
      comprehension: %{
        understood: ["computer", "power down"],
        uncertain: [], degraded?: true, reasons: [:weak_decision_rate_high]
      }
    }

    payload =
      Modal.explain_payload_for(%{
        text: "The computer is already powered down.",
        intent: :command,
        confidence: 0.82,
        cognition: ExplainEvidence.from_si(input)
      })

    ChatHistory.append(%{
      id: "b-cognition-parity",
      role: :assistant,
      text: "The computer is already powered down.",
      explain_text: "The computer is already powered down.",
      explain_payload: payload
    })

    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("button[phx-value-id='b-cognition-parity']", "Explain")
    |> render_click()

    html = render(view)
    assert html =~ "Verified phrase meanings"
    assert html =~ "power down|verb|1"
    assert html =~ "computer|noun|3"
    assert html =~ "Canonical semantic winners"
    assert html =~ "weak_decision_rate_high"
    assert html =~ "The computer is already powered down."
  end

  test "remount upgrades an old cached Explain payload from its saved SI", %{conn: conn} do
    ChatHistory.append(%{
      id: "b-legacy-semantics",
      role: :assistant,
      text: "The computer is already powered down.",
      explain_text: "The computer is already powered down.",
      si: %{
        intent: :command,
        confidence: 0.82,
        semantic_choices: [%{id: "computer|noun|3", token_index: 2}],
        lifg_choices: [%{id: "computer|noun|3", token_index: 2}],
        lexical_constructions: [
          %{id: "power down|verb|1", phrase: "power down",
            definition: "To switch off something electrical.",
            dictionary_verified?: true, source: :verified_discontinuous_mwe,
            context_score: 0.145, object_index: 2, constituent_indices: [0, 3]}
        ],
        comprehension: %{
          understood: ["computer", "power down"],
          uncertain: [], degraded?: true, reasons: [:weak_decision_rate_high]
        }
      },
      explain_payload: %{
        title: "Explain",
        sections: [
          %{key: :intent_tone, title: "Intent & tone",
            items: [%{label: "tone", body: ":warm"}]},
          %{key: :message, title: "Message",
            items: [%{label: "content", body: "The computer is already powered down."}]}
        ]
      }
    })

    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("button[phx-value-id='b-legacy-semantics']", "Explain")
    |> render_click()

    html = render(view)
    assert html =~ "Verified phrase meanings"
    assert html =~ "power down|verb|1"
    assert html =~ "computer|noun|3"
    assert html =~ "weak_decision_rate_high"
    assert html =~ "Intent &amp; tone"
  end

  test "Explain shows actual post-draft LLM repair without confusing it with Core's action" do
    payload =
      SymbrellaWeb.HomeLive.HTML.Modal.explain_payload_for(%{
        text: "I understand that you're asking me to power down the device.",
        intent: :command,
        selected_action: :answer_user,
        action_meta: %{safety_gate: :approved},
        from: %{
          response_source: :llm,
          response_reflection: %{
            status: :repair,
            issues: [:unsupported_device_refusal],
            applied?: true,
            repair_count: 1,
            critique: "Draft required bounded repair.",
            draft_sha256: "hash-before",
            final_sha256: "hash-after"
          }
        }
      })

    review = Enum.find(payload.sections, &(&1.key == :response_reflection))
    assert review.title == "LLM response review"
    assert review.tag == ":repair"
    assert Enum.any?(review.items,
             &(&1.label == "issues" and &1.body =~ "unsupported_device_refusal"))
    assert Enum.any?(review.items,
             &(&1.label == "applied?" and &1.body in ["true", ":true"]))
    assert Enum.any?(review.items,
             &(&1.label == "repair_count" and &1.body == "1"))
    assert Enum.any?(review.items,
             &(&1.label == "draft SHA-256" and &1.body == "hash-before"))

    action = Enum.find(payload.sections, &(&1.key == :action_selection))
    assert action.tag == ":answer_user"

    untouched =
      SymbrellaWeb.HomeLive.HTML.Modal.explain_payload_for(%{
        text: "Your name is Mary.",
        from: %{response_source: :memory}
      })

    refute Enum.any?(untouched.sections, &(&1.key == :response_reflection))
  end

  test "explain payload includes action selection agency surface" do
    payload =
      SymbrellaWeb.HomeLive.HTML.Modal.explain_payload_for(%{
        id: "b-action",
        text: "Please contact your pharmacist about the missed medication.",
        intent: :health_support,
        confidence: 0.85,
        tone: :warm,
        symbolic_frame: %{
          type: :health_support_event,
          event: :forgot_medication,
          medication: "quetiapine"
        },
        selected_action: :safe_support,
        action_candidates: [
          %{action: :safe_support, score: 0.94, reason: :health_boundary},
          %{action: :store_memory, score: 0.12, reason: :low_relevance}
        ],
        action_meta: %{selected: :safe_support, safety_gate: :approved, confidence: 0.94}
      })

    action_section =
      Enum.find(payload.sections, fn section -> section.key == :action_selection end)

    assert Enum.count(payload.sections, &(&1.key == :action_selection)) == 1
    assert action_section.title == "Action selection"
    assert action_section.tag == ":safe_support"

    assert Enum.any?(
             action_section.items,
             &(&1.label == "selected action" and &1.body == ":safe_support")
           )

    assert Enum.any?(
             action_section.items,
             &(&1.label == "safety gate" and &1.body == ":approved")
           )

    assert Enum.any?(
             action_section.items,
             &(&1.label == "candidates" and &1.body =~ ":store_memory")
           )
  end

  test "explain payload attributes memory name replies to memory source" do
    payload =
      SymbrellaWeb.HomeLive.HTML.Modal.explain_payload_for(%{
        id: "b-memory",
        text: "Your name is Bradley.",
        intent: :name_query,
        confidence: 1.0,
        tone: :warm,
        from: %{
          action: :identity,
          intent_inferred: :name_query,
          response_source: :memory,
          memory_key: :user_name,
          memory_source: :hippocampus_fact
        }
      })

    section = Enum.find(payload.sections, fn section -> section.key == :intent_tone end)

    assert section.title == "Intent & tone"
    assert section.hint == "Memory attribution and tone metadata that shaped the reply."

    assert Enum.any?(section.items, &(&1.label == "intent" and &1.body == ":name_query"))
    assert Enum.any?(section.items, &(&1.label == "response source" and &1.body == ":memory"))
    assert Enum.any?(section.items, &(&1.label == "memory key" and &1.body == ":user_name"))

    assert Enum.any?(
             section.items,
             &(&1.label == "memory source" and &1.body == ":hippocampus_fact")
           )
  end

  test "chat stores and recalls direct user facts before attached LLM responses", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "my sisters name is Mary-Anne"})

    html = wait_for_reply(view)
    assert html =~ "I’ve noted that your sisters name is Mary-Anne."

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "what is my sisters name"})

    html = wait_for_reply(view)
    assert html =~ "Your sisters name is Mary-Anne."
  end

  test "chat recalls the user's name without using assistant identity", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "my name is Bradley"})

    html = wait_for_reply(view)
    assert html =~ "Nice to meet you, Bradley. I’ll remember that."

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "what is my name?"})

    html = wait_for_reply(view)
    assert html =~ "Your name is Bradley."
    refute html =~ "Your name is Symbrella."
  end

  test "chat does not accept assistant identity as the user's name", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "my name is Symbrella"})

    html = wait_for_reply(view)

    assert html =~
             "I should not store Symbrella as your name because that is my assistant identity."

    refute html =~ "Nice to meet you, Symbrella"

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "what is my name?"})

    html = wait_for_reply(view)
    assert html =~ "I don’t know your name yet"
    refute html =~ "Your name is Symbrella."
  end

  test "cached assistant identity leak is not rendered", %{conn: conn} do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Bradley|proper|0", lemma: "Bradley"}]},
      %{
        kind: :fact,
        key: :user_name,
        value: "Bradley",
        subject: :user,
        tags: ["fact", "user_name"]
      }
    )

    ChatHistory.append([
      %{id: "u-name", role: :user, text: "what is my name?"},
      %{id: "b-bad-name", role: :assistant, text: "Your name is Symbrella."}
    ])

    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "Your name is Bradley."
    refute html =~ "Your name is Symbrella."
  end

  test "cached assistant name-question echo is rewritten", %{conn: conn} do
    ChatHistory.append([
      %{id: "u-name", role: :user, text: "what is my name?"},
      %{id: "b-parrot-name", role: :assistant, text: "Symbrella: What is your name?"}
    ])

    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "I don’t know your name yet"
    refute html =~ "Symbrella: What is your name?"
  end

  test "chat stores location facts and does not append curiosity about the recall question", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "i live in Richmond, BC, please remeber that"})

    html = wait_for_reply(view)
    assert html =~ "I’ve noted that your location is Richmond, BC."

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "where do i live"})

    html = wait_for_reply(view)
    assert html =~ "Your location is Richmond, BC."
    refute html =~ "Curiosity check:"
    refute html =~ "Should I treat that as a feature idea"
  end

  # Wait for the asynchronous HomeLive task to finish.
  # Unlike a fixed sleep, this observes actual UI state.
  defp wait_for_reply(view, attempts \\ 200)

  defp wait_for_reply(view, 0) do
    html = render(view)

    refute html =~ ~s(id="typing"),
           "HomeLive reply did not finish within 10 seconds"

    html
  end

  defp wait_for_reply(view, attempts) do
    html = render(view)

    if String.contains?(html, ~s(id="typing")) do
      Process.sleep(50)
      wait_for_reply(view, attempts - 1)
    else
      html
    end
  end
end
