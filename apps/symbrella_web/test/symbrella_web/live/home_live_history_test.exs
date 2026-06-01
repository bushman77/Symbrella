defmodule SymbrellaWeb.HomeLiveHistoryTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatHistory

  setup do
    ChatHistory.clear()

    if Code.ensure_loaded?(Brain.Hippocampus) do
      Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
      Brain.Hippocampus.reset()
    end

    :ok
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
    assert Enum.any?(section.items, &(&1.label == "memory source" and &1.body == ":hippocampus_fact"))
  end

  test "chat stores and recalls direct user facts before attached LLM responses", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "my sisters name is Mary-Anne"})

    Process.sleep(300)
    html = render(view)
    assert html =~ "I’ve noted that your sisters name is Mary-Anne."

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "what is my sisters name"})

    Process.sleep(300)
    html = render(view)
    assert html =~ "Your sisters name is Mary-Anne."
  end

  test "chat stores location facts and does not append curiosity about the recall question", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "i live in Richmond, BC, please remeber that"})

    Process.sleep(300)
    html = render(view)
    assert html =~ "I’ve noted that your location is Richmond, BC."

    view
    |> element("#chat-form")
    |> render_submit(%{"message" => "where do i live"})

    Process.sleep(300)
    html = render(view)
    assert html =~ "Your location is Richmond, BC."
    refute html =~ "Curiosity check:"
    refute html =~ "Should I treat that as a feature idea"
  end
end
