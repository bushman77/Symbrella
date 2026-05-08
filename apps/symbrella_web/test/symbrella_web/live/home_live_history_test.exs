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
