defmodule SymbrellaWeb.SemanticLiveTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatHistory

  setup %{conn: conn} do
    ChatHistory.clear()
    {:ok, conn: Plug.Test.init_test_session(conn, %{
      "symbrella_chat_session_id" => ChatHistory.session_id()
    })}
  end

  test "renders every SemanticInput field in a report", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/semantic")

    assert has_element?(view, "#semantic-report-form")
    assert has_element?(view, "#semantic-load-latest-chat")
    assert has_element?(view, "#semantic-summary")
    assert has_element?(view, "#semantic-overview")
    assert has_element?(view, "#semantic-field-report")

    for key <- Map.keys(Map.from_struct(%Core.SemanticInput{})) do
      assert has_element?(view, "#semantic-field-#{key}")
    end
  end

  test "analyzes submitted text and updates token report", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/semantic")

    view
    |> form("#semantic-report-form", semantic: %{sentence: "blue quiet lamp", mode: "test"})
    |> render_submit()

    assert has_element?(view, "#semantic-token-0")
    assert has_element?(view, "#semantic-field-sentence")
    assert render(view) =~ "blue quiet lamp"
  end

  test "latest semantic report cannot read a different browser's chat", %{conn: conn} do
    other_id = "s-" <> String.duplicate("b", 32)

    ChatHistory.append(other_id, %{
      id: "private-semantic",
      role: :assistant,
      text: "Private response",
      session_id: other_id,
      si: %Core.SemanticInput{sentence: "private browser conversation", intent: :ask}
    })

    {:ok, _view, html} = live(conn, ~p"/semantic")

    refute html =~ "private browser conversation"
    refute html =~ "private-semantic"
  end

  test "loads the latest chat semantic snapshot instead of the sample input", %{conn: conn} do
    si = %Core.SemanticInput{
      sentence: "good afternoon symbrella",
      source: :prod,
      intent: :greet,
      confidence: 0.92,
      session_id: "s-real",
      tokens: [%{phrase: "good"}, %{phrase: "afternoon"}, %{phrase: "symbrella"}],
      trace: [%{stage: :intent}]
    }

    ChatHistory.append(%{
      id: "b-real",
      role: :assistant,
      text: "Good afternoon.",
      session_id: "s-real",
      si: si
    })

    {:ok, view, html} = live(conn, ~p"/semantic")

    assert html =~ "good afternoon symbrella"
    assert html =~ "Latest chat semantic snapshot b-real"
    assert has_element?(view, "#semantic-token-0")
    refute html =~ "Tell me about your working memory."
  end
end
