defmodule SymbrellaWeb.EpisodesLiveTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias Db.Episode

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    Ecto.Adapters.SQL.Sandbox.mode(Db, {:shared, self()})
    :ok
  end

  test "renders persisted episodes from the database", %{conn: conn} do
    assert {:ok, episode} =
             Episode.insert(%{
               si: %{
                 "sentence" => "a clock that told stories",
                 "intent" => "note"
               },
               tokens: ["clock", "stories"],
               tags: ["test", "curiosity"]
             })

    {:ok, view, html} = live(conn, ~p"/episodes")

    assert html =~ "Episodes"
    assert html =~ "a clock that told stories"
    assert has_element?(view, "#episodes-count")
    assert has_element?(view, "#episodes-refresh")
    assert has_element?(view, "#episodes-list")
    assert has_element?(view, "#episode-copy-button-#{episode.id}")
    assert has_element?(view, "#episode-copy-#{episode.id}")
    assert html =~ ~s(phx-hook="ClipboardCopy")
    assert html =~ ~s(data-clipboard-target="#episode-copy-#{episode.id}")
  end
end
