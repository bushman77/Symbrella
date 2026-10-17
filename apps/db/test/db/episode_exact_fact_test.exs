defmodule Db.EpisodeExactFactTest do
  use ExUnit.Case, async: false

  alias Db.Episode

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    :ok
  end

  test "owner-scoped spatial observations choose the newest event across sessions" do
    owner = "owner-#{System.unique_integer([:positive])}"

    for {sid, place} <- [{"first", "a box"}, {"second", "a shelf"}] do
      assert {:ok, _} = Episode.insert(%{
        slate: %{tokens: ["lantern"]},
        meta: %{kind: :user_reported_spatial, subject: :user, owner: owner,
          session_id: sid, object_key: "lantern", object: "copper lantern",
          position: "on", location: place},
        tags: ["user_reported_spatial"]
      })
    end

    assert %{location: "a shelf", provenance: :user_reported} =
             Episode.latest_user_spatial(owner, "lantern")
    assert Episode.latest_user_spatial(owner <> "-other", "lantern") == nil
    assert Episode.latest_user_spatial(owner, "book") == nil
    assert Episode.latest_user_fact("lantern") == nil
  end

  test "latest user fact is selected by exact key rather than recent-episode limit" do
    key = "persistent_name_test_#{System.unique_integer([:positive])}"

    write_fact(key, "Original", :user)

    for i <- 1..205 do
      assert {:ok, _} = Episode.insert(%{
        slate: %{tokens: ["distractor_#{i}"]},
        meta: %{kind: :other},
        tags: ["other"]
      })
    end

    assert Episode.latest_user_fact(key) == "Original"

    write_fact(key, "Updated", :user)
    assert Episode.latest_user_fact(key) == "Updated"
  end

  test "self facts are never returned as user identity" do
    key = "identity_subject_test_#{System.unique_integer([:positive])}"
    write_fact(key, "Symbrella", :symbrella)

    assert Episode.latest_user_fact(key) == nil
  end

  defp write_fact(key, value, subject) do
    assert {:ok, _} =
             Episode.insert(%{
               slate: %{tokens: [key]},
               meta: %{kind: :fact, key: key, value: value, subject: subject},
               tags: ["fact"]
             })
  end
end
