defmodule Db.EpisodeExactFactTest do
  use ExUnit.Case, async: false

  alias Db.Episode

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    :ok
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
