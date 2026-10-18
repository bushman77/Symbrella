defmodule Brain.Synapses.ReceiptStoreTest do
  use ExUnit.Case, async: false

  alias Brain.Synapses.Shadow

  setup do
    dir =
      Path.join(System.tmp_dir!(),
        "symbrella-synapses-#{System.unique_integer([:positive, :monotonic])}")

    on_exit(fn -> File.rm_rf(dir) end)

    {:ok, dir: dir}
  end

  defp start_store(dir) do
    {:ok, pid} = Shadow.start_link(name: nil, durable?: true, dir: dir)
    pid
  end

  defp observation(patient, evidence) do
    %{
      provenance: :user_reported,
      session_id: "user-session-A",
      evidence: evidence,
      edges: [
        {"draws|verb|1", "artist|noun|1", :actor},
        {"draws|verb|1", patient <> "|noun|1", :patient}
      ]
    }
  end

  defp review(evidence, verdict) do
    %{
      provenance: :human_review,
      evidence: evidence,
      reviewer: "reviewer-with-sensitive-name",
      decision: verdict
    }
  end

  test "individual receipts, review provenance and weights survive BEAM process restarts",
       %{dir: dir} do
    first = observation("picture", "sentence-one-my-private-text")
    second = observation("diagram", "sentence-two-other-context")

    a = start_store(dir)
    assert Shadow.snapshot(a).persistence_status == :ready
    assert {:ok, :recorded} = Shadow.observe(first, a)
    assert {:ok, :recorded} = Shadow.observe(second, a)
    assert {:ok, :confirmed} = Shadow.review(review(first.evidence, :confirm), a)
    assert %{experiences: 2, confirmed_experiences: 1, connections: 3,
             persisted?: true} = Shadow.snapshot(a)

    actor = Shadow.edge("draws|verb|1", "artist|noun|1", :actor, a)
    assert_in_delta actor.weight, 0.04, 1.0e-8
    assert_in_delta actor.provisional_weight, 0.01, 1.0e-8
    assert_in_delta actor.reviewed_weight, 0.03, 1.0e-8
    assert actor.provisional_experiences == 1
    assert actor.reviewed_experiences == 1
    assert actor.experiences == 2

    assert {:ok, json} = File.read(Path.join(dir, "receipts.json"))
    refute String.contains?(json, "sentence-one-my-private-text")
    refute String.contains?(json, "reviewer-with-sensitive-name")
    refute String.contains?(json, "user-session-A")
    assert byte_size(File.read!(Path.join(dir, "secret.key"))) == 32
    assert {:ok, %{"payload" => payload}} = Jason.decode(json)
    assert {:ok, %{"receipts" => records}} = Jason.decode(payload)
    assert length(records) == 2
    assert Enum.all?(records, fn record ->
      is_integer(record["first_seen_ms"]) and
        byte_size(record["first_session_digest"]) == 64 and
        record["provenance"] == "user_reported_structural"
    end)

    GenServer.stop(a)
    b = start_store(dir)

    assert %{experiences: 2, confirmed_experiences: 1, connections: 3,
             durable_review_events: 1, persisted?: true} = Shadow.snapshot(b)
    assert Shadow.edge("draws|verb|1", "artist|noun|1", :actor, b) == actor
    assert {:ok, :duplicate} = Shadow.observe(first, b)

    assert {:ok, :rejected} = Shadow.review(review(first.evidence, :reject), b)
    assert %{experiences: 1, rejected_experiences: 1, connections: 2} =
             Shadow.snapshot(b)
    assert Shadow.edge("draws|verb|1", "picture|noun|1", :patient, b) == nil
    assert %{weight: other_weight, experiences: 1} =
             Shadow.edge("draws|verb|1", "artist|noun|1", :actor, b)
    assert_in_delta other_weight, 0.01, 1.0e-8

    GenServer.stop(b)
    c = start_store(dir)
    assert %{experiences: 1, rejected_experiences: 1,
             persistence_status: :ready} = Shadow.snapshot(c)
    assert {:ok, :duplicate} = Shadow.observe(first, c)
    assert Shadow.edge("draws|verb|1", "picture|noun|1", :patient, c) == nil
    assert {:ok, :confirmed} = Shadow.review(review(first.evidence, :confirm), c)
    assert %{weight: revived_weight} =
             Shadow.edge("draws|verb|1", "picture|noun|1", :patient, c)
    assert_in_delta revived_weight, 0.03, 1.0e-8
    GenServer.stop(c)
  end

  test "invalid authenticated snapshot fails closed without overwriting saved evidence",
       %{dir: dir} do
    a = start_store(dir)
    assert {:ok, :recorded} =
             Shadow.observe(observation("canvas", "signed-evidence"), a)
    GenServer.stop(a)

    path = Path.join(dir, "receipts.json")
    assert :ok = File.write(path, File.read!(path) <> "tampering")
    before = File.read!(path)

    b = start_store(dir)
    assert %{persistence_status: :unavailable, persisted?: false} =
             Shadow.snapshot(b)
    assert {:error, :storage_unavailable} =
             Shadow.observe(observation("poster", "new-evidence"), b)
    assert {:error, :storage_unavailable} =
             Shadow.review(review("signed-evidence", :reject), b)
    assert File.read!(path) == before
    GenServer.stop(b)
  end

  test "failed durable commit cannot affect visible synaptic state", %{dir: dir} do
    a = start_store(dir)
    prior = observation("poster", "accepted-prior")
    assert {:ok, :recorded} = Shadow.observe(prior, a)
    before = Shadow.snapshot(a)
    weight_before = Shadow.edge("draws|verb|1", "poster|noun|1", :patient, a)

    # Replace the store directory with a regular file to force a
    # deterministic write failure independently of OS permission quirks.
    assert {:ok, _} = File.rm_rf(dir)
    :ok = File.write(dir, "blocked")

    assert {:error, :persistence_failed} =
             Shadow.observe(observation("mural", "new-when-disk-blocked"), a)
    assert Shadow.snapshot(a) == before
    assert Shadow.edge("draws|verb|1", "poster|noun|1", :patient, a) ==
             weight_before
    GenServer.stop(a)
  end

  test "review history is bounded, preserved, and cannot silently truncate",
       %{dir: dir} do
    a = start_store(dir)
    ev = "eight-reviewed-decisions"
    assert {:ok, :recorded} = Shadow.observe(observation("atlas", ev), a)

    for _ <- 1..4 do
      assert {:ok, :confirmed} = Shadow.review(review(ev, :confirm), a)
      assert {:ok, :rejected} = Shadow.review(review(ev, :reject), a)
    end

    assert {:error, :review_capacity} =
             Shadow.review(review(ev, :confirm), a)

    assert %{rejected_experiences: 1} = Shadow.snapshot(a)
    GenServer.stop(a)

    b = start_store(dir)
    assert %{rejected_experiences: 1, persistence_status: :ready} =
             Shadow.snapshot(b)
    assert {:error, :review_capacity} =
             Shadow.review(review(ev, :confirm), b)
    GenServer.stop(b)
  end

  test "missing key does not silently regenerate the prior snapshot", %{dir: dir} do
    a = start_store(dir)
    assert {:ok, :recorded} =
             Shadow.observe(observation("poster", "lost-key-evidence"), a)
    GenServer.stop(a)
    assert :ok = File.rm(Path.join(dir, "secret.key"))

    b = start_store(dir)
    assert %{persistence_status: :unavailable} = Shadow.snapshot(b)
    assert {:error, :storage_unavailable} =
             Shadow.observe(observation("poster", "lost-key-evidence"), b)
    GenServer.stop(b)
  end
end
