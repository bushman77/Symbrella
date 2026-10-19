defmodule Brain.SemanticPulseTest do
  use ExUnit.Case, async: false

  alias Brain.SemanticPulse

  @id "draws|verb|1"

  test "per-session lemma pulse decays with the same half-life as BrainCells" do
    t = 100_000
    pulses = SemanticPulse.put(%{}, "a", "draw", @id, 0.03, t)

    {a, pulses} = SemanticPulse.snapshot(pulses, "a", t)
    assert_in_delta a["draw"], 0.03, 1.0e-8

    half = Brain.Cell.Activation.half_life_ms()
    {later, _} = SemanticPulse.snapshot(pulses, "a", t + half)
    assert_in_delta later["draw"], 0.015, 1.0e-8
  end

  test "a signal for one conversation never appears in another" do
    state = SemanticPulse.put(%{}, "session-a", "draw", @id, 0.03, 500)
    {other, state} = SemanticPulse.snapshot(state, "session-b", 501)
    assert other == %{}
    {same, _} = SemanticPulse.snapshot(state, "session-a", 501)
    assert same["draw"] > 0
    assert elem(SemanticPulse.snapshot(state, nil, 501), 0) == %{}
  end

  test "no sense equivalence is created; only validated inflections can pulse" do
    for {lemma, id} <- [
      {"draw", "draw|verb|1"},   # same surface is not a bridge
      {"bank", "bank|noun|10"},  # not a verb
      {"draw", "draws|verb|fallback"},
      {"fake sense", @id}
    ] do
      assert SemanticPulse.put(%{}, "a", lemma, id, 0.03, 500) == %{}
    end

    assert SemanticPulse.put(%{}, nil, "draw", @id, 0.03, 500) == %{}
    assert SemanticPulse.put(%{}, "a", "draw", @id, 0.0, 500) == %{}
  end

  test "bounded accumulation cannot exceed the routing signal cap" do
    pulses =
      Enum.reduce(1..100, %{}, fn i, acc ->
        SemanticPulse.put(acc, "one", "draw", @id, 0.03, 100_000 + i)
      end)

    {snapshot, _} = SemanticPulse.snapshot(pulses, "one", 100_100)
    assert snapshot["draw"] <= 0.03
    assert map_size(pulses) == 1
  end

  test "absolute expiry is not extended by repeated snapshot reads" do
    t = 100_000
    initial = SemanticPulse.put(%{}, "a", "draw", @id, 0.03, t)
    {_, refreshed} = SemanticPulse.snapshot(initial, "a", t + 1_000)
    {_, refreshed} = SemanticPulse.snapshot(refreshed, "a", t + 2_000)
    {expired, storage} = SemanticPulse.snapshot(refreshed, "a", t + 30_100)
    assert expired == %{}
    assert storage == %{}
  end

  test "Brain coordinator exposes each session's lemma routing evidence separately" do
    assert is_pid(Process.whereis(Brain))
    session = "pulse-test-#{System.unique_integer([:positive])}"
    another = session <> "-other"

    assert :ok = Brain.semantic_pulse(session, "draw", @id, 0.03)
    assert %{"draw" => value} = Brain.semantic_snapshot(session)
    assert value > 0 and value <= 0.03
    assert Brain.semantic_snapshot(another) == %{}

    # Semantic routing does not create a new BrainCell or forge an exact sense.
    refute Map.has_key?(Brain.activation_snapshot(), "draw|verb|semantic")
  end

  test "memory is bounded globally and by session without new processes" do
    entries =
      Enum.reduce(1..200, %{}, fn i, acc ->
        session = "session-#{i}"
        SemanticPulse.put(acc, session, "draw", @id, 0.01, 100_000)
      end)

    assert map_size(entries) <= 128

    own =
      ~w(draw paint dance sing walk jump kick leap glance shout talk hum)
      |> Enum.reduce(%{}, fn lemma, acc ->
        SemanticPulse.put(acc, "same", lemma, @id, 0.01, 100_000)
      end)

    {session_pulses, _} = SemanticPulse.snapshot(own, "same", 100_000)
    assert map_size(session_pulses) <= 8
  end
end
