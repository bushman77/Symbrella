defmodule Brain.HippocampusTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus

  setup do
    # Ensure server is running for tests (safe even if it’s already started)
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  test "encode/2 is pass-through and records last/window" do
    slate = %{winners: [%{id: "hello|interjection|6", token_index: 1, lemma: "hello"}]}
    assert Hippocampus.encode(slate, %{source: :atl}) == slate

    s = Hippocampus.snapshot()
    assert s.last != nil
    assert length(s.window) == 1
    {_, ep} = hd(s.window)
    assert ep.slate == slate
    assert ep.meta == %{source: :atl}
  end

  test "window_keep is respected (rolling cap)" do
    Hippocampus.configure(window_keep: 2)
    for i <- 1..3 do
      Hippocampus.encode(%{winners: [%{id: "w|" <> Integer.to_string(i) <> "|0"}]}, %{})
    end
    s = Hippocampus.snapshot()
    assert s.window_keep == 2
    assert length(s.window) == 2
  end

  test "configure accepts :keep alias" do
    Hippocampus.configure(keep: 5)
    s = Hippocampus.snapshot()
    assert s.window_keep == 5
    assert s.opts.window_keep == 5
  end
end

