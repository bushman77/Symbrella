defmodule Brain.LIFG.ReanalysisTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Reanalysis

  defp c(id, score), do: %{id: id, score: score}

  test "accepts top candidate when it fits" do
    cands = [c("A", 0.9), c("B", 0.7), c("C", 0.4)]
    fit? = fn cand, _si -> cand.id == "A" end

    assert {:ok, %{id: "A"}, trace} =
             Reanalysis.pick(cands, %{}, fit?: fit?, max_flips: 2)

    assert trace == [accept: "A"]
  end

  test "flips to next-best when top fails (within flip budget)" do
    cands = [c("A", 0.9), c("B", 0.8), c("C", 0.2)]
    fit? = fn cand, _si -> cand.id == "B" end

    assert {:ok, %{id: "B"}, trace} =
             Reanalysis.pick(cands, %{}, fit?: fit?, max_flips: 2)

    assert trace == [reject: "A", accept: "B"]
  end

  test "fails with :no_fit when none pass within flip budget" do
    cands = [c("A", 0.9), c("B", 0.8), c("C", 0.7)]
    fit? = fn _cand, _si -> false end

    assert {:error, :no_fit, trace} =
             Reanalysis.pick(cands, %{}, fit?: fit?, max_flips: 2)

    # We tried A then B and stopped (budget = 2 rejects)
    assert trace == [reject: "A", reject: "B"]
  end

  test "handles empty list" do
    assert {:error, :no_candidates, _trace} =
             Reanalysis.pick([], %{}, fit?: fn _, _ -> true end)
  end
end
