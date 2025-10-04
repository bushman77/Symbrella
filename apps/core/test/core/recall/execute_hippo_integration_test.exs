defmodule Core.Recall.ExecuteHippoIntegrationTest do
  use ExUnit.Case, async: false

  alias Core.SemanticInput, as: SI
  alias Core.Recall.{Execute, Plan}
  alias Brain.Hippocampus

  setup do
    # Ensure Hippocampus server is running and clean
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Hippocampus.reset()
    :ok
  end

  # ---------- helpers ----------

  defp si_with_tokens(norms) when is_list(norms) do
    # Tokens as plain maps so both Core.Execute and Hippocampus can read :norm directly.
    toks =
      norms
      |> Enum.with_index()
      |> Enum.map(fn {n, i} -> %{norm: String.downcase(n), id: i} end)

    %SI{sentence: Enum.join(norms, " "), source: :test, tokens: toks}
  end

  defp slate_with(words) when is_list(words) do
    %{
      winners:
        Enum.map(words, fn w ->
          %{id: "#{w}|noun|0", lemma: String.downcase(w)}
        end)
    }
  end

  defp plan_min() do
    # Only the fields Execute.execute/3 actually reads; provide required :triggers key.
    %Plan{budget_ms: 25, max_items: 4, strategies: [:exact], triggers: []}
  end

  # Small helper to safely read episodes from a struct/map
  defp episodes_of(si) do
    si
    |> Map.get(:evidence)
    |> case do
      nil -> nil
      ev -> Map.get(ev, :episodes)
    end
  end

  # ---------- tests ----------

  test "hippo: true attaches episodes into si.evidence[:episodes]" do
    # Arrange: one overlapping episode ("alpha") and one non-overlapping ("delta")
    Hippocampus.encode(slate_with(["alpha"]), %{scope: :a})
    Hippocampus.encode(slate_with(["delta"]), %{scope: :b})

    si = si_with_tokens(["alpha", "beta"])
    plan = plan_min()

    # Act (explicitly enable hippo to be robust to default)
    si2 = Execute.execute(si, plan, hippo: true)

    # Assert
    episodes = episodes_of(si2)
    assert is_list(episodes)
    assert length(episodes) >= 1

    # Confirm result shape and that at least one episode overlaps "alpha"
    assert Enum.all?(episodes, fn e ->
             is_map(e) and is_number(e.score) and is_integer(e.at) and is_map(e.episode)
           end)

    assert Enum.any?(episodes, fn %{episode: %{slate: %{winners: ws}}} ->
             Enum.any?(ws, &match?(%{lemma: "alpha"}, &1))
           end)

    # Scores should be positive for overlapping episodes
    assert Enum.any?(episodes, fn %{score: s} -> is_float(s) and s > 0.0 end)
  end

  test "hippo: false disables attachment (no :evidence key created)" do
    Hippocampus.encode(slate_with(["alpha"]), %{})

    si = si_with_tokens(["alpha"])
    plan = plan_min()

    si2 = Execute.execute(si, plan, hippo: false)

    assert Map.get(si2, :evidence) == nil
  end

  test "hippo_opts honored: min_jaccard filters out weak overlap" do
    # Episode has only "alpha"
    Hippocampus.encode(slate_with(["alpha"]), %{})

    # Cues contain "alpha" and "beta" → Jaccard = 1 / 2 = 0.5
    si = si_with_tokens(["alpha", "beta"])
    plan = plan_min()

    si2 = Execute.execute(si, plan, hippo: true, hippo_opts: [min_jaccard: 0.9])

    assert episodes_of(si2) == []
  end
end

