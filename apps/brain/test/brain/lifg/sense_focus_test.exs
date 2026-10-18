defmodule Brain.LIFG.SenseFocusTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.{SenseFocus, Stage1}

  @adjective_role %{
    token_index: 0, pos: "adjective", status: :supported, role: :modifier,
    basis: :complete_determiner_adjective_noun,
    evidence: [:bounded_determiner_surface, :dictionary_pos_witness,
               :complete_nominal_constituent]
  }

  defp adjudicated_si(role \\ @adjective_role) do
    %{role_competition: %{
      source: :core_syntax_and_attested_dictionary_pos,
      roles: %{0 => role}
    }}
  end

  defp candidates do
    [
      %{id: "light|adjective|1", pos: "adjective", norm: "light", activation: 0.5,
        definition: "Not heavy; weighing relatively little."},
      %{id: "light|adjective|2", pos: "adjective", norm: "light", activation: 0.5,
        definition: "Bright, not dark."},
      %{id: "light|noun|7", pos: "noun", norm: "light", activation: 0.5,
        definition: "Knowledge or ideas."}
    ]
  end

  test "focused pass evaluates contextual evidence within existing POS without removing candidates" do
    original = candidates()
    plan = SenseFocus.plan(adjudicated_si(), 0, original)

    assert plan == %{
      mode: :focused,
      reason: :adjudicated_structural_pos,
      focus_pos: "adjective",
      candidates: 3,
      context_evaluations: 2,
      context_skipped: 1
    }

    assert Enum.map(original, &SenseFocus.evaluate_context?(plan, &1)) ==
      [true, true, false]

    # A grammatical role does not itself select any particular adjective.
    refute Map.has_key?(plan, :selected_sense_id)
    assert original == candidates()
  end

  test "ambiguous or untrusted evidence retains full broad scoring" do
    original = candidates()

    for context <- [
      %{},
      %{role_competition: %{roles: %{0 => @adjective_role}}},
      adjudicated_si(%{@adjective_role | status: :unresolved}),
      adjudicated_si(%{@adjective_role | evidence: []}),
      adjudicated_si(%{@adjective_role | basis: :unknown}),
      adjudicated_si(%{@adjective_role | pos: "noun"})
    ] do
      plan = SenseFocus.plan(context, 0, original)
      assert plan.mode == :broad
      assert plan.context_evaluations == 3
      assert plan.context_skipped == 0
      assert Enum.all?(original, &SenseFocus.evaluate_context?(plan, &1))
    end

    assert SenseFocus.plan(adjudicated_si(), 0, original, enabled?: false).mode == :broad
    assert SenseFocus.plan(adjudicated_si(), 0, original, mwe?: true).mode == :broad

    all_adjectives = Enum.take(original, 2)
    assert SenseFocus.plan(adjudicated_si(), 0, all_adjectives).mode == :broad
  end

  test "actual Stage1 preserves all candidate probabilities while reducing contextual evaluations" do
    si =
      adjudicated_si()
      |> Map.merge(%{
        sentence: "a light suitcase",
        tokens: [%{index: 0, phrase: "light", n: 1, mw: false, span: {2, 7}}],
        active_cells: candidates(),
        sense_candidates: %{0 => candidates()}
      })

    assert {:ok, %{choices: [focused], audit: focused_audit}} =
      Stage1.run(si, scores: :all, preserve_sense_candidates: true,
        sense_focus?: true, semantic_control?: false)

    assert focused.sense_focus.mode == :focused
    assert focused.sense_focus.focus_pos == "adjective"
    assert focused.sense_focus.context_evaluations == 2
    assert focused.sense_focus.context_skipped == 1
    assert focused_audit.sense_focus.focused_tokens == 1
    assert focused_audit.sense_focus.context_evaluations == 2
    assert focused_audit.sense_focus.context_skipped == 1
    assert Map.keys(focused.scores) |> Enum.sort() ==
      Enum.map(candidates(), & &1.id) |> Enum.sort()
    assert_in_delta Enum.sum(Map.values(focused.scores)), 1.0, 1.0e-5

    assert {:ok, %{choices: [broad], audit: broad_audit}} =
      Stage1.run(si, scores: :all, preserve_sense_candidates: true,
        sense_focus?: false, semantic_control?: false)

    assert broad.sense_focus.mode == :broad
    assert broad.sense_focus.reason == :disabled
    assert broad_audit.sense_focus.focused_tokens == 0
    assert broad_audit.sense_focus.context_evaluations == 3
    assert broad_audit.sense_focus.context_skipped == 0
    assert Map.keys(broad.scores) |> Enum.sort() ==
      Map.keys(focused.scores) |> Enum.sort()
  end
end
