defmodule Core.Discourse.NominalFocusContinuityTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Discourse.References
  alias Core.Semantic.Composition
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp session do
    "nominal-focus-#{System.unique_integer([:positive])}"
  end

  defp original(sid) do
    words = ~w(The small black bird)
    positions = ~w(determiner adjective adjective noun)

    rows =
      Enum.zip(words, positions)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos, semantic_atoms: []}
      end)

    %Core.SemanticInput{
      session_id: sid,
      sentence: "The small black bird.",
      source: :test,
      intent: :unknown,
      tokens:
        words
        |> Enum.with_index()
        |> Enum.map(fn {word, index} -> %{phrase: word, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices:
        rows
        |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{token_index: index, id: row.id} end)
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  defp comprehension do
    %{recognized_topics: [], understood: [], uncertain: [],
      degraded?: false}
  end

  defp recent_context(sid) do
    si = original(sid)
    assert {:ok, _proof} = Composition.verified(si)

    WMComprehension.context_from(si, comprehension())
  end

  test "verified noun description becomes a bounded, nonfactual WM referent" do
    sid = session()
    context = recent_context(sid)

    assert context.entities == ["small black bird"]
    assert context.nominal_focus == %{
      kind: :nominal_description,
      status: :provisional,
      head: "bird",
      modifiers: ["small", "black"],
      surface: "small black bird",
      source: :revalidated_nominal_composition,
      scope_id: WMComprehension.frame_id(sid),
      verification: :conversation_context_only,
      factual_authority?: false
    }
    assert context.verification == :conversation_context_only
  end

  test "real WM storage recalls nominal referent only for its own session" do
    assert is_pid(Process.whereis(Brain))
    sid = session()
    other = session()
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)

    si = %{original(sid) | comprehension: comprehension(), trace: []}
    stored = WMComprehension.store(si, comprehension_wm?: true)
    assert Enum.any?(stored.trace,
      &(&1.stage == :wm_comprehension and &1.decision == :admitted))

    first =
      %{session_id: sid, sentence: "It's sitting on the fence.", trace: []}
      |> WMComprehension.recall(comprehension_wm?: true)
      |> References.attach()

    assert first.wm_context.nominal_focus.head == "bird"
    assert first.discourse_reference == %{
      pronoun: "it",
      antecedent: "small black bird",
      antecedent_head: "bird",
      antecedent_modifiers: ["small", "black"],
      source: :wm_nominal_focus,
      evidence: :previous_verified_nominal_composition,
      referent_status: :candidate_same_session,
      verification: :conversation_context_only,
      factual_authority?: false
    }

    # The response is not asserting the bird exists or has a known location.
    refute Map.has_key?(first.discourse_reference, :location)
    refute Map.has_key?(first.discourse_reference, :event_verified?)

    other_turn =
      %{session_id: other, sentence: "It's sitting on the fence.", trace: []}
      |> WMComprehension.recall(comprehension_wm?: true)
      |> References.attach()

    refute Map.has_key?(other_turn, :wm_context)
    refute Map.has_key?(other_turn, :discourse_reference)
  end

  test "matched pronoun can sustain the active focus, unrelated turn releases it" do
    sid = session()
    previous = recent_context(sid)

    followup =
      %{session_id: sid, sentence: "It is sleeping under a tree.",
        wm_context: previous}
      |> References.attach()

    assert followup.discourse_reference.referent_status == :candidate_same_session

    next_context = WMComprehension.context_from(followup, comprehension())
    assert next_context.nominal_focus == previous.nominal_focus
    assert next_context.entities == ["small black bird"]

    unrelated = %{session_id: sid, sentence: "Let's discuss machine learning.",
      wm_context: previous}
    released = WMComprehension.context_from(unrelated, comprehension())
    assert released.nominal_focus == nil
    assert released.entities == []
  end

  test "stale lexical choice or tampered proof never creates a nominal WM referent" do
    sid = session()
    si = original(sid)

    stale = %{si | lifg_choices: Enum.map(si.lifg_choices, fn
      %{token_index: 2} = choice -> %{choice | id: "black|noun|15"}
      choice -> choice
    end)}

    assert Composition.verified(stale) == :not_applicable
    assert WMComprehension.context_from(stale, comprehension()).nominal_focus == nil

    forged = %{si | compositional_derivation:
      %{si.compositional_derivation | result: {:var, "fake"}}}
    assert Composition.verified(forged) == :not_applicable
    assert WMComprehension.context_from(forged, comprehension()).nominal_focus == nil
  end

  test "unscoped, question, and other forms never acquire a singular referent" do
    sid = session()
    context = recent_context(sid)

    for sentence <- [
      "What's the population of each?",
      "What is it?",
      "It might be raining.",
      "The weather is wonderful."
    ] do
      si = %{session_id: sid, sentence: sentence, wm_context: context}
      assert Map.get(References.attach(si), :discourse_reference) == nil
    end

    no_session = %{sentence: "It's sitting on the fence.",
      wm_context: context}
    assert Map.get(References.attach(no_session), :discourse_reference) == nil

    wrong_scope = %{session_id: session(), sentence: "It's sitting on the fence.",
      wm_context: context}
    assert Map.get(References.attach(wrong_scope), :discourse_reference) == nil
  end
end
