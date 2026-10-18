defmodule Core.Brain.LiveSemanticCarryoverIntegrationTest do
  use ExUnit.Case, async: false

  alias Core.Brain.SemanticPropagation

  @assertion "The artist draws a picture."
  @question "what did the artist draw?"

  # Exercise the real Brain coordinator and LIFG path, without writing
  # episodes, training weights, self-model memory, or generating LLM replies.
  # No neuron_probe_snapshot/semantic_probe_snapshot overrides are supplied.
  defp resolve(sentence, session, overrides) do
    opts = [
      mode: :prod,
      session_id: session,
      response: :off,
      persist_episodes: false,
      episodes: false,
      hippocampus_encode?: false,
      persist_self_model: false,
      write_self_monitor_memory: false,
      comprehension_wm?: false,
      focus_prompt_topics?: false,
      lifg_opts: [cerebellum_learn?: false, cerebellum_snapshot_path: nil],
      neuron_activation?: true,
      notify_brain?: false
    ]

    Core.resolve_input(sentence, Keyword.merge(opts, overrides))
  end

  defp session do
    "live-carryover-#{System.unique_integer([:positive, :monotonic])}"
  end

  defp feedback(si) do
    Enum.find(si.trace, fn
      %{stage: :neuron_feedback} -> true
      _ -> false
    end)
  end

  defp draw_verb?(%{id: id}) when is_binary(id) do
    case String.split(id, "|") do
      ["draw", "verb", sense] -> sense != "" and sense != "fallback"
      _ -> false
    end
  end

  defp draw_verb?(_), do: false

  defp has_lemma_prior?(cell) when is_map(cell) do
    case Map.get(cell, :prior_semantic_activation) do
      value when is_number(value) and value > 0 -> true
      _ -> false
    end
  end

  defp has_lemma_prior?(_), do: false

  defp slate_rows(si) do
    Enum.flat_map([:sense_candidates, :candidates_by_token], fn key ->
      case Map.get(si, key) do
        %{} = slates ->
          slates
          |> Map.values()
          |> Enum.flat_map(fn
            rows when is_list(rows) -> rows
            _ -> []
          end)

        _ ->
          []
      end
    end)
  end

  test "real first-turn feedback survives Brain STM and primes next-turn LIFG candidates" do
    assert is_pid(Process.whereis(Brain))

    owner = session()
    stranger = session()

    assert Brain.semantic_snapshot(owner) == %{}
    assert Brain.semantic_snapshot(stranger) == %{}

    # This is the actual Core.resolve_input -> Activation.notify ->
    # Brain.semantic_pulse production call, not a fabricated probe prior.
    first = resolve(@assertion, owner, notify_brain?: true)

    assert %{status: :structurally_interpreted, kind: :participant_assertion} =
             first.sentence_meaning

    assert [%{source_frame: :finite_present_svo}] = first.episodic_candidates

    assert %{lemma: "draw", source_id: "draws|verb|1",
             evidence: :attested_inflected_predicate} =
             SemanticPropagation.lemma_pulse(first)

    assert %{meta: %{
               lemma_pulse_submitted?: true,
               lemma_pulse: %{lemma: "draw", scope: :verb_lemma_not_sense}
             }} = feedback(first)

    assert %{"draw" => stored} = Brain.semantic_snapshot(owner)
    assert stored > 0 and stored <= 0.03
    assert Brain.semantic_snapshot(stranger) == %{}

    # Turn two pulls from Brain.stm/1, without any isolated/synthetic snapshots
    # and without submitting more activity during the comparison.
    second = resolve(@question, owner, notify_brain?: false)

    assert %{"draw" => carried} = second.semantic_pulses
    assert carried > 0 and carried <= stored

    assert %{status: :structurally_interpreted,
             proposition: %{type: :information_request,
                            queried: %{role: :patient, status: :unbound}}} =
             second.sentence_meaning
    assert second.episodic_candidates == []

    boosted =
      second.active_cells
      |> Enum.filter(&draw_verb?/1)
      |> Enum.filter(&has_lemma_prior?/1)

    assert boosted != []
    assert Enum.all?(boosted, fn cell ->
             abs(cell.prior_semantic_activation - carried) < 1.0e-6
           end)

    # Stage1 prefers these lexical buckets over active_cells; priming just
    # active_cells would not establish that LIFG received the live signal.
    assert Enum.any?(slate_rows(second), fn cell ->
             draw_verb?(cell) and has_lemma_prior?(cell)
           end)

    # The signal is not a sense. It cannot prime draw as a noun or unrelated
    # lemma/POS candidates, and it must not assert the question as an event.
    refute Enum.any?(second.active_cells, fn cell ->
             not draw_verb?(cell) and has_lemma_prior?(cell)
           end)

    # Compare the same live session with the propagation read gate disabled.
    off = resolve(@question, owner,
      notify_brain?: false, semantic_propagation?: false
    )

    assert off.semantic_pulses == %{}
    refute Enum.any?(off.active_cells, &has_lemma_prior?/1)
    refute Enum.any?(slate_rows(off), &has_lemma_prior?/1)

    # Exact BrainCell activation remains a separate channel; only the
    # session-scoped semantic lemma signal is isolated across conversations.
    other = resolve(@question, stranger, notify_brain?: false)
    assert other.semantic_pulses == %{}
    refute Enum.any?(other.active_cells, &has_lemma_prior?/1)
    assert Brain.semantic_snapshot(stranger) == %{}
  end

  test "the production write gate prevents semantic pulses" do
    owner = session()

    first =
      resolve(@assertion, owner,
        notify_brain?: true,
        semantic_propagation?: false
      )

    assert %{status: :structurally_interpreted} = first.sentence_meaning
    assert %{meta: %{lemma_pulse: nil, lemma_pulse_submitted?: false}} =
             feedback(first)
    assert Brain.semantic_snapshot(owner) == %{}
  end
end
