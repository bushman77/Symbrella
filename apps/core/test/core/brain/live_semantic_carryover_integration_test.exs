defmodule Core.Brain.LiveSemanticCarryoverIntegrationTest do
  use ExUnit.Case, async: false

  alias Core.Brain.{Activation, SemanticPropagation, STM}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @sentence "The artist draws a picture."
  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  # The test DB is not the imported production Wiktextract corpus. Reuse the
  # independently witnessed SVO/ATL structure tested by SemanticPropagationTest
  # rather than assuming Db.ltm can build an entire production comprehension.
  # From Activation.notify onward, all state flows through the REAL Brain
  # coordinator and the real Stage1 scoring path; no synthetic snapshots.
  defp witnessed_first_turn(session) do
    choices = [
      %{token_index: 1, id: "artist|noun|1", lemma: "artist"},
      %{token_index: 2, id: "draws|verb|1", lemma: "draws"},
      %{token_index: 4, id: "picture|noun|1", lemma: "picture"}
    ]

    %{
      session_id: session,
      sentence: @sentence,
      source: :user,
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "artist", n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "picture.", n: 1}
      ],
      sentence_signatures: [
        %{
          id: :finite_present_svo_candidate,
          source: :core_syntax,
          span: {0, 4},
          evidence: @svo_evidence,
          roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
                   object_determiner: 3, object_head: 4}
        }
      ],
      active_cells: [
        %{id: "artist|noun|1", norm: "artist", pos: "noun"},
        %{id: "picture|noun|1", norm: "picture", pos: "noun"},
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          definition: "An inflected form of draw.", semantic_atoms: @verb_tags}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp session, do: "live-carryover-#{System.unique_integer([:positive, :monotonic])}"

  defp feedback(si) do
    Enum.find(si.trace, fn
      %{stage: :neuron_feedback} -> true
      _ -> false
    end)
  end

  defp row(id, pos) do
    %{
      id: id,
      token_index: 0,
      lemma: "draw",
      norm: "draw",
      pos: pos,
      definition: "Test candidate for #{id}.",
      source: :ltm,
      score: 0.5
    }
  end

  defp draw_verb?(%{id: id}) when is_binary(id) do
    case String.split(id, "|") do
      ["draw", "verb", suffix] -> suffix != "" and suffix != "fallback"
      _ -> false
    end
  end

  defp draw_verb?(_), do: false

  defp semantic_prior?(%{prior_semantic_activation: value})
       when is_number(value) and value > 0, do: true

  defp semantic_prior?(_), do: false

  defp candidate_rows(si, field) do
    si
    |> Map.get(field, %{})
    |> Map.get(0, [])
  end

  defp followup(session, options \\ []) do
    # STM is the production read path for the session-scoped lemma signal.
    # The lexical rows below are fixed test-DB fixtures, NOT a probe snapshot.
    # This isolates live routing from dictionary import/population concerns.
    rows = [
      row("draw|verb|1", "verb"),
      row("draw|verb|2", "verb"),
      row("draw|noun|1", "noun")
    ]

    %{
      sentence: "draw",
      session_id: session,
      tokens: [%{index: 0, phrase: "draw", n: 1, mw: false, span: {0, 4}}],
      active_cells: [],
      trace: []
    }
    |> STM.run()
    |> Map.put(:active_cells, rows)
    |> Map.put(:sense_candidates, %{0 => rows})
    |> Map.put(:candidates_by_token, %{0 => rows})
    |> Activation.prime(Keyword.merge([notify_brain?: false], options))
    |> Activation.overlay_candidate_slates()
  end

  defp stage1_scores(si) do
    assert {:ok, %{choices: [%{scores: scores}]}} =
             Brain.LIFG.Stage1.run(si,
               preserve_sense_candidates: true,
               scores: :all,
               cerebellum_learn?: false,
               cerebellum_snapshot_path: nil,
               weights: %{lex_fit: 0.0, context_fit: 0.0, rel_prior: 0.0,
                          activation: 1.0, intent_bias: 0.0}
             )

    scores
  end

  test "real Brain stores witnessed SVO pulse and next turn routes it into Stage1" do
    assert is_pid(Process.whereis(Brain))

    owner = session()
    stranger = session()
    assert Brain.semantic_snapshot(owner) == %{}
    assert Brain.semantic_snapshot(stranger) == %{}

    first = witnessed_first_turn(owner)

    assert %{status: :structurally_interpreted, kind: :participant_assertion} =
             first.sentence_meaning
    assert [%{source_frame: :finite_present_svo}] = first.episodic_candidates

    assert %{lemma: "draw", source_id: "draws|verb|1",
             evidence: :attested_inflected_predicate} =
             SemanticPropagation.lemma_pulse(first)

    # This sends through Activation.notify -> Brain.semantic_pulse, using
    # Brain's live GenServer state, not a test-injected activation map.
    notified = Activation.notify(first, notify_brain?: true)

    assert %{meta: %{
               lemma_pulse_submitted?: true,
               lemma_pulse: %{lemma: "draw", scope: :verb_lemma_not_sense}
             }} = feedback(notified)

    assert %{"draw" => stored} = Brain.semantic_snapshot(owner)
    assert stored > 0 and stored <= 0.03
    assert Brain.semantic_snapshot(stranger) == %{}

    next_turn = followup(owner)
    assert %{"draw" => carried} = next_turn.semantic_pulses
    assert carried > 0 and carried <= stored

    primed = Enum.filter(next_turn.active_cells, &semantic_prior?/1)
    assert length(primed) == 2
    assert Enum.all?(primed, &draw_verb?/1)

    assert Enum.all?(primed, fn cell ->
             abs(cell.prior_semantic_activation - carried) < 1.0e-6
           end)

    # The same signal reaches the preferred Stage1 candidate buckets.
    for field <- [:sense_candidates, :candidates_by_token] do
      rows = candidate_rows(next_turn, field)
      assert Enum.count(rows, &semantic_prior?/1) == 2
      refute Enum.any?(Enum.filter(rows, &(not draw_verb?(&1))), &semantic_prior?/1)
    end

    on_scores = stage1_scores(next_turn)

    # Disable only lemma routing. The live exact-ID STM snapshot is unchanged.
    off = followup(owner, semantic_propagation?: false)
    assert off.semantic_pulses == %{}
    # Live exact-ID activity can decay between separate STM calls, so its
    # numeric snapshot must not be compared for bitwise equality.
    assert is_map(off.neuron_activation)
    refute Enum.any?(off.active_cells, &semantic_prior?/1)
    off_scores = stage1_scores(off)

    assert Map.fetch!(on_scores, "draw|verb|1") >
             Map.fetch!(off_scores, "draw|verb|1")

    # A different session cannot read this lemma prior.
    other = followup(stranger)
    assert other.semantic_pulses == %{}
    refute Enum.any?(other.active_cells, &semantic_prior?/1)
    assert Brain.semantic_snapshot(stranger) == %{}
  end

  test "disabling the production write gate prevents semantic pulse submission" do
    owner = session()
    first = witnessed_first_turn(owner)

    assert %{status: :structurally_interpreted} = first.sentence_meaning

    notified =
      Activation.notify(first, notify_brain?: true, semantic_propagation?: false)

    assert %{meta: %{lemma_pulse: nil, lemma_pulse_submitted?: false}} =
             feedback(notified)
    assert Brain.semantic_snapshot(owner) == %{}
  end

  test "an unsupported morphological witness never produces live semantic memory" do
    owner = session()
    first = witnessed_first_turn(owner)

    cells =
      Enum.map(first.active_cells, fn
        %{id: "draws|verb|1"} = cell -> %{cell | semantic_atoms: []}
        cell -> cell
      end)

    unsupported = %{first | active_cells: cells}
    assert SemanticPropagation.lemma_pulse(unsupported) == nil

    notified = Activation.notify(unsupported, notify_brain?: true)
    assert %{meta: %{lemma_pulse: nil, lemma_pulse_submitted?: false}} =
             feedback(notified)
    assert Brain.semantic_snapshot(owner) == %{}
  end
end
