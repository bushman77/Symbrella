# From the Symbrella umbrella root:
#   mix run priv/phase_three_shadow_validation.exs
#
# Phase 3 shadow-only end-to-end diagnostic against the production Core pipeline.
# Core.resolve_input/2 executes ordinary runtime memory/experience side effects.
# The Phase 3 Ledger flag is explicitly enabled ONLY for this probe's sessions.
# Response attachment is explicitly opt-in and disabled by default. Ledger sessions are cleared on
# exit; other ordinary production side effects are not rolled back.

defmodule PhaseThreeShadowValidation do
  alias Core.Discourse.Ledger
  alias Core.Semantic.Composition.FinitePastLocative
  alias Core.Semantic.FinitePastDiscourse, as: Candidate
  alias Core.Semantic.FinitePastDiscourseQuestion, as: Question
  alias Core.Response.FinitePastDiscourseReadout, as: Readout
  alias Core.Response.FinitePastDiscourseArbitration, as: Arbitration

  @positives [
    {"The bird sat on the fence.", :sit, "sat|noun|1"},
    {"The bird slept on the fence.", :sleep, "slept|verb|1"}
  ]
  @samples 25

  def run do
    ids = for _ <- 1..8, do: unique_session()

    try do
      [sit_id, sleep_id, off_id, ambiguous_id, intervention_id,
       gated_answer_id, gated_ambiguity_id, gated_stale_id] = ids

      accepted = Enum.zip(@positives, [sit_id, sleep_id])
      |> Enum.map(fn {{sentence, predicate, lifg_id}, session} ->
        {pipeline_us, si} = :timer.tc(fn ->
          Core.resolve_input(sentence,
            session_id: session, finite_past_discourse_ledger?: true)
        end)

        proof = si.finite_past_locative_derivation
        require!(is_map(proof) and FinitePastLocative.verify(proof) == :ok,
          "missing or invalid production proof: #{sentence}")
        require!(proof.event_semantics.predicate == predicate,
          "wrong production predicate: #{sentence}")
        require!(proof.event_semantics.event_existence == :not_asserted and
                   proof.factual_authority? == false,
          "proof asserted an event: #{sentence}")

        selected = Enum.find(List.wrap(si.lifg_choices),
          &(Map.get(&1, :token_index) == 2))
        require!(is_map(selected) and selected.id == lifg_id,
          "LIFG selected a different predicate sense: #{sentence}")

        assert_record(session, predicate)

        {candidate_us, {:ok, candidate}} = :timer.tc(fn ->
          Candidate.project(si)
        end)
        require!(candidate.status == :provisional and
                   candidate.factual_authority? == false,
          "pure candidate changed epistemic status: #{sentence}")

        {question_us, {:ok, projected}} = :timer.tc(fn ->
          Question.project(session, "What was the bird doing?")
        end)
        require!(projected.description.lemma == predicate and
                   projected.description.tense == :past and
                   projected.factual_authority? == false,
          "read-only question resolved incorrectly: #{sentence}")

        require!(match?({:ok, %{description: %{kind: :reported_location,
                     relation: :on, landmark_kind: :fence}}},
                   Question.project(session, "Where was the bird?")),
          "location question failed: #{sentence}")

        # A separate, explicitly called readout may describe the user's
        # words, but does not feed them into the production response planner.
        require!(match?({:ok, _, %{status: :speaker_description_candidate,
                                    factual_authority?: false,
                                    llm_called?: false}},
                        Readout.read(session, "What was the bird doing?")),
          "qualified read-only description failed: #{sentence}")

        # Arbitration is a separate opt-in. It may answer only as a
        # speaker-attributed description and may NOT call older memory.
        require!(Arbitration.decide(session, "What was the bird doing?") == :pass,
          "arbitration unexpectedly enabled by default")

        require!(match?({:answer, _, %{status: :speaker_description_candidate,
                                      factual_authority?: false}},
                   Arbitration.route(session, "What was the bird doing?",
                     fn -> raise "unexpected old episodic fallback" end,
                     finite_past_response_arbitration?: true)),
          "same-session attributed arbitration answer failed")

        # The legacy Ledger resolver must not gain authority or new answers.
        require!(Ledger.resolve(session, "Where was the bird?") == :not_found,
          "legacy Ledger.resolve unexpectedly consumed Phase 3 candidate")

        %{
          sentence: sentence,
          predicate: predicate,
          selected_lifg_id: selected.id,
          proof_valid?: true,
          ledger_status: :provisional,
          pipeline_us: pipeline_us,
          candidate_us: candidate_us,
          question_us: question_us,
          question_read_p50_us: p50(@samples, fn ->
            Question.project(session, "What was the bird doing?")
          end)
        }
      end)

      # A fully shaped record is still not an authorization to write:
      # only the owner-side evidence revalidation path can append this kind.
      saved = hd(Ledger.entries(sit_id))
      require!(Core.Discourse.Ledger.Owner.append(sit_id, saved) ==
                 {:error, :verified_finite_past_required},
        "owner allowed a raw finite-past record")
      require!(GenServer.call(Core.Discourse.Ledger.Owner,
                   {:append, sit_id, %{saved | source_turn_sha256: "forged"}}) ==
                 {:error, :verified_finite_past_required},
        "owner accepted forged finite-past evidence")
      require!(length(Ledger.entries(sit_id)) == 1,
        "raw rejected writes mutated sequence state")

      # Production default must remain off without explicit Phase 3 opt-in.
      Core.resolve_input("The bird sat on the fence.", session_id: off_id)
      require!(Enum.all?(Ledger.entries(off_id),
        &(&1.kind != :reported_finite_past_locative_candidate)),
        "default production enabled the opt-in Ledger kind")
      require!(Question.project(off_id, "What was the bird doing?") ==
                 :not_applicable, "default production exposed past questions")

      # Independently revalidated previously captured production SI, stored
      # into one fresh scope, must be ambiguous, not a recency-based answer.
      [sit, sleep] =
        Enum.map(@positives, fn {sentence, _predicate, _lifg_id} ->
          # Re-run normal production parsing on each source in the same
          # isolated session. Do not copy a saved proof to a different turn.
          Core.resolve_input(sentence,
            session_id: ambiguous_id, finite_past_discourse_ledger?: true)
        end)

      require!(sit.finite_past_locative_derivation != nil and
                 sleep.finite_past_locative_derivation != nil,
        "ambiguous production setup lacked proof")
      assert_ambiguous(ambiguous_id)
      require!(match?({:clarify, _, %{status: :referent_ambiguous,
                                         factual_authority?: false}},
                   Arbitration.route(ambiguous_id, "Where was the bird?",
                     fn -> raise "ambiguous history fell through to old memory" end,
                     finite_past_response_arbitration?: true)),
        "arbitration did not preserve ambiguity")

      require!(match?({:ambiguous, _, %{status: :referent_ambiguous,
                                          factual_authority?: false}},
                      Readout.read(ambiguous_id, "Where was the bird?")),
        "readout selected a source from ambiguous reports")


      # An unrelated next turn is a hard continuity barrier for this V1.
      Core.resolve_input("The bird slept on the fence.",
        session_id: intervention_id, finite_past_discourse_ledger?: true)
      require!(match?({:ok, _},
        Question.project(intervention_id, "Where was the bird?")),
        "intervention baseline missing")
      Core.resolve_input("An unrelated question?",
        session_id: intervention_id, finite_past_discourse_ledger?: true)
      require!(Question.project(intervention_id, "Where was the bird?") ==
                 :not_applicable, "older source revived through unrelated turn")
      require!(Readout.read(intervention_id, "Where was the bird?") ==
                 :not_applicable, "readout revived older report through unrelated turn")
      require!(match?({:abstain, _, %{status: :continuity_unresolved,
                                         factual_authority?: false}},
                   Arbitration.route(intervention_id, "Where was the bird?",
                     fn -> raise "stale history fell through to old memory" end,
                     finite_past_response_arbitration?: true)),
        "arbitration did not block old memory on stale continuity")

      require!(Question.project(sit_id, "Where was it?") == :not_applicable,
        "pronoun acquired unsupported identity")
      require!(Question.project(sit_id, "Where is the bird?") == :not_applicable,
        "present question reused past description")
      require!(Ledger.entries(sleep_id) |> Enum.count() == 1,
        "other session was mutated during ambiguous probe")

      # Controlled FULL response attachment checks (separate sessions):
      # the new gate runs before episodic fallback, but remains opt-in.
      Core.resolve_input("The bird slept on the fence.",
        session_id: gated_answer_id,
        response: :off, finite_past_discourse_ledger?: true)

      {response_us, answered} = :timer.tc(fn ->
        Core.resolve_input("What was the bird doing?",
          session_id: gated_answer_id,
          finite_past_discourse_ledger?: true,
          finite_past_response_arbitration?: true)
      end)

      require!(is_binary(answered.response_text) and
                 String.contains?(answered.response_text, "bird as sleeping") and
                 String.contains?(answered.response_text, "not an independently verified event"),
        "opt-in production response was not speaker-qualified")
      require!(Map.get(answered.response_meta, :status) == :speaker_description_candidate and
                 Map.get(answered.response_meta, :response_source) ==
                   :symbolic_finite_past_report and
                 Map.get(answered.response_meta, :factual_authority?) == false and
                 Map.get(answered.response_meta, :llm_called?) == false,
        "opt-in production response did not carry the safe metadata")

      Core.resolve_input("The bird sat on the fence.",
        session_id: gated_ambiguity_id,
        response: :off, finite_past_discourse_ledger?: true)
      Core.resolve_input("The bird slept on the fence.",
        session_id: gated_ambiguity_id,
        response: :off, finite_past_discourse_ledger?: true)

      ambiguous_response = Core.resolve_input("Where was the bird?",
        session_id: gated_ambiguity_id,
        finite_past_discourse_ledger?: true,
        finite_past_response_arbitration?: true)

      require!(Map.get(ambiguous_response.response_meta, :status) ==
                 :referent_ambiguous and
                 String.contains?(ambiguous_response.response_text || "",
                   "multiple descriptions"),
        "full response path guessed a referent instead of clarifying")

      Core.resolve_input("The bird sat on the fence.",
        session_id: gated_stale_id,
        response: :off, finite_past_discourse_ledger?: true)
      Core.resolve_input("An unrelated question?",
        session_id: gated_stale_id,
        response: :off, finite_past_discourse_ledger?: true)

      stale_response = Core.resolve_input("Where was the bird?",
        session_id: gated_stale_id,
        finite_past_discourse_ledger?: true,
        finite_past_response_arbitration?: true)

      require!(Map.get(stale_response.response_meta, :status) ==
                 :continuity_unresolved and
                 String.contains?(stale_response.response_text || "",
                   "cannot safely connect"),
        "full response path retrieved stale episodic memory")

      IO.inspect(%{
        status: :passed,
        scope: :shadow_only,
        accepted: accepted,
        default_gate: :disabled,
        raw_owner_append: :rejected,
        repeated_actor_kind: :ambiguous,
        unrelated_turn: :abstained,
        cross_session: :isolated,
        readout_observation_only?: true,
        arbitration_observation_only?: false,
        explicit_opt_in_response_path?: true,
        opt_in_response_us: response_us,
        legacy_fallback_blocked_on_ambiguity?: true,
        live_answer_path_default_enabled?: false,
        complete_phase_three?: false,
        samples_per_question_timing_case: @samples,
        warning: :production_pipeline_has_normal_runtime_side_effects
      }, label: "Phase 3 production shadow verification",
         limit: :infinity, printable_limit: :infinity)

      :ok
    after
      Enum.each(ids, &Ledger.clear/1)
    end
  end

  defp assert_record(session, predicate) do
    case Ledger.entries(session) do
      [%{seq: 1, kind: :reported_finite_past_locative_candidate,
         status: :provisional, predicate: ^predicate,
         event_existence: :not_asserted,
         event_occurrence_verified?: false,
         referent_identity_verified?: false,
         factual_authority?: false,
         verification: :conversation_context_only}] -> :ok

      other ->
        raise "Unexpected admitted production record: #{inspect(other, limit: 2)}"
    end
  end

  defp assert_ambiguous(session) do
    case Question.project(session, "What was the bird doing?") do
      {:ambiguous, [%{source_turn_seq: 2, factual_authority?: false},
                    %{source_turn_seq: 1, factual_authority?: false}]} -> :ok

      other ->
        raise "Repeated actor-kind did not abstain as ambiguous: #{inspect(other)}"
    end
  end

  defp p50(samples, work) do
    times = for _ <- 1..samples do
      {elapsed, result} = :timer.tc(work)
      require!(match?({:ok, %{factual_authority?: false}}, result),
        "timed question diverged")
      elapsed
    end
    times |> Enum.sort() |> Enum.at(div(samples, 2))
  end

  defp unique_session do
    "phase3-shadow-#{System.unique_integer([:positive, :monotonic])}"
  end

  defp require!(true, _), do: :ok
  defp require!(false, reason), do: raise("Phase 3 validation failed: " <> reason)
end

PhaseThreeShadowValidation.run()
