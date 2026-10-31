# From the Symbrella umbrella root:
#   mix run priv/phase_three_shadow_validation.exs
#
# Phase 3 shadow-only end-to-end diagnostic against the production Core pipeline.
# Core.resolve_input/2 executes ordinary runtime memory/experience side effects.
# The Phase 3 Ledger flag is explicitly enabled ONLY for this probe's sessions.
# No live answer/planner integration is enabled. Ledger sessions are cleared on
# exit; other ordinary production side effects are not rolled back.

defmodule PhaseThreeShadowValidation do
  alias Core.Discourse.Ledger
  alias Core.Semantic.Composition.FinitePastLocative
  alias Core.Semantic.FinitePastDiscourse, as: Candidate
  alias Core.Semantic.FinitePastDiscourseQuestion, as: Question

  @positives [
    {"The bird sat on the fence.", :sit, "sat|noun|1"},
    {"The bird slept on the fence.", :sleep, "slept|verb|1"}
  ]
  @samples 25

  def run do
    ids = for _ <- 1..5, do: unique_session()

    try do
      [sit_id, sleep_id, off_id, ambiguous_id, intervention_id] = ids

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

      # Production default must remain off without explicit Phase 3 opt-in.
      Core.resolve_input("The bird sat on the fence.", session_id: off_id)
      require!(Enum.all?(Ledger.entries(off_id),
        &(&1.kind != :reported_finite_past_locative_candidate)),
        "default production enabled the opt-in Ledger kind")
      require!(Question.project(off_id, "What was the bird doing?") ==
                 :not_applicable, "default production exposed past questions")

      # Independently revalidated previously captured production SI, stored
      # into one fresh scope, must be ambiguous, not a recency-based answer.
      [sit, sleep] = Enum.zip(@positives, [sit_id, sleep_id])
        |> Enum.map(fn {_case, session} -> session end)
        |> Enum.map(fn session ->
          # Core produced and admitted this source in its original session;
          # for a new synthetic scope, rebuild once from the production path.
          # Do not copy a proof into a different sentence.
          original = if session == sit_id,
            do: "The bird sat on the fence.",
            else: "The bird slept on the fence."
          Core.resolve_input(original,
            session_id: ambiguous_id, finite_past_discourse_ledger?: true)
        end)

      require!(sit.finite_past_locative_derivation != nil and
                 sleep.finite_past_locative_derivation != nil,
        "ambiguous production setup lacked proof")
      assert_ambiguous(ambiguous_id)

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

      require!(Question.project(sit_id, "Where was it?") == :not_applicable,
        "pronoun acquired unsupported identity")
      require!(Question.project(sit_id, "Where is the bird?") == :not_applicable,
        "present question reused past description")
      require!(Ledger.entries(sleep_id) |> Enum.count() == 1,
        "other session was mutated during ambiguous probe")

      IO.inspect(%{
        status: :passed,
        scope: :shadow_only,
        accepted: accepted,
        default_gate: :disabled,
        repeated_actor_kind: :ambiguous,
        unrelated_turn: :abstained,
        cross_session: :isolated,
        live_answer_path_enabled?: false,
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
