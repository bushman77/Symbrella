# Run from the Symbrella umbrella root:
#   mix run priv/phase_two_final_validation.exs
#
# Phase 2 acceptance, abstention, opt-out and bounded latency audit.
# Core.resolve_input/2 executes the normal production pipeline, which can
# update working memory/experience state. The timing loops below do NOT call
# that pipeline: they only replay the read-only semantic attachment on
# captured SemanticInput snapshots, with normal PostgreSQL lemma lookups.

defmodule PhaseTwoFinalValidation do
  alias Core.Semantic.Composition.{FinitePastLocative, FinitePastLocativeIntegration}
  alias Core.SemanticInput

  @positive [
    {"The bird sat on the fence.", :sit},
    {"The bird slept on the fence.", :sleep}
  ]
  @negative [
    "The bird is sleeping on the fence.",
    "The bird slept into the tree."
  ]
  @samples 25

  def run do
    accepted =
      Enum.map(@positive, fn {sentence, predicate} ->
        si = Core.resolve_input(sentence)
        proof = Map.get(si, :finite_past_locative_derivation)

        require!(is_map(proof), "missing default-on proof for: #{sentence}")
        require!(FinitePastLocative.verify(proof) == :ok,
          "invalid proof for: #{sentence}")
        require!(get_in(proof, [:event_semantics, :predicate]) == predicate,
          "wrong predicate for: #{sentence}")
        require!(get_in(proof, [:event_semantics, :tense]) == :past,
          "wrong tense for: #{sentence}")
        require!(get_in(proof, [:event_semantics, :event_existence]) == :not_asserted,
          "event was improperly asserted: #{sentence}")
        require!(get_in(proof, [:event_semantics, :actor, :referent]) == :unresolved,
          "actor referent was improperly resolved: #{sentence}")
        require!(Map.get(proof, :factual_authority?) == false,
          "factual authority was improperly assigned: #{sentence}")
        require!(Map.get(proof, :semantic_compatibility_proven?) == false,
          "sense compatibility was improperly asserted: #{sentence}")
        require!(Map.get(proof, :epistemic_status) == :conditional_on_supplied_evidence,
          "epistemic status was improperly promoted: #{sentence}")
        require!(Core.SemanticInput.validate_contract(si) == :ok,
          "SemanticInput contract invalid: #{sentence}")

        selected =
          si.lifg_choices
          |> List.wrap()
          |> Enum.find(&(Map.get(&1, :token_index) == 2))

        require!(is_map(selected), "no predicate LIFG choice: #{sentence}")
        require!(Map.get(selected, :id) == get_in(proof, [:premises, :selected_lifg_id]),
          "proof overwrote LIFG evidence: #{sentence}")

        # Pure observational replay cannot alter any other SI field.
        blank = %{si | finite_past_locative_derivation: nil}
        attached = FinitePastLocativeIntegration.attach(blank)
        require!(attached.finite_past_locative_derivation == proof,
          "read-only replay diverged: #{sentence}")
        require!(Map.delete(attached, :finite_past_locative_derivation) ==
                   Map.delete(blank, :finite_past_locative_derivation),
          "read-only attachment changed unrelated SI state: #{sentence}")

        %{
          sentence: sentence,
          predicate: predicate,
          proof_valid?: true,
          selected_lifg_id: Map.get(selected, :id),
          past_attachment_latency_us: compare_attachment_latency(blank)
        }
      end)

    abstained =
      Enum.map(@negative, fn sentence ->
        si = Core.resolve_input(sentence)
        require!(is_nil(si.finite_past_locative_derivation),
          "unsupported sentence received finite-past proof: #{sentence}")

        %{
          sentence: sentence,
          finite_past_proof: nil,
          original_composition_kind: composition_kind(si)
        }
      end)

    disabled = Core.resolve_input("The bird sat on the fence.",
      finite_past_locative?: false)

    require!(is_nil(disabled.finite_past_locative_derivation),
      "explicit false opt-out produced a finite-past proof")

    IO.inspect(%{
      status: :passed,
      accepted: accepted,
      abstained: abstained,
      explicit_opt_out: :passed,
      samples_per_timing_case: @samples,
      timing_scope: :read_only_attachment_only,
      production_pipeline_timing?: false
    }, label: "Phase 2 final validation", limit: :infinity, printable_limit: :infinity)

    :ok
  end

  defp compare_attachment_latency(si) do
    # Warm up once, so first-use compilation isn't counted.
    _ = FinitePastLocativeIntegration.attach(si, finite_past_locative?: false)
    _ = FinitePastLocativeIntegration.attach(si)

    disabled = measure(si, finite_past_locative?: false)
    enabled = measure(si, [])
    %{
      disabled_p50_us: percentile(disabled, 50),
      disabled_p95_us: percentile(disabled, 95),
      enabled_p50_us: percentile(enabled, 50),
      enabled_p95_us: percentile(enabled, 95),
      approximate_incremental_p50_us: percentile(enabled, 50) - percentile(disabled, 50)
    }
  end

  defp measure(si, opts) do
    Enum.map(1..@samples, fn _ ->
      {elapsed, updated} = :timer.tc(fn ->
        FinitePastLocativeIntegration.attach(si, opts)
      end)

      if Keyword.get(opts, :finite_past_locative?, true) do
        require!(is_map(updated.finite_past_locative_derivation),
          "timed default-on replay unexpectedly abstained")
      else
        require!(updated == si, "timed opt-out changed SemanticInput")
      end

      elapsed
    end)
  end

  defp percentile(values, pct) do
    sorted = Enum.sort(values)
    n = length(sorted)
    index = min(n - 1, max(0, div(n * pct + 99, 100) - 1))
    Enum.at(sorted, index)
  end

  defp composition_kind(%SemanticInput{compositional_derivation: %{kind: kind}}),
    do: kind
  defp composition_kind(_), do: nil

  defp require!(true, _message), do: :ok
  defp require!(false, message), do: raise("Phase 2 validation failed: " <> message)
end

PhaseTwoFinalValidation.run()
