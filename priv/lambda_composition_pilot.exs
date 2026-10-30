# Run from the umbrella root:
#   mix run priv/lambda_composition_pilot.exs
# Optional production-pipeline observation (may invoke ordinary pipeline side
# effects; does not itself write DB records):
#   mix run priv/lambda_composition_pilot.exs --live
#
# Pure controls test the current, closed lexical vocabulary, typed beta
# reduction, and replay verification. They do NOT establish exact dictionary
# sense accuracy. Live observations keep the full LIFG candidate distribution.
# Only the output JSONL under priv/ is written by this script.

alias Core.Semantic.Composition
alias Core.Semantic.Composition.Event

cases = [
  %{
    id: "positive_static",
    expected: :supported,
    sentence: "The bird is sitting on the fence.",
    input: %{actor: "bird", predicate: "sitting", preposition: "on", landmark: "fence"}
  },
  %{
    id: "positive_directional",
    expected: :supported,
    sentence: "The bird is flying into the tree.",
    input: %{actor: "bird", predicate: "flying", preposition: "into", landmark: "tree"}
  },
  %{
    id: "positive_other_static",
    expected: :supported,
    sentence: "The cat is sleeping under the table.",
    input: %{actor: "cat", predicate: "sleeping", preposition: "under", landmark: "table"}
  },
  %{
    id: "negative_unlicensed_flying_on",
    expected: :unsupported,
    sentence: "The bird is flying on the fence.",
    input: %{actor: "bird", predicate: "flying", preposition: "on", landmark: "fence"}
  },
  %{
    id: "negative_unlicensed_sitting_into",
    expected: :unsupported,
    sentence: "The bird is sitting into the tree.",
    input: %{actor: "bird", predicate: "sitting", preposition: "into", landmark: "tree"}
  },
  %{
    id: "negative_outside_vocabulary",
    expected: :unsupported,
    sentence: "The fisherman sat on the bank of the river.",
    input: %{actor: "fisherman", predicate: "sat", preposition: "on", landmark: "bank"}
  }
]

pure =
  Enum.map(cases, fn fixture ->
    result =
      case Event.compose(fixture.input) do
        {:ok, derivation} ->
          verified = Event.verify(derivation)

          %{
            actual: if(verified == :ok, do: :supported, else: :invalid_proof),
            proof_valid?: verified == :ok,
            reduction_steps: length(derivation.steps),
            result_type: inspect(derivation.result_type),
            semantics: derivation.semantics
          }

        :unsupported ->
          %{actual: :unsupported, proof_valid?: false,
            reduction_steps: 0, result_type: nil}
      end

    %{
      study: :closed_typed_lambda_baseline,
      case_id: fixture.id,
      sentence: fixture.sentence,
      expected: fixture.expected,
      matches_expected?: result.actual == fixture.expected,
      result: result
    }
  end)

live =
  if "--live" in System.argv() do
    Enum.flat_map([
      {"known_progressive", "The bird is sitting on the fence."},
      {"novel_past_locative", "The fisherman sat on the bank of the river."}
    ], fn {case_id, sentence} ->
      Enum.map([:baseline, :spatial_prior], fn mode ->
        opts = [
          fuzzy: false,
          neuron_activation?: false,
          notify_brain?: false,
          lifg_opts: [
            scores: :all,
            synapse_eval_full_scores?: true,
            spatial_pp_senses?: mode == :spatial_prior
          ]
        ]

        try do
          si = Core.resolve_input(sentence, opts)
          tokens = Map.get(si, :tokens, []) || []

          on_token =
            Enum.find(tokens, fn token ->
              Map.get(token, :n, 1) == 1 and
                String.downcase(Map.get(token, :phrase, "")) == "on"
            end)

          index = if is_map(on_token), do: Map.get(on_token, :index), else: nil

          choice =
            Enum.find(Map.get(si, :lifg_choices, []) || [], fn c ->
              Map.get(c, :token_index) == index and not is_nil(index)
            end)

          probabilities =
            if is_map(choice) do
              Map.get(choice, :candidate_probs) || %{}
            else
              %{}
            end

          top =
            probabilities
            |> Enum.filter(fn {id, score} ->
              is_binary(id) and is_number(score)
            end)
            |> Enum.sort_by(fn {id, score} -> {-score, id} end)
            |> Enum.take(12)
            |> Enum.map(fn {id, score} ->
              %{id: id, probability: score}
            end)

          proof =
            case Composition.verified(si) do
              {:ok, derivation} ->
                %{status: :verified, kind: derivation.kind,
                  proof_steps: length(derivation.steps)}
              :not_applicable ->
                %{status: :not_applicable}
            end

          %{
            study: :live_pipeline_observation,
            case_id: case_id,
            sentence: sentence,
            mode: mode,
            selected_sense: if(is_map(choice), do: Map.get(choice, :id), else: nil),
            scored_candidates: map_size(probabilities),
            top: top,
            spatial_pp: if(is_map(choice), do: Map.get(choice, :spatial_pp), else: nil),
            lambda_proof: proof
          }
        rescue
          exception ->
            %{
              study: :live_pipeline_observation,
              case_id: case_id, mode: mode,
              error: exception.__struct__ |> Module.split() |> List.last()
            }
        end
      end)
    end)
  else
    []
  end

records = pure ++ live
output = "priv/lambda_composition_pilot_results.jsonl"
File.write!(output, Enum.map_join(records, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(
  %{
    saved_to: output,
    pure_cases: length(pure),
    pure_expected_matches: Enum.count(pure, & &1.matches_expected?),
    live_observations: length(live),
    pure_results: Enum.map(pure, fn r ->
      %{case_id: r.case_id, expected: r.expected,
        actual: r.result.actual, proof_steps: r.result.reduction_steps}
    end),
    live_results: Enum.map(live, fn r ->
      Map.take(r, [:case_id, :mode, :selected_sense, :scored_candidates,
                   :lambda_proof, :error])
    end)
  },
  label: "Lambda composition pilot",
  limit: :infinity
)
