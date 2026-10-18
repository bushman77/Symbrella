defmodule Core.Brain.ActivationProbe do
  @moduledoc """
  Read-only, isolated counterfactual probe for short-term neuron carryover.

  This is NOT a complete live-chat A/B test. It evaluates one shared first
  sentence with normal Core, constructs a bounded prior from the exact
  recruitment/feedback plan, then runs the following sentence twice:
  baseline (neuron activation disabled) and carryover (the isolated prior).

  Production event persistence and LLM synthesis are disabled. Hippocampal
  user-event reads are not used: old owner-scoped events would confound a test.
  A read-only event answer can be reconstructed from the current input's
  independently revalidated episodic projection.

  Other singleton cognitive components may still change transient state while
  Core.resolve_input runs, so scores are observational, not causal proof.
  """

  alias Core.Brain.Activation
  alias Core.Semantic.{ActionQuestions, EpisodicProjection}

  @default_gap_ms 1_000
  @max_gap_ms 60_000

  @spec scenarios() :: [{String.t(), String.t(), String.t()}]
  def scenarios do
    [
      {"artist", "The artist draws a picture.", "what did the artist draw?"},
      {"bank_switch", "We sat beside the river bank.", "I deposited money at the bank."}
    ]
  end

  @doc "Run an isolated two-turn comparison (no event writes or LLM responses)."
  @spec run(String.t(), String.t(), keyword()) :: map()
  def run(first, second, opts \\ [])
      when is_binary(first) and is_binary(second) and is_list(opts) do
    gap_ms = bounded_gap(Keyword.get(opts, :gap_ms, @default_gap_ms))
    session = "neuron-probe-#{System.unique_integer([:positive, :monotonic])}"

    common_opts = [
      mode: :prod,
      session_id: session,
      response: :off,
      persist_episodes: false,
      comprehension_wm?: false,
      notify_brain?: false
    ]

    # This is one shared first-turn analysis. The neural arm's current-turn
    # priors would also be empty; only its planned activity is carried forward.
    first_si = Core.resolve_input(first, common_opts ++ [neuron_activation?: false])
    prior = carryover(first_si, gap_ms)

    baseline_si =
      Core.resolve_input(second, common_opts ++ [neuron_activation?: false])

    neural_si =
      Core.resolve_input(
        second,
        common_opts ++ [neuron_activation?: true, neuron_probe_snapshot: prior]
      )

    baseline = summarize(baseline_si)
    neural = summarize(neural_si)

    %{
      schema: 1,
      first: first,
      second: second,
      gap_ms: gap_ms,
      synthetic_prior_count: map_size(prior),
      synthetic_prior_top: top_prior(prior),
      first_turn: summarize(first_si),
      read_only_event_answer: event_answer(first_si, second),
      baseline: baseline,
      neural: neural,
      differences: compare(baseline, neural),
      limits: [
        :planned_carryover_not_live_cell_processes,
        :no_persistent_user_events_written_or_read,
        :no_llm_answer_generated,
        :shared_singletons_can_change_transient_state,
        :sense_probabilities_are_not_ground_truth
      ]
    }
  end

  @doc """
  Project the first turn's weak lexical recruitment and evidence-graded
  post-comprehension feedback through Brain.Cell's actual half-life formula.

  Uses *only* the supplied first-turn SI, not the global Brain activation
  snapshot, and makes no calls that stimulate BrainCells.
  """
  @spec carryover(map(), non_neg_integer()) :: map()
  def carryover(%{} = first_si, gap_ms) when is_integer(gap_ms) and gap_ms >= 0 do
    recruited =
      first_si
      |> Activation.recruitment_ids()
      |> Enum.reduce(%{}, fn id, acc ->
        Map.update(acc, id, Activation.recruitment_delta(), &(&1 + Activation.recruitment_delta()))
      end)

    reports =
      first_si
      |> Activation.feedback_plan()
      |> Enum.reduce(recruited, fn %{id: id, delta: delta}, acc ->
        Map.update(acc, id, delta, &(&1 + delta))
      end)

    Map.new(reports, fn {id, activation} ->
      {id, Brain.Cell.Activation.decay(activation, 0, gap_ms)}
    end)
    |> Enum.reject(fn {_, activation} -> activation <= 0 end)
    |> Map.new()
  end

  def carryover(_, _), do: %{}

  @doc "Compact outcome report without interpreting a neural ranking as truth."
  @spec summarize(map()) :: map()
  def summarize(%{} = si) do
    comprehension = Map.get(si, :comprehension) || %{}

    %{
      intent: Map.get(si, :intent),
      intent_confidence: Map.get(si, :confidence),
      senses: si |> choices() |> Enum.map(&choice_summary/1),
      comprehension_degraded?: Map.get(comprehension, :degraded?),
      comprehension_reasons: Map.get(comprehension, :reasons, []),
      verified_lexical_meanings: Map.get(comprehension, :lexical_meanings, []),
      uncertain: Map.get(comprehension, :uncertain, []),
      sentence_meaning_status: get(Map.get(si, :sentence_meaning), :status),
      projected_event_count: length(List.wrap(Map.get(si, :episodic_candidates))),
      active_prior_candidates:
        si |> Map.get(:active_cells, []) |> List.wrap()
        |> Enum.count(&(get(&1, :runtime_primed?) == true)),
      feedback_plan: Activation.feedback_plan(si)
    }
  end

  def summarize(_), do: %{}

  @doc "Report changed sense IDs by token index, without calling either one correct."
  @spec compare(map(), map()) :: map()
  def compare(baseline, neural) do
    old = Map.new(Map.get(baseline, :senses, []), &{&1.token_index, &1.id})
    new = Map.new(Map.get(neural, :senses, []), &{&1.token_index, &1.id})

    changed =
      (Map.keys(old) ++ Map.keys(new))
      |> Enum.uniq()
      |> Enum.sort()
      |> Enum.flat_map(fn idx ->
        if Map.get(old, idx) == Map.get(new, idx) do
          []
        else
          [%{token_index: idx, baseline_id: Map.get(old, idx), neural_id: Map.get(new, idx)}]
        end
      end)

    %{
      changed_sense_selections: changed,
      degraded_changed?:
        Map.get(baseline, :comprehension_degraded?) != Map.get(neural, :comprehension_degraded?),
      projected_event_count_delta:
        Map.get(neural, :projected_event_count, 0) - Map.get(baseline, :projected_event_count, 0)
    }
  end

  @doc """
  Answer a follow-up from a *current-turn revalidated* participant event only.
  This bypasses persistent Hippocampus intentionally; nil means the isolated
  projection cannot answer, not that live memory cannot recall.
  """
  @spec event_answer(map(), String.t()) :: String.t() | nil
  def event_answer(%{} = si, question) when is_binary(question) do
    event = EpisodicProjection.validated_participant(si)

    case {event, ActionQuestions.parse(question)} do
      {%{entity_key: actor_key, relation: "verb_" <> predicate} = event,
       {:forward, actor_key, predicate}} ->
        event
        |> Map.put(:status, "asserted")
        |> ActionQuestions.answer(:patient, %{predicate: predicate})

      _ ->
        nil
    end
  end

  def event_answer(_, _), do: nil

  defp choice_summary(choice) do
    %{
      token_index: get(choice, :token_index),
      id: get(choice, :id),
      pos: get(choice, :pos),
      score: get(choice, :score),
      source: get(choice, :source)
    }
  end

  defp choices(si) do
    case Map.get(si, :semantic_choices) do
      list when is_list(list) -> list
      _ -> List.wrap(Map.get(si, :lifg_choices))
    end
  end

  defp top_prior(prior) do
    prior
    |> Enum.sort_by(fn {id, value} -> {-value, id} end)
    |> Enum.take(10)
    |> Enum.map(fn {id, value} -> %{id: id, activation: Float.round(value, 5)} end)
  end

  defp bounded_gap(n) when is_integer(n) and n >= 0, do: min(n, @max_gap_ms)
  defp bounded_gap(_), do: @default_gap_ms

  defp get(%{} = m, key), do: Map.get(m, key, Map.get(m, Atom.to_string(key)))
  defp get(_, _), do: nil
end
