defmodule Core.Brain.SemanticSenseValidator do
  @moduledoc """
  Evidence-gated, read-only second-pass validator of an *attested base lexeme*
  sense inventory. Not a general semantic verifier and not an LIFG reranker.

  A fully revalidated user SVO first passes the morphology gate in
  InflectedVerbSenseBridge. The validator then:
    * excludes dictionary senses explicitly marked intransitive-only when
      the clause has a direct object;
    * requires an explicit context witness for a narrow set of recognizable
      restricted dictionary domains/mechanisms (e.g. cricket bowling);
    * preserves lexical context-fit scores but abstains on zero evidence,
      close contenders or insufficient independent restrictions.

  Missing domain evidence means "not supported", NOT "false meaning".
  Context fit is not a probability of sense correctness. No review, learned
  weight transfer, LIFG/ATL change, BrainCell stimulation or persistence.
  """

  alias Core.Brain.InflectedVerbSenseBridge

  @min_context_fit 0.10
  @min_gap 0.10
  @max_report 12

  # Narrow, explicit dictionary-derived restriction cues. These are
  # *requirements for making a positive hypothesis*, not assertions that
  # other contexts cannot instantiate the sense. Any absent cue abstains.
  #
  # Do not add broad terms like "ball", "picture", "person" or "object":
  # their co-occurrence alone cannot establish the specialized sense.
  @restricted_domains [
    {:cricket_bowling, ~w(cricket bowling bowler)},
    {:electric_execution, ["electric chair", "electrocution", "execution"]},
    {:jazz_music, ["jazz"]},
    {:computing, ~w(computer computing software code programming)},
    {:radar, ~w(radar sonar)},
    {:grenade, ["grenade", "fuse"]},
    {:graphical_interface, ["graphical user interface", "gui"]}
  ]

  @spec evaluate(map(), keyword()) :: map()
  def evaluate(si, opts \\ [])

  def evaluate(%{} = si, opts) when is_list(opts) do
    # No alternate unverified candidate ingress: reuse the same
    # independently validated morphology-and-dictionary bridge.
    projection = InflectedVerbSenseBridge.project(
      si, Keyword.put(opts, :validation_evidence?, true)
    )

    case projection do
      %{status: :read_only_sense_inventory,
        validation_rows: rows, all_sense_ids: ids} = bridge
          when is_list(rows) and is_list(ids) ->
        if valid_full_slate?(rows, ids) do
          assess(si, bridge, rows, opts)
        else
          skipped(:incomplete_validation_inventory)
        end

      %{status: :skipped, reason: reason} ->
        skipped({:bridge_unavailable, reason})

      _ ->
        skipped(:bridge_unavailable)
    end
  end

  def evaluate(_, _), do: skipped(:invalid_input)

  @doc """
  Aggregate *diagnostic* results. Caller-supplied labels must have been
  independently reviewed; the module cannot authenticate their provenance.
  Wrong-to-right excludes abstention-to-correct, which is reported separately.
  """
  @spec aggregate([map()]) :: map()
  def aggregate(reports) when is_list(reports) do
    evaluated = Enum.filter(reports, &(Map.get(&1, :status) in
      [:provisional_hypothesis, :abstained]))
    labeled = Enum.filter(evaluated, &(&1.label_status == :caller_labeled))

    %{
      total: length(reports),
      evaluated: length(evaluated),
      skipped: length(reports) - length(evaluated),
      provisional_hypotheses: Enum.count(evaluated,
        &(&1.status == :provisional_hypothesis)),
      abstentions: Enum.count(evaluated, &(&1.status == :abstained)),
      restrictions_blocked: Enum.sum(Enum.map(evaluated, & &1.excluded_count)),
      labeled: length(labeled),
      baseline_correct: Enum.count(labeled,
        &(&1.baseline_id == &1.gold_id)),
      validator_correct: Enum.count(labeled,
        &(&1.proposed_id == &1.gold_id)),
      wrong_to_right: Enum.count(labeled,
        &(change(&1) == :wrong_to_right)),
      right_to_wrong: Enum.count(labeled,
        &(change(&1) == :right_to_wrong)),
      wrong_to_abstention: Enum.count(labeled,
        &(change(&1) == :wrong_to_abstention)),
      right_to_abstention: Enum.count(labeled,
        &(change(&1) == :right_to_abstention)),
      abstention_to_right: Enum.count(labeled,
        &(change(&1) == :abstention_to_right)),
      abstention_to_wrong: Enum.count(labeled,
        &(change(&1) == :abstention_to_wrong)),
      independently_verified_labels?: false,
      live_lifg_modified?: false
    }
  end

  def aggregate(_), do: %{status: :invalid_reports}

  defp assess(si, bridge, rows, opts) do
    context = explicit_context(si)
    reviews =
      Enum.map(rows, fn row ->
        evidence = row.validation_evidence || %{}
        grammar = grammar_gate(evidence)
        restrictions = restriction_gates(evidence, context)

        blocks =
          (case grammar do
            :intransitive_only -> [:intransitive_with_object]
            :conflicting_grammar -> [:conflicting_dictionary_grammar]
            _ -> []
          end) ++ Enum.map(restrictions, &{:missing_restriction_witness, &1})

        %{
          id: row.id,
          lexical_score: row.context_fit,
          matching_context: row.matched_context_terms,
          grammar: grammar,
          missing_restrictions: restrictions,
          exclusions: blocks,
          eligible?: blocks == [],
          dictionary_gloss: row.dictionary_definition,
          sense_verified?: false
        }
      end)

    eligible =
      reviews
      |> Enum.filter(& &1.eligible?)
      |> Enum.sort_by(fn row -> {-row.lexical_score, row.id} end)

    best = List.first(eligible)
    runner = Enum.at(eligible, 1)
    gap = if(best && runner, do: best.lexical_score - runner.lexical_score, else: 0.0)

    min_fit = Keyword.get(opts, :min_context_fit, @min_context_fit)
    min_gap = Keyword.get(opts, :min_gap, @min_gap)
    valid_thresholds? = valid_threshold?(min_fit) and valid_threshold?(min_gap)

    proposed =
      if valid_thresholds? && best && runner &&
           best.lexical_score >= min_fit && gap >= min_gap,
        do: best.id, else: nil

    reason =
      cond do
        not valid_thresholds? -> :invalid_thresholds
        is_nil(best) -> :no_compatible_dictionary_sense
        best.lexical_score < min_fit -> :insufficient_lexical_evidence
        is_nil(runner) -> :no_remaining_competition
        gap < min_gap -> :ambiguous_context_evidence
        true -> :provisional_context_hypothesis
      end

    gold = Keyword.get(opts, :gold_id)
    label_status =
      cond do
        is_nil(gold) -> :unlabeled
        is_binary(gold) && gold in bridge.all_sense_ids &&
            Keyword.get(opts, :split) == :holdout &&
            Keyword.get(opts, :label_source) in [:human_curated, :external_gold] ->
          :caller_labeled
        true -> :untrusted_label
      end

    if label_status == :untrusted_label do
      skipped(:invalid_or_untrusted_gold_label)
    else
      baseline = case bridge.lexical_hypothesis do
        %{candidate_id: id} -> id
        _ -> nil
      end

      %{
        status: if(is_binary(proposed), do: :provisional_hypothesis, else: :abstained),
        reason: reason,
        original_surface_id: bridge.original_surface_id,
        attested_base_lexeme: bridge.attested_base_lexeme,
        sense_count: length(rows),
        scored_all_dictionary_senses?: true,
        baseline_id: baseline,
        proposed_id: proposed,
        baseline_score_gap: bridge.top_two_gap,
        validator_score_gap: gap,
        excluded_count: Enum.count(reviews, &(not &1.eligible?)),
        exclusions: reviews
          |> Enum.reject(& &1.eligible?)
          |> Enum.take(@max_report)
          |> Enum.map(&Map.take(&1,
            [:id, :lexical_score, :grammar, :missing_restrictions, :exclusions])),
        eligible_top: Enum.take(eligible, @max_report),
        label_status: label_status,
        gold_id: if(label_status == :caller_labeled, do: gold, else: nil),
        baseline_correct?: if(label_status == :caller_labeled, do: baseline == gold, else: nil),
        validator_correct?: if(label_status == :caller_labeled, do: proposed == gold, else: nil),
        label_verified_by_system?: false,
        sense_meaning_verified?: false,
        synaptic_weight_transferred?: false,
        applied_to_lifg?: false,
        brain_cells_stimulated?: false
      }
    end
  end

  defp valid_full_slate?(rows, ids) do
    length(rows) >= 2 and length(rows) <= 256 and
      length(ids) == length(rows) and
      MapSet.new(Enum.map(rows, & &1.id)) == MapSet.new(ids) and
      Enum.all?(rows, fn row ->
        is_map(row.validation_evidence) and
          is_binary(row.id) and is_number(row.context_fit) and
          row.context_fit >= 0.0 and row.context_fit <= 1.0
      end)
  end

  defp grammar_gate(evidence) do
    grammar = List.wrap(Map.get(evidence, :gram_function))
    description = Map.get(evidence, :definition) || ""
    # Distinguish explicit intransitive-only from ambitransitive or
    # a gloss that describes both transitive and intransitive uses.
    tags = Enum.map(grammar, &String.downcase(to_string(&1)))
    labels = leading_grammar_labels(description)
    supports_object? = fn values ->
      Enum.any?(values, &(&1 in ["transitive", "ambitransitive", "ditransitive"]))
    end

    cond do
      "intransitive" in labels and supports_object?.(labels) ->
        :conflicting_grammar
      "intransitive" in labels and supports_object?.(tags) ->
        :conflicting_grammar
      "intransitive" in labels ->
        :intransitive_only
      supports_object?.(labels) and "intransitive" in tags and
          not supports_object?.(tags) ->
        :conflicting_grammar
      supports_object?.(labels) ->
        :compatible_direct_object
      "intransitive" in tags and supports_object?.(tags) ->
        :conflicting_grammar
      supports_object?.(tags) ->
        :compatible_direct_object
      "intransitive" in tags ->
        :intransitive_only
      true ->
        :unspecified
    end
  end

  defp leading_grammar_labels(description) when is_binary(description) do
    case Regex.run(~r/\A\(([^)]{1,120})\)/u, description) do
      [_, labels] ->
        labels
        |> String.downcase()
        |> String.split(~r/[\s,;]+/u, trim: true)
        |> Enum.filter(&(&1 in ["transitive", "intransitive", "ambitransitive", "ditransitive"]))
      _ -> []
    end
  end

  defp leading_grammar_labels(_), do: []

  defp restriction_gates(evidence, context) do
    gloss = String.downcase(Map.get(evidence, :definition) || "")
    atoms = Map.get(evidence, :semantic_atoms) |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.downcase/1)
    # Any mention in a long gloss is NOT a reliable domain tag. Only
    # explicitly categorized senses, a leading usage qualifier, and a
    # small set of unambiguous mechanisms trigger mandatory evidence.
    labels =
      case Regex.run(~r/\A\(([^)]{1,120})\)/u, gloss) do
        [_, value] -> value
        _ -> ""
      end

    Enum.flat_map(@restricted_domains, fn {domain, cues} ->
      explicit_category? =
        Enum.any?(atoms, fn a ->
          Enum.any?(cues, fn cue ->
            a in ["domain:" <> cue, "topic:" <> cue, "cat:en:" <> cue]
          end)
        end)

      explicit_gloss? =
        case domain do
          :cricket_bowling -> String.contains?(gloss, "bowling arm") ||
            word_in?(labels, "cricket")
          :electric_execution -> String.contains?(gloss, "electric chair")
          :jazz_music -> word_in?(labels, "jazz")
          :computing -> word_in?(labels, "computing")
          :radar -> word_in?(labels, "radar")
          :grenade -> word_in?(gloss, "grenade")
          :graphical_interface -> String.contains?(gloss, "graphical user interface")
        end

      if (explicit_category? or explicit_gloss?) and
           not Enum.any?(cues, &context_cue?(context, &1)),
        do: [domain], else: []
    end)
  end

  defp context_cue?(context, cue) do
    cue
    |> String.split(" ", trim: true)
    |> Enum.all?(&MapSet.member?(context, &1))
  end

  defp word_in?(text, word) do
    Regex.scan(~r/[\p{L}\p{N}]+/u, text)
    |> List.flatten()
    |> Enum.member?(word)
  end

  defp explicit_context(si) do
    si
    |> Map.get(:sentence, "")
    |> String.downcase()
    |> then(&Regex.scan(~r/[\p{L}\p{N}]+/u, &1))
    |> List.flatten()
    |> MapSet.new()
  end

  defp valid_threshold?(value) do
    is_number(value) && value >= 0.0 && value <= 1.0
  end

  defp change(report) do
    baseline = report.baseline_id
    projected = report.proposed_id
    gold = report.gold_id

    cond do
      baseline == projected -> :unchanged
      is_nil(baseline) && projected == gold -> :abstention_to_right
      is_nil(baseline) -> :abstention_to_wrong
      is_nil(projected) && baseline == gold -> :right_to_abstention
      is_nil(projected) -> :wrong_to_abstention
      baseline == gold -> :right_to_wrong
      projected == gold -> :wrong_to_right
      true -> :different_wrong_prediction
    end
  end

  defp skipped(reason), do: %{
    status: :skipped,
    reason: reason,
    applied_to_lifg?: false,
    synaptic_weight_transferred?: false
  }
end
