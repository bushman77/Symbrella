defmodule SymbrellaWeb.SocialEvaluation do
  @moduledoc """
  P-009: operator-initiated, read-only Colony comprehension evaluation.

  Samples at most 50 sentences from a currently fetched public discussion.
  All interpretations are results of the existing isolated SemanticProbe.
  Nothing is trained, admitted into WM, persisted, posted or published.

  Human annotations are ephemeral LiveView assigns until an operator chooses
  to copy a JSONL dataset. A structural candidate is *coverage evidence*,
  never an automatically correct semantic interpretation. Held-out membership
  is stable by normalized excerpt fingerprint, including across threads.
  """

  alias Core.Social.SemanticProbe
  alias SymbrellaWeb.SocialCognitiveReport

  @max_samples 50
  @max_excerpt 600
  @expected %{
    "unlabeled" => :unlabeled,
    "interpret" => :interpret,
    "abstain" => :abstain,
    "needs_context" => :needs_context,
    "exclude" => :exclude
  }
  @families %{
    "unlabeled" => :unlabeled,
    "statement" => :statement,
    "question" => :question,
    "social" => :social,
    "fragment" => :fragment,
    "negation" => :negation,
    "other" => :other
  }
  @reviews %{
    "unreviewed" => :unreviewed,
    "correct" => :correct,
    "incorrect" => :incorrect,
    "uncertain" => :uncertain
  }
  @degradation_reasons [
    :no_lifg_choices, :weak_decision_rate_high, :fallback_rate_high,
    :unresolved_content_tokens
  ]
  @meaning_statuses [:structurally_interpreted, :provisional,
                     :not_yet_interpreted]
  @structure_statuses [:preferred, :unresolved, :no_complete_candidate,
                       :ambiguous]

  @doc "Start an explicitly requested evaluation from already loaded SocialData."
  def analyze(input, opts \\ [])

  def analyze(%{topic: %{id: post_id}, comments: comments} = input, opts)
      when is_binary(post_id) and is_list(comments) and is_list(opts) do
    probe = Keyword.get(opts, :probe, &SemanticProbe.run/2)
    reporter = Keyword.get(opts, :reporter, &SocialCognitiveReport.analyze/2)

    # The evaluation path always forces no neuron stimulation, even if caller
    # supplied contradictory option keys. Existing SocialCognitiveReport
    # forces emit_telemetry?: false for each probe.
    with {:ok, %{
           source: :colony_external, trust: :untrusted_external,
           mode: :read_only_shadow, segments: segments
         } = report} <-
           reporter.(input,
             target: :evaluation, brain_observation?: false, probe: probe
           ) do
      {items, duplicates} = build_items(segments)

      {:ok, %{
        post_id: post_id,
        source: :colony_external,
        trust: :untrusted_external,
        mode: :read_only_evaluation,
        items: items,
        sampled: length(items),
        duplicates: duplicates,
        omitted_replies: report.omitted_replies,
        omitted_sentences: report.omitted_sentences,
        limit: @max_samples,
        annotation_status: :operator_unlabeled
      }}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :evaluation_unavailable}
    end
  end

  def analyze(_, _), do: {:error, :invalid_evaluation_input}

  def expected_options, do: Map.keys(@expected) |> Enum.sort()
  def family_options, do: Map.keys(@families) |> Enum.sort()
  def review_options, do: Map.keys(@reviews) |> Enum.sort()

  def default_label, do: %{expected: :unlabeled, family: :unlabeled, review: :unreviewed}

  def selected?(labels, %{id: id}, field, value)
      when is_map(labels) and is_atom(field) and is_binary(value) do
    label = Map.get(labels, id, default_label())
    case Map.get(label, field) do
      atom when is_atom(atom) -> Atom.to_string(atom) == value
      _ -> false
    end
  end

  @doc "Update only allowlisted label fields, and only for a sampled item."
  def put_label(labels, %{id: id}, params)
      when is_map(labels) and is_binary(id) and is_map(params) do
    prior = Map.get(labels, id, default_label())

    next =
      Enum.reduce(
        [{"expected", :expected, @expected},
         {"family", :family, @families},
         {"review", :review, @reviews}],
        prior,
        fn {field, key, allowed}, acc ->
          case Map.get(params, field) do
            value when is_binary(value) ->
              case Map.fetch(allowed, value) do
                {:ok, parsed} -> Map.put(acc, key, parsed)
                :error -> acc
              end

            _ -> acc
          end
        end
      )

    Map.put(labels, id, next)
  end

  def put_label(labels, _, _), do: labels

  @doc """
  Report *coverage* and operator judgments separately for development and
  held-out samples. No unlabeled item contributes to either agreement score.
  """
  def summary(%{items: items}, labels) when is_list(items) and is_map(labels) do
    %{
      development: group_summary(items, labels, :development),
      heldout: group_summary(items, labels, :heldout),
      total: length(items)
    }
  end

  def summary(_, _), do: %{
    development: empty_counts(), heldout: empty_counts(), total: 0
  }

  @doc """
  Export only on explicit operator copy. JSONL includes untrusted excerpts,
  immutable content IDs, source position, observed evidence and manual labels.
  Copying is not a database, upload, publishing action or memory write.
  """
  def export_jsonl(%{post_id: post_id, items: items}, labels)
      when is_binary(post_id) and is_list(items) and is_map(labels) do
    items
    |> Enum.map(fn item ->
      label = Map.get(labels, item.id, default_label())
      Jason.encode!(%{
        schema: "symbrella_social_eval_v1",
        origin: :colony_external,
        trust: :untrusted_external,
        post_id: post_id,
        id: item.id,
        fold: item.fold,
        source_type: item.source_type,
        source_index: item.source_index,
        sample_index: item.sample_index,
        excerpt: item.excerpt,
        expected: label.expected,
        family: label.family,
        human_review: label.review,
        observed: item.observed,
        lexical_choices: item.lexical_choices,
        degraded: item.degraded,
        structural_status: item.structural_status,
        meaning_status: item.meaning_status,
        reason_codes: item.reason_codes
      })
    end)
    |> Enum.join("\n")
  end

  def export_jsonl(_, _), do: ""

  defp build_items(segments) when is_list(segments) do
    segments
    |> Enum.take(@max_samples)
    |> Enum.reduce({[], MapSet.new(), 0}, fn segment, {items, seen, duplicates} ->
      text =
        case Map.get(segment, :excerpt) do
          excerpt when is_binary(excerpt) ->
            excerpt |> String.trim() |> String.slice(0, @max_excerpt)
          _ -> ""
        end

      if text == "" do
        {items, seen, duplicates}
      else
        # Hash the sentence, not source indices: duplicate text always enters
        # the same fold, including when sampled in a different thread.
        digest = :crypto.hash(:sha256, String.downcase(text))
        id = Base.encode16(binary_part(digest, 0, 12), case: :lower)

        if MapSet.member?(seen, id) do
          {items, seen, duplicates + 1}
        else
          fold =
            if rem(:binary.decode_unsigned(binary_part(digest, 0, 2)), 5) == 0,
              do: :heldout, else: :development

          item = %{
            id: id,
            fold: fold,
            excerpt: text,
            source_type: Map.get(segment, :source_type),
            source_index: Map.get(segment, :source_index),
            sample_index: Map.get(segment, :sample_index),
            observed: observed(segment),
            lexical_choices: bounded_count(get_in(segment, [:result, :lifg, :chosen_count])),
            degraded: get_in(segment, [:result, :comprehension, :degraded?]) == true,
            structural_status: allow_status(
              get_in(segment, [:result, :stage_diagnostic, :structural_competition]),
              @structure_statuses
            ),
            meaning_status: allow_status(
              get_in(segment, [:result, :sentence_meaning, :status]),
              @meaning_statuses
            ),
            reason_codes:
              allowed_reasons(get_in(segment, [:result, :comprehension, :reasons]))
          }

          {[item | items], MapSet.put(seen, id), duplicates}
        end
      end
    end)
    |> then(fn {items, _, duplicates} -> {Enum.reverse(items), duplicates} end)
  end

  defp build_items(_), do: {[], 0}

  defp allowed_reasons(reasons) when is_list(reasons) do
    reasons
    |> Enum.filter(&(&1 in @degradation_reasons))
    |> Enum.uniq()
    |> Enum.take(4)
  end

  defp allowed_reasons(_), do: []

  defp observed(%{
         status: :analyzed,
         result: %{
           stage: :social_semantic_shadow,
           stage_diagnostic: %{
             pipeline: :read_only_external_projection,
             external_source_isolation: :enforced,
             runtime_lifg_ingest: :not_performed,
             runtime_atl_ingest: :not_performed,
             working_memory_admission: :not_performed,
             episodic_persistence: :not_performed
           },
           sentence_meaning: %{status: status}
         }
       }) do
    case status do
      :structurally_interpreted -> :structural_candidate
      :provisional -> :provisional_candidate
      :not_yet_interpreted -> :not_interpreted
      _ -> :unclassified
    end
  end

  defp observed(_), do: :infrastructure_failure

  defp allow_status(value, accepted) do
    if value in accepted, do: value, else: :not_observed
  end

  defp bounded_count(n) when is_integer(n) and n >= 0 and n <= 64, do: n
  defp bounded_count(_), do: nil

  defp empty_counts, do: %{
    samples: 0, labeled: 0, coverage_candidates: 0, coverage_gaps: 0,
    expected_abstentions: 0, unexpected_candidates: 0,
    infrastructure_failures: 0, human_correct: 0, human_incorrect: 0,
    human_uncertain: 0
  }

  defp group_summary(items, labels, fold) do
    items
    |> Enum.filter(&(&1.fold == fold))
    |> Enum.reduce(empty_counts(), fn item, acc ->
      label = Map.get(labels, item.id, default_label())
      acc = Map.update!(acc, :samples, &(&1 + 1))
      acc =
        if label.expected in [:interpret, :abstain, :needs_context],
          do: Map.update!(acc, :labeled, &(&1 + 1)), else: acc

      acc =
        case {label.expected, item.observed} do
          {:interpret, :structural_candidate} ->
            Map.update!(acc, :coverage_candidates, &(&1 + 1))
          {:interpret, :not_interpreted} ->
            Map.update!(acc, :coverage_gaps, &(&1 + 1))
          {:abstain, :not_interpreted} ->
            Map.update!(acc, :expected_abstentions, &(&1 + 1))
          {:abstain, :structural_candidate} ->
            Map.update!(acc, :unexpected_candidates, &(&1 + 1))
          _ -> acc
        end

      acc =
        if item.observed == :infrastructure_failure,
          do: Map.update!(acc, :infrastructure_failures, &(&1 + 1)),
          else: acc

      if label.expected in [:interpret, :abstain, :needs_context] do
        case label.review do
          :correct -> Map.update!(acc, :human_correct, &(&1 + 1))
          :incorrect -> Map.update!(acc, :human_incorrect, &(&1 + 1))
          :uncertain -> Map.update!(acc, :human_uncertain, &(&1 + 1))
          _ -> acc
        end
      else
        acc
      end
    end)
  end
end
