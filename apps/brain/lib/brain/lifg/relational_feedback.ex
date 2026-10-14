defmodule Brain.LIFG.RelationalFeedback do
  @moduledoc """
  One bounded, post-ATL semantic-control pass.

  ATL provides *structural* token relationships. The contextual cues are the
  other tokens' original surfaces, never their selected sense IDs, definitions,
  proposition glosses or derived confidence. Dictionary definitions for the
  token under evaluation are the only semantic evidence used for reweighting.

  The evaluator is pure. Core is responsible for applying a safe revision and
  re-running ATL when a winner changes.
  """

  alias Brain.LIFG.{ContextFit, SemanticControl}

  @max_edges 16
  @max_cues 2
  @max_candidates 64

  @spec evaluate(map(), keyword()) :: map()
  def evaluate(si, opts \\ [])
  def evaluate(%{} = si, opts) when is_list(opts) do
    edges = structural_edges(si)
    choices = Map.get(si, :lifg_choices, []) |> List.wrap()
    candidate_map = Map.get(si, :sense_candidates) || %{}

    decisions =
      edges
      |> Enum.group_by(& &1.target_index)
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {index, related} ->
        choice = Enum.find(choices, &(Map.get(&1, :token_index) == index))
        bucket = Map.get(candidate_map, index) || Map.get(candidate_map, Integer.to_string(index)) || []
        evaluate_target(index, related, choice, bucket, opts)
      end)

    %{
      version: 1,
      source: :atl_structural_relations_and_original_token_surfaces,
      status: if(edges == [], do: :no_verified_relations, else: :evaluated),
      edges_considered: length(edges),
      decisions: decisions,
      recommended_changes: Enum.count(decisions, &(Map.get(&1, :revision?) == true)),
      constraint: :no_self_sense_evidence
    }
  end

  def evaluate(_, _), do: %{
    version: 1, source: :atl_structural_relations_and_original_token_surfaces,
    status: :invalid_input, edges_considered: 0, decisions: [],
    recommended_changes: 0, constraint: :no_self_sense_evidence
  }

  @doc "Extract only relationships whose indices match both Syntax and ATL."
  @spec structural_edges(map()) :: [map()]
  def structural_edges(%{} = si) do
    tokens = indexed_tokens(Map.get(si, :tokens, []))
    signatures = Map.get(si, :sentence_signatures, []) |> List.wrap()
    constituents =
      case Map.get(si, :atl_slate) do
        %{constituents: items} when is_list(items) -> items
        _ -> []
      end

    nominal =
      constituents
      |> Enum.flat_map(fn
        %{id: :noun_phrase, status: :resolved,
          span: span, roles: %{head: head, modifier: modifier}} = c ->
          if matched?(c, signatures, [:head, :modifier]) do
            edge_pair(tokens, head, modifier, :nominal_modifier, span)
          else
            []
          end
        _ -> []
      end)

    topic =
      constituents
      |> Enum.flat_map(fn
        %{id: :wh_aux_topic_question, status: :resolved,
          span: {start, last}, roles: roles} = c ->
          if matched?(c, signatures, [:predicate, :preposition]) do
            with %{token_index: predicate} <- Map.get(roles, :predicate),
                 %{token_index: prep} <- Map.get(roles, :preposition),
                 true <- predicate >= start and prep > predicate and prep < last do
              # Choose the closest resolved nominal constituent wholly after
              # the topic preposition. Never use the nominal's selected sense.
              closest =
                Enum.find(constituents, fn
                  %{id: :noun_phrase, status: :resolved,
                    span: {first, finish}} = n ->
                    first > prep and finish <= last and
                      matched?(n, signatures, [:head])
                  _ -> false
                end)

              case closest do
                %{roles: %{head: head}, span: span} ->
                  # The topic informs predicate disambiguation. The generic
                  # predicate (e.g. "know") must not become lexical evidence
                  # for the nominal topic in the reverse direction.
                  directed_edge(tokens, %{token_index: predicate}, head,
                    :predicate_topic, span)
                _ -> []
              end
            else
              _ -> []
            end
          else
            []
          end
        _ -> []
      end)

    (nominal ++ topic)
    |> Enum.uniq_by(&{&1.target_index, &1.partner_index, &1.relation})
    |> Enum.take(@max_edges)
  end

  def structural_edges(_), do: []

  defp evaluate_target(index, edges, choice, bucket, opts) do
    chosen = if is_map(choice), do: Map.get(choice, :id), else: nil
    probs = if is_map(choice), do: Map.get(choice, :candidate_probs), else: nil

    cues =
      edges
      |> Enum.map(& &1.partner_surface)
      |> Enum.uniq()
      |> Enum.take(@max_cues)

    provenance = Enum.map(edges, &Map.take(&1,
      [:relation, :partner_index, :partner_surface, :span]))

    base = %{
      token_index: index, selected_id: chosen, context_cues: cues,
      relations: provenance, revision?: false, proposed_id: chosen,
      adjustments: []
    }

    cond do
      not is_binary(chosen) or not is_map(probs) or map_size(probs) < 2 or
          map_size(probs) > @max_candidates ->
        Map.merge(base, %{status: :missing_complete_probabilities})

      not complete_distribution?(probs) ->
        Map.merge(base, %{status: :incomplete_probability_distribution})

      not Map.has_key?(probs, chosen) or
          Map.get(probs, chosen, 0.0) + 1.0e-6 < Enum.max(Map.values(probs)) ->
        # Core's canonical choice may differ from the Stage1 raw winner after
        # grammar or MWE compatibility selection. Such a prior cannot be
        # safely revised without redoing that selection first.
        Map.merge(base, %{status: :prior_winner_mismatch})

      not is_list(bucket) or length(bucket) > @max_candidates ->
        Map.merge(base, %{status: :candidate_bucket_unavailable})

      true ->
        selected_pos = sense_pos(chosen)
        rows =
          bucket
          |> Enum.flat_map(fn
            %{} = candidate ->
              id = get(candidate, :id)
              if is_binary(id) and sense_pos(id) == selected_pos and
                   Map.has_key?(probs, id) and lexical_evidence?(candidate) do
                fit =
                  ContextFit.explain(nil, sense_lemma(id), candidate,
                    extra_context: cues).score
                [%{id: id, context_evidence: fit}]
              else
                []
              end
            _ -> []
          end)

        {_ignored_scored_map, report} =
          SemanticControl.apply(
            probs,
            rows,
            max_delta: Keyword.get(opts, :max_delta, 0.10),
            min_fit: Keyword.get(opts, :min_fit, 0.18),
            min_gap: Keyword.get(opts, :min_gap, 0.15),
            focus_pos: selected_pos
          )

        if report.applied? do
          adjustments =
            Enum.map(report.adjustments, fn adjustment ->
              Map.put(adjustment, :source,
                :atl_structural_relation_dictionary_context)
            end)

          # SemanticControl supplies bounded logit deltas; do not treat the
          # resulting probability-domain score map as a posterior distribution.
          # Instead add deltas to log(prior) and renormalize exactly once.
          adjustments_by_id = Map.new(adjustments, &{&1.id, &1.delta})
          logits = Map.new(probs, fn {id, probability} ->
            {id, :math.log(probability) + Map.get(adjustments_by_id, id, 0.0)}
          end)
          posterior = normalize_logits(logits)
          {proposed, _} =
            Enum.max_by(posterior, fn {id, value} -> {value, id} end)

          Map.merge(base, %{
            status: if(proposed == chosen, do: :supported_existing_choice,
              else: :revision_proposed),
            revision?: proposed != chosen and sense_pos(proposed) == selected_pos and
              Enum.any?(adjustments, &(&1.id == proposed)),
            proposed_id: proposed,
            adjustments: adjustments,
            posterior: posterior,
            original_prob: Map.get(probs, chosen),
            proposed_prob: Map.get(posterior, proposed)
          })
        else
          Map.merge(base, %{status: :insufficient_independent_evidence})
        end
    end
  end

  defp matched?(%{id: id, span: span, roles: actual}, signatures, names) do
    Enum.any?(signatures, fn
      %{id: ^id, span: ^span, roles: expected} ->
        Enum.all?(names, fn role ->
          case {Map.get(expected, role), Map.get(actual, role)} do
            {idx, %{token_index: role_index, sense_id: sense}}
            when is_integer(idx) and role_index == idx and is_binary(sense) -> true
            _ -> false
          end
        end)
      _ -> false
    end)
  end

  defp edge_pair(tokens, %{token_index: a}, %{token_index: b}, relation, span)
       when is_integer(a) and is_integer(b) and a != b do
    case {Map.get(tokens, a), Map.get(tokens, b)} do
      {aa, bb} when is_binary(aa) and is_binary(bb) and aa != "" and bb != "" ->
        [
          %{target_index: a, partner_index: b, partner_surface: bb,
            relation: relation, span: span},
          %{target_index: b, partner_index: a, partner_surface: aa,
            relation: relation, span: span}
        ]
      _ -> []
    end
  end

  defp edge_pair(_, _, _, _, _), do: []

  defp directed_edge(tokens, %{token_index: a}, %{token_index: b}, relation, span)
       when is_integer(a) and is_integer(b) and a != b do
    case {Map.get(tokens, a), Map.get(tokens, b)} do
      {aa, bb} when is_binary(aa) and is_binary(bb) and aa != "" and bb != "" ->
        [%{target_index: a, partner_index: b, partner_surface: bb,
          relation: relation, span: span}]
      _ -> []
    end
  end

  defp directed_edge(_, _, _, _, _), do: []

  defp indexed_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback}, acc ->
      index = get(token, :index) || fallback
      surface = get(token, :phrase) || get(token, :norm)
      if is_integer(index) and get(token, :n, 1) == 1 and is_binary(surface) do
        Map.put(acc, index, surface
          |> String.trim()
          |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
          |> String.downcase())
      else
        acc
      end
    end)
  end

  defp indexed_tokens(_), do: %{}

  defp lexical_evidence?(candidate) do
    Enum.any?([:definition, :example, :synonyms, :semantic_atoms, :gram_function],
      fn key ->
        case get(candidate, key) do
          value when is_binary(value) -> String.trim(value) != ""
          value when is_list(value) -> value != []
          _ -> false
        end
      end)
  end

  defp complete_distribution?(probs) do
    Enum.all?(probs, fn {id, p} ->
      is_binary(id) and is_number(p) and p > 0.0 and p <= 1.0
    end) and abs(Enum.sum(Map.values(probs)) - 1.0) < 0.005
  end

  defp normalize_logits(logits) do
    highest = logits |> Map.values() |> Enum.max()
    weights = Map.new(logits, fn {id, logit} -> {id, :math.exp(logit - highest)} end)
    total = Enum.sum(Map.values(weights))
    Map.new(weights, fn {id, weight} -> {id, weight / total} end)
  end

  defp sense_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp sense_pos(_), do: nil

  defp sense_lemma(id) do
    id |> String.split("|", parts: 2) |> hd()
  end

  defp get(map, key, fallback \\ nil)
  defp get(%{} = map, key, fallback),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key), fallback))
  defp get(_, _, fallback), do: fallback
end
