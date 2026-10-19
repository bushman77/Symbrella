defmodule Core.Brain.SemanticPropagation do
  @moduledoc """
  Evidence-gated, bounded *routing* between symbolic role interpretations
  and transient neuron activity. This does not establish synapses, train
  weights, verify exact senses or write any durable memory.

  1. A currently revalidated finite-present participant event may give its
     already-selected subject, predicate and object neurons small support.
  2. A fully interpreted WH-object question may similarly support its subject
     and verb, never the unbound patient.
  3. Only an explicit form-of dictionary witness may produce a short-lived
     verb-lemma pulse. That pulse is NOT a BrainCell/sense and cannot resolve
     an arbitrary base-lemma sense.
  """

  alias Core.Semantic.{EpisodicProjection, SVOEvent, VerbLemma}

  @role_delta 0.025
  @lemma_delta 0.03

  @spec role_plan(map()) :: [map()]
  def role_plan(%{} = si) do
    meaning = Map.get(si, :sentence_meaning) || %{}

    roles =
      case meaning do
        %{kind: :participant_assertion, event_type: :action,
          status: :structurally_interpreted, source_frame: :finite_present_svo} ->
          case EpisodicProjection.validated_participant(si) do
            %{source_frame: :finite_present_svo} -> participant_roles(si)
            _ -> []
          end

        %{kind: :wh_object_question, status: :structurally_interpreted,
          source_frame: :wh_aux_object_question,
          proposition: %{type: :information_request,
            queried: %{role: :patient, status: :unbound}}} ->
          question_roles(si, meaning)

        _ ->
          []
      end

    roles
    |> Enum.uniq()
    |> Enum.sort_by(fn {index, _} -> index end)
    |> Enum.flat_map(fn {index, pos} ->
      case selected_id(si, index, pos) do
        id when is_binary(id) ->
          [%{id: id, delta: @role_delta, evidence: :structural_role_not_lexically_verified}]
        _ -> []
      end
    end)
    |> Enum.take(3)
  end

  def role_plan(_), do: []

  @doc """
  A lemma signal requires the *same revalidated* finite present event, an
  exactly selected surface verb sense, and VerbLemma's explicit morphology
  witness. Unverified or unchanged-lemma verbs produce no pulse.
  """
  @spec lemma_pulse(map()) :: map() | nil
  def lemma_pulse(%{} = si) do
    with %{kind: :participant_assertion, event_type: :action,
           status: :structurally_interpreted, source_frame: :finite_present_svo} <-
           Map.get(si, :sentence_meaning),
         %{source_frame: :finite_present_svo,
           participants: %{"predicate" => lemma, "surface_predicate" => surface},
           relation: relation} <- EpisodicProjection.validated_participant(si),
         true <- relation == "verb_" <> lemma,
         %{roles: %{predicate: index}} <- participant_signature(si),
         %{id: id} = choice <- one_choice(si, index),
         true <- id_pos(id) == "verb",
         {:ok, ^lemma} <- VerbLemma.canonical(si, choice),
         [selected_surface, "verb", _] <- String.split(id, "|", parts: 3),
         true <- selected_surface == surface and surface != lemma do
      %{lemma: lemma, source_id: id, delta: @lemma_delta,
        evidence: :attested_inflected_predicate, scope: :verb_lemma_not_sense}
    else
      _ -> nil
    end
  end

  def lemma_pulse(_), do: nil

  defp participant_signature(si) do
    # SVOEvent already checks that the signature covers the entire original
    # utterance and has exactly one interpretation. Searching *all* finite
    # present hypotheses here can accidentally count partial substrings as
    # competing complete clauses (e.g. "the artist draws a").
    SVOEvent.complete_signature(si)
  end

  defp participant_roles(si) do
    case participant_signature(si) do
      %{roles: %{subject_head: subject, predicate: predicate, object_head: object}} ->
        [{subject, "noun"}, {predicate, "verb"}, {object, "noun"}]
      _ -> []
    end
  end

  defp question_roles(si, meaning) do
    sigs = si |> Map.get(:sentence_signatures, []) |> List.wrap()

    complete? =
      Enum.count(sigs, fn
        %{id: :wh_aux_object_question, source: :core_syntax, span: {0, 4},
          roles: %{subject_head: 3, predicate: 4}} -> true
        _ -> false
      end) == 1

    roles = Map.get(meaning, :roles, %{})

    rebuilt = Core.Semantic.SentenceMeaning.build(si)

    if complete? and rebuilt == meaning and
         match?(%{subject: %{head_sense_id: _}, predicate: %{sense_id: _}}, roles) and
         Map.get(roles.subject, :head_sense_id) == selected_id(si, 3, "noun") and
         Map.get(roles.predicate, :sense_id) == selected_id(si, 4, "verb") do
      [{3, "noun"}, {4, "verb"}]
    else
      []
    end
  end

  defp selected_id(si, index, pos) do
    with %{id: id} <- one_choice(si, index),
         true <- id_pos(id) == pos and not String.ends_with?(id, "|fallback"),
         true <- Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
           %{id: ^id} -> true
           _ -> false
         end),
         %{winners: winners} when is_list(winners) <- Map.get(si, :atl_slate),
         [%{id: ^id}] <- Enum.filter(winners, &(Map.get(&1, :token_index) == index)) do
      id
    else
      _ -> nil
    end
  end

  defp one_choice(si, index) do
    choices = Map.get(si, :lifg_choices)
    if is_list(choices) do
      case Enum.filter(choices, &(is_map(&1) and Map.get(&1, :token_index) == index)) do
        [choice] ->
          case Map.get(choice, :id) || Map.get(choice, :chosen_id) do
            id when is_binary(id) -> Map.put(choice, :id, id)
            _ -> nil
          end
        _ -> nil
      end
    end
  end

  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp id_pos(_), do: nil
end
