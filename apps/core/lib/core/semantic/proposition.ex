defmodule Core.Semantic.Proposition do
  @moduledoc """
  Build compositional propositions for resolved WH questions and
  revalidated action/spatial participant assertions.

  This is structural interpretation, not external factual verification or
  certification of selected dictionary senses. WH role sense IDs are copied
  only from ATL evidence; participant roles come from event projection whose
  validity is rechecked by SentenceMeaning. Unsupported phrases remain
  surface-only rather than acquiring invented dictionary meanings.
  """

  @max_modifiers 4

  @spec build(map(), map()) :: map() | nil
  def build(%{} = si, %{
        kind: :wh_topic_question,
        status: :structurally_interpreted,
        roles: %{
          question_word: question,
          subject: subject,
          predicate: predicate,
          preposition: preposition,
          topic: topic
        }
      } = meaning) do
    with true <- verified_word?(question),
         true <- verified_word?(subject),
         true <- verified_word?(predicate),
         true <- verified_word?(preposition),
         %{surface: surface, span: {first, last}} <- topic,
         true <- is_binary(surface) and is_integer(first) and
                   is_integer(last) and first <= last do
      nominal = compose_nominal(si, topic)

      %{
        version: 1,
        type: :information_request,
        queried: %{type: :wh_open, surface: question.surface,
          sense_id: question.sense_id},
        content: %{
          predicate: lexical_node(predicate),
          subject: %{
            role: :subject,
            reference: Map.get(subject, :reference, :unspecified),
            surface: subject.surface,
            token_index: subject.token_index,
            sense_id: subject.sense_id
          },
          complement: %{
            kind: :prepositional_topic,
            relation: lexical_node(preposition),
            argument: nominal
          }
        },
        epistemic_status: :question_not_assertion,
        polarity: Map.get(meaning, :polarity, :unspecified),
        operators: Map.get(meaning, :operators, %{}),
        grounding: %{
          grammatical_frame: :atl_resolved,
          predicate_sense: :selected_not_contextually_verified,
          topic_senses: nominal.grounding_status,
          external_facts: :not_checked
        },
        provenance: %{
          frame: :wh_aux_topic_question,
          topic_span: {first, last},
          nominal_source: nominal.source
        }
      }
    else
      _ -> nil
    end
  end

  # Validated participant assertions share the same proposition contract as
  # WH questions. The caller is SentenceMeaning, which has already checked
  # the candidate against the exact lexical/grammatical input.
  def build(_, %{
        kind: :participant_assertion,
        status: :structurally_interpreted,
        speech_act: :assertion,
        event_type: :action,
        source_frame: :finite_present_svo,
        roles: %{
          actor: %{surface: actor},
          predicate: %{surface: surface, lemma: lemma},
          patient: %{surface: patient}
        }
      } = meaning)
      when is_binary(actor) and is_binary(surface) and
           is_binary(lemma) and is_binary(patient) do
    participant_proposition(meaning, :action, %{
      predicate: %{surface: surface, lemma: lemma},
      arguments: %{actor: actor, patient: patient},
      modifiers: %{tense: :present}
    })
  end

  def build(_, %{
        kind: :participant_assertion,
        status: :structurally_interpreted,
        speech_act: :assertion,
        event_type: :spatial,
        source_frame: :progressive_spatial,
        roles: %{
          actor: %{surface: actor},
          predicate: %{surface: predicate, aspect: :progressive},
          position: %{surface: position},
          location: %{surface: location}
        }
      } = meaning)
      when is_binary(actor) and is_binary(predicate) and
           is_binary(position) and is_binary(location) do
    participant_proposition(meaning, :spatial, %{
      predicate: %{surface: predicate, aspect: :progressive},
      arguments: %{actor: actor, location: location},
      modifiers: %{position: position}
    })
  end

  def build(_, _), do: nil

  defp participant_proposition(meaning, event_type, content) do
    %{
      version: 1,
      type: :participant_assertion,
      event_type: event_type,
      epistemic_status: :user_reported_unverified,
      content: content,
      grounding: Map.fetch!(meaning, :grounding),
      provenance: %{
        source: :user_statement,
        frame: Map.fetch!(meaning, :source_frame),
        evidence: :candidate_revalidated_against_original_input,
        persistence: :not_claimed
      }
    }
  end

  defp compose_nominal(si, %{surface: surface, span: {first, last}} = topic) do
    constituents =
      si
      |> Map.get(:atl_slate, %{})
      |> case do
        %{constituents: entries} when is_list(entries) -> entries
        _ -> []
      end

    tokens =
      si
      |> Map.get(:tokens, [])
      |> index_tokens()

    # Require a constituent that covers the *whole* nominal complement.
    # An incidental noun phrase inside a longer topic is not proof of the
    # meaning of that topic. A surface genitive contributes one prefix token.
    nominal_start =
      case Map.get(topic, :possession) do
        %{token_index: ^first, grounding_status: :unverified} -> first + 1
        _ -> first
      end

    nominal =
      Enum.find(constituents, fn
        %{id: :noun_phrase, status: :resolved, span: {start, finish},
          roles: %{head: %{token_index: head_idx, sense_id: id}}}
        when start == nominal_start and finish == last and
             head_idx == last and is_binary(id) -> true
        _ -> false
      end)

    {head, modifiers, source, grounding} =
      case nominal do
        %{roles: %{head: head_role} = roles} ->
          head = role_node(head_role, tokens)

          modifiers =
            roles
            |> Enum.filter(fn {role, _} ->
              role in [:modifier, :modifier_1, :modifier_2]
            end)
            |> Enum.sort_by(fn {role, _} -> role end)
            |> Enum.take(@max_modifiers)
            |> Enum.map(fn {_role, role} -> role_node(role, tokens) end)
            |> Enum.reject(&is_nil/1)

          if head do
            {head, modifiers, :atl_resolved_noun_phrase,
             :selected_senses_not_contextually_verified}
          else
            {nil, [], :surface_only, :unverified}
          end

        _ ->
          {nil, [], :surface_only, :unverified}
      end

    %{
      type: :nominal_topic,
      surface: surface,
      span: {first, last},
      head: head,
      modifiers: modifiers,
      genitive: genitive(Map.get(topic, :possession)),
      source: source,
      grounding_status: grounding
    }
  end

  defp genitive(%{
         possessor: surface, token_index: index,
         grounding_status: :unverified
       }) when is_binary(surface) and is_integer(index) do
    %{
      type: :genitive_nominal_modifier,
      possessor: %{surface: surface, token_index: index, sense_id: nil,
        grounding_status: :unverified},
      semantic_relation: :unspecified,
      status: :syntactically_observed
    }
  end

  defp genitive(_), do: nil

  defp role_node(%{token_index: index, sense_id: id}, tokens)
       when is_integer(index) and is_binary(id) do
    case Map.get(tokens, index) do
      surface when is_binary(surface) and surface != "" ->
        %{surface: surface, token_index: index, sense_id: id,
          grounding_status: :selected_not_contextually_verified}
      _ -> nil
    end
  end

  defp role_node(_, _), do: nil

  defp lexical_node(%{surface: surface, token_index: index, sense_id: id}) do
    %{surface: surface, token_index: index, sense_id: id,
      grounding_status: :selected_not_contextually_verified}
  end

  defp verified_word?(%{surface: surface, token_index: index, sense_id: id})
       when is_binary(surface) and is_integer(index) and is_binary(id),
       do: surface != "" and id != ""

  defp verified_word?(_), do: false

  defp index_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback}, acc ->
      if is_map(token) and Map.get(token, :n, 1) == 1 do
        index = Map.get(token, :index, Map.get(token, :token_index, fallback))
        surface = Map.get(token, :phrase) || Map.get(token, :norm)

        if is_integer(index) and is_binary(surface) do
          Map.put(acc, index, trim_punctuation(surface))
        else
          acc
        end
      else
        acc
      end
    end)
  end

  defp index_tokens(_), do: %{}

  defp trim_punctuation(text) do
    text
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end
end
