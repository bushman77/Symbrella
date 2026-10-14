defmodule Core.Semantic.Proposition do
  @moduledoc """
  Build a compositional proposition from a verified sentence meaning and ATL
  constituents. This is a structural interpretation, not a factual assertion
  or a certification of the selected dictionary senses.

  No lexical evidence is synthesized: each sense_id is copied from a verified
  ATL role, and incomplete noun phrases remain surface-only.
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

  def build(_, _), do: nil

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
