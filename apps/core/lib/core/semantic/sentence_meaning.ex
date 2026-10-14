defmodule Core.Semantic.SentenceMeaning do
  @moduledoc """
  A conservative, sentence-level interpretation assembled from independently
  observed grammatical roles and ATL lexical winners.

  This is *not* a dictionary entry for a whole sentence. It records what the
  sentence asks or asserts, where its components came from, and what remains
  ungrounded. No BrainCell IDs, external facts, or sense confidences are invented.
  """

  alias Core.Pipeline.Trace

  @max_topic_tokens 16

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    meaning = build(si)

    si
    |> Map.put(:sentence_meaning, meaning)
    |> Trace.append(:sentence_meaning,
      decision: meaning.status,
      reason: :compositional_sentence_interpretation,
      meta: meaning
    )
  end

  def attach(other), do: other

  @spec build(map()) :: map()
  def build(%{} = si) do
    sentence = Map.get(si, :sentence)
    tokens = indexed_base_tokens(Map.get(si, :tokens, []))
    frames = List.wrap(Map.get(si, :sentence_signatures))
    slate = Map.get(si, :atl_slate) || %{}
    constituents = List.wrap(Map.get(slate, :constituents))

    question =
      Enum.find_value(frames, fn
        %{id: :wh_aux_topic_question, span: span, roles: roles} = frame ->
          atl =
            Enum.find(constituents, fn
              %{id: :wh_aux_topic_question, span: ^span, status: :resolved} -> true
              _ -> false
            end)

          if atl && is_map(roles), do: compose_question(frame, atl, frames, tokens), else: nil

        _ -> nil
      end)

    question || %{
      version: 1,
      sentence: sentence,
      definition: nil,
      kind: :unknown,
      status: :not_yet_interpreted,
      source: :insufficient_verified_sentence_structure
    }
  end

  def build(_), do: %{
    version: 1, sentence: nil, definition: nil, kind: :unknown,
    status: :not_yet_interpreted, source: :invalid_input
  }

  defp compose_question(
         %{span: {start, last}, roles: roles},
         atl, signatures, indexed
       )
       when is_integer(start) and is_integer(last) and is_map(roles) and
              is_map(atl) and is_map(indexed) do
    with %{wh: wh, auxiliary: auxiliary, subject: subject,
           predicate: predicate, preposition: prep} <- roles,
         true <- Enum.all?([wh, auxiliary, subject, predicate, prep], &is_integer/1),
         true <- last > prep and last - prep <= @max_topic_tokens,
         %{wh: _, auxiliary: _, subject: _, predicate: _, preposition: _} <- atl.roles,
         {:ok, topic} <- surface_range(indexed, prep + 1, last),
         {:ok, wh_text} <- surface_at(indexed, wh),
         {:ok, aux_text} <- surface_at(indexed, auxiliary),
         {:ok, subject_text} <- surface_at(indexed, subject),
         {:ok, predicate_text} <- surface_at(indexed, predicate),
         {:ok, prep_text} <- surface_at(indexed, prep) do

      possessive =
        Enum.find_value(signatures, fn
          %{id: :possessive_noun_phrase,
            span: {owner_start, owner_end},
            surface_possessor: owner,
            roles: %{possessor: pos_index}}
          when is_binary(owner) and owner_start > prep and
               owner_end <= last and pos_index == owner_start ->
            %{possessor: owner, token_index: pos_index,
              relation: :genitive_nominal_modifier,
              grounding_status: :unverified}

          _ -> nil
        end)

      negation =
        case Map.get(roles, :negation) do
          n when is_integer(n) -> case surface_at(indexed, n) do {:ok, text} -> text; _ -> nil end
          _ -> nil
        end

      pred_modifier =
        case Map.get(roles, :predicate_modifier) do
          n when is_integer(n) -> case surface_at(indexed, n) do {:ok, text} -> text; _ -> nil end
          _ -> nil
        end

      wh_modifier =
        case Map.get(roles, :wh_modifier) do
          n when is_integer(n) -> case surface_at(indexed, n) do {:ok, text} -> text; _ -> nil end
          _ -> nil
        end

      definition =
        definition(%{
          question: wh_text, auxiliary: aux_text, subject: subject_text,
          predicate: predicate_text, preposition: prep_text, topic: topic,
          negation: negation, predicate_modifier: pred_modifier,
          wh_modifier: wh_modifier
        })

      %{
        version: 1,
        sentence: original_sentence(indexed, start, last),
        definition: definition,
        kind: :wh_topic_question,
        status: :structurally_interpreted,
        speech_act: :request_information,
        roles: %{
          question_word: %{surface: wh_text, token_index: wh,
            sense_id: atl.roles.wh.sense_id},
          auxiliary: %{surface: aux_text, token_index: auxiliary,
            sense_id: atl.roles.auxiliary.sense_id},
          subject: %{surface: subject_text, token_index: subject,
            sense_id: atl.roles.subject.sense_id,
            reference: if(String.downcase(subject_text) == "you",
              do: :addressee, else: :unspecified)},
          predicate: %{surface: predicate_text, token_index: predicate,
            sense_id: atl.roles.predicate.sense_id},
          preposition: %{surface: prep_text, token_index: prep,
            sense_id: atl.roles.preposition.sense_id},
          topic: %{surface: topic, span: {prep + 1, last},
            possession: possessive, grounding_status: :not_fully_verified}
        },
        unresolved: if(possessive, do: [:topic_possessor_lexical_grounding], else: []),
        grounding: %{
          grammatical_roles: :atl_verified,
          exact_lexical_meanings: :not_verified,
          topic_facts: :not_checked
        },
        source: :sentence_signature_and_atl
      }
    else
      _ -> nil
    end
  end

  defp compose_question(_, _, _, _), do: nil

  defp definition(%{question: question, auxiliary: aux, subject: subject,
                     predicate: predicate, preposition: prep, topic: topic} = parts) do
    subject_low = String.downcase(subject)
    wh_modifier = Map.get(parts, :wh_modifier)
    negation = Map.get(parts, :negation)
    modifier = Map.get(parts, :predicate_modifier)
    negative_aux? = String.contains?(aux, "n't") or String.contains?(aux, "n’t")
    is_negative = is_binary(negation) or negative_aux?

    if String.downcase(question) == "what" and
         subject_low in ~w(i you we they) do
      # Put the auxiliary back only for polarity; ordinary do-support carries
      # no separate lexical proposition. Do not try to inflect 3rd-person verbs.
      clause =
        if is_negative do
          Enum.reject([subject, aux, negation, modifier, predicate, prep, topic],
            &is_nil/1)
        else
          Enum.reject([subject, modifier, predicate, prep, topic], &is_nil/1)
        end
        |> Enum.join(" ")

      prefix = if is_binary(wh_modifier), do: "#{wh_modifier} ", else: ""
      "The speaker asks #{prefix}what #{clause}."
    else
      # A technical gloss is safer than guessing English agreement, scope
      # or the meaning of a verb with a different syntactic subject.
      "An information question about #{topic}, with subject '#{subject}' " <>
        "and predicate '#{predicate}' (relation '#{prep}')."
    end
  end

  defp indexed_base_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback_index}, acc ->
      if is_map(token) and Map.get(token, :n, 1) == 1 do
        index = Map.get(token, :index, fallback_index)
        surface = Map.get(token, :phrase) || Map.get(token, :norm)

        if is_integer(index) and index >= 0 and is_binary(surface) do
          Map.put(acc, index, String.trim(surface))
        else
          acc
        end
      else
        acc
      end
    end)
  end

  defp indexed_base_tokens(_), do: %{}

  defp surface_at(indexed, index) do
    case Map.get(indexed, index) do
      s when is_binary(s) and s != "" -> {:ok, clean_word(s)}
      _ -> :error
    end
  end

  defp surface_range(indexed, first, last)
       when is_integer(first) and is_integer(last) and last >= first and
              last - first < @max_topic_tokens do
    results = for index <- first..last, do: surface_at(indexed, index)

    if Enum.all?(results, &match?({:ok, _}, &1)) do
      {:ok, results |> Enum.map(fn {:ok, s} -> s end) |> Enum.join(" ")}
    else
      :error
    end
  end

  defp surface_range(_, _, _), do: :error

  defp original_sentence(indexed, first, last) do
    first..last
    |> Enum.map(&Map.get(indexed, &1, ""))
    |> Enum.join(" ")
    |> String.trim()
  end

  defp clean_word(s) do
    s
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end
end
