defmodule Core.Semantic.TemporalSentence do
  @moduledoc """
  A narrowly witnessed two-clause temporal assertion.

  Both clauses must have explicitly named, closed-vocabulary noun subjects.
  Original contiguous base tokens, exact or independently scored LIFG/dictionary
  POS witnesses, present-participle morphology and the temporal connective are
  rechecked before applying TemporalRelation.

  This is an attribution of the *speaker's claim*, not proof of event
  occurrence, event-time chronology, or individual identity. Arbitrary
  pronouns, subordinate scope and ambiguous clauses are not accepted.
  """

  alias Core.Semantic.Composition.{Event, RoleWitness, TemporalRelation}

  @surface_length 15
  @locations ["fence", "tree", "table"]
  @actors ["bird", "cat", "dog"]
  @activities ["sitting", "sleeping"]
  @prepositions ["on", "in", "under", "beside", "above", "below"]
  @connectives ["before", "after"]
  @event_type {:arrow, :event, {:arrow, :event, :truth}}

  @spec build(map()) :: map() | nil
  def build(%{} = si) do
    sentence = Map.get(si, :sentence)

    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- is_binary(sentence) and byte_size(sentence) <= 180,
         false <- Regex.match?(~r/[?;:"“”\n\r]/u, sentence),
         {:ok, words} <- contiguous_words(si, sentence),
         ["the", actor1, "is", action1, prep1, "the", place1,
          connective, "the", actor2, "is", action2, prep2, "the", place2] <- words,
         true <- actor1 in @actors and actor2 in @actors,
         true <- action1 in @activities and action2 in @activities,
         true <- prep1 in @prepositions and prep2 in @prepositions,
         true <- place1 in @locations and place2 in @locations,
         true <- connective in @connectives,
         {:ok, witnesses} <- lexical_witnesses(si),
         true <- Enum.map(witnesses, & &1.word) == words,
         {:ok, event1} <- Event.compose(%{
           actor: actor1, predicate: action1, preposition: prep1, landmark: place1
         }),
         :ok <- Event.verify(event1),
         {:ok, event2} <- Event.compose(%{
           actor: actor2, predicate: action2, preposition: prep2, landmark: place2
         }),
         :ok <- Event.verify(event2),
         connective_kind <- if(connective == "before", do: :before, else: :after),
         {:ok, proof} <- TemporalRelation.compose(connective_kind, event1, event2),
         :ok <- TemporalRelation.verify(proof, connective_kind, event1, event2) do
      {earlier, later} =
        if connective_kind == :before,
          do: {event1.semantics, event2.semantics},
          else: {event2.semantics, event1.semantics}

      %{
        version: 1,
        kind: :explicit_temporal_report,
        status: :provisional,
        sentence: sentence,
        definition: nil,
        speech_act: :assertion,
        source: :revalidated_temporal_clause_composition,
        source_frame: :two_explicit_progressive_clauses,
        connective: connective_kind,
        surface_clause_order: :first_then_second,
        temporal_claim: %{
          relation: :before,
          earlier_event_property: earlier,
          later_event_property: later,
          basis: :explicit_user_temporal_connective,
          speaker_attributed?: true,
          external_chronology_verified?: false,
          event_occurrence_verified?: false,
          referent_identity_verified?: false
        },
        compositional_evidence: %{
          result: proof.result,
          type: @event_type,
          proof_steps: length(proof.steps),
          event_properties_verified?: true,
          temporal_operator_verified?: true,
          connective_witness_verified?: true,
          lexical_witnesses: witnesses,
          source_event_semantics: [event1.semantics, event2.semantics],
          factual_authority?: false
        },
        unresolved: [:event_occurrence, :actual_event_chronology,
          :referent_identity, :exact_dictionary_senses],
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end

  def build(_), do: nil

  @doc "Reject saved, stale or edited meanings by rebuilding current witnesses."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :sentence_meaning)

    if is_map(saved) and saved.kind == :explicit_temporal_report and
         saved.factual_authority? == false and saved == build(si) do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp contiguous_words(si, sentence) do
    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(fn
        %{index: i, phrase: p} = t when is_integer(i) and is_binary(p) ->
          Map.get(t, :n, 1) == 1 and
            Map.get(t, :kind) not in [:punct, "punct"]
        _ -> false
      end)
      |> Enum.sort_by(& &1.index)

    # The live tokenizer retains the sentence-final full stop in the final
    # base token (e.g. "tree."). Strip ONLY that full stop, ONLY at the
    # canonical final index. No internal punctuation is ever normalized away.
    words = Enum.map(tokens, fn token ->
      word = token.phrase |> String.downcase() |> String.trim()
      if token.index == @surface_length - 1,
        do: String.replace(word, ~r/\.\z/u, ""),
        else: word
    end)
    written =
      sentence
      |> String.downcase()
      |> String.trim()
      |> String.replace(~r/\.\z/u, "")
      |> String.split(~r/\s+/u, trim: true)

    if length(tokens) == @surface_length and
       Enum.map(tokens, & &1.index) == Enum.to_list(0..14) and
       words == written and
       Enum.all?(words, &Regex.match?(~r/\A\p{L}+\z/u, &1)) do
      {:ok, words}
    else
      :unsupported
    end
  end

  # Each role is backed by current LIFG and an exact dictionary row. Only
  # auxiliaries and progressive predicates may use RoleWitness's uniquely
  # scored, independently attested alternatives. Predicate alternatives must
  # satisfy the existing participle morphology guard. Other roles require
  # the selected witness, including the temporal connective.
  defp lexical_witnesses(si) do
    roles = [
      {0, :subject_determiner, ["determiner"]},
      {1, :subject_head, ["noun"]},
      {2, :auxiliary, ["verb", "auxiliary"]},
      {3, :predicate, ["verb"]},
      {4, :preposition, ["preposition"]},
      {5, :location_determiner, ["determiner"]},
      {6, :location_head, ["noun"]},
      {7, :temporal_connective, ["conjunction", "preposition"]},
      {8, :subject_determiner, ["determiner"]},
      {9, :subject_head, ["noun"]},
      {10, :auxiliary, ["verb", "auxiliary"]},
      {11, :predicate, ["verb"]},
      {12, :preposition, ["preposition"]},
      {13, :location_determiner, ["determiner"]},
      {14, :location_head, ["noun"]}
    ]

    Enum.reduce_while(roles, {:ok, []}, fn {index, role, pos}, {:ok, acc} ->
      case RoleWitness.resolve(si, role, index, pos) do
        {:ok, %{selection_basis: :selected} = witness} ->
          {:cont, {:ok, [witness | acc]}}
        {:ok, %{selection_basis: :construction_alternative,
                candidate_score: score} = witness}
            when role in [:auxiliary, :predicate] and
                 is_number(score) and score >= 0 ->
          {:cont, {:ok, [witness | acc]}}
        _ -> {:halt, :unsupported}
      end
    end)
    |> case do
      {:ok, witnesses} -> {:ok, Enum.reverse(witnesses)}
      other -> other
    end
  end
end
