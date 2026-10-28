defmodule Core.Semantic.DiscourseComposition do
  @moduledoc """
  Provisional cross-turn composition for a singular anaphoric spatial report.

  The previous turn supplies a revalidated *nominal description* from
  same-session WM, not a proven real-world referent. The current turn supplies
  independently witnessed grammatical roles and a typed progressive event
  property. Neither a grammatical preference nor a formal derivation proves
  that an event actually occurred.
  """

  alias Core.Discourse.References
  alias Core.Semantic.Composition.{Event, Nominal, Proof, RoleWitness}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  @construction :progressive_anaphoric_locative_candidate
  @evidence [
    :contiguous_surface,
    :attested_subject_pronoun,
    :attested_present_participle,
    :attested_preposition,
    :attested_nominal_location,
    :unresolved_anaphoric_subject
  ]

  @doc "Recompute only from independently checked current-turn and WM evidence."
  @spec build(map()) :: map() | nil
  def build(%{} = si) do
    sentence = Map.get(si, :sentence)

    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- is_binary(sentence) and byte_size(sentence) <= 160,
         true <- not Regex.match?(~r/[?;\n\r"“”]/u, sentence),
         %{} = reference <- Map.get(si, :discourse_reference),
         ^reference <- References.nominal_followup_reference(si),
         %{antecedent_head: noun, antecedent_modifiers: modifiers,
           referent_status: :candidate_same_session,
           factual_authority?: false} <- reference,
         [first_modifier, second_modifier] <- modifiers,
         {:ok, nominal} <- Nominal.compose(noun, [first_modifier, second_modifier]),
         :ok <- Proof.verify(nominal),
         {:ok, %{preferred: preferred}} <- StructuralCompetition.verified(si),
         %{construction: @construction, span: {0, 5}, roles: roles} <- preferred,
         %{subject: 0, auxiliary: 1, predicate: 2, preposition: 3,
           location_determiner: 4, location_head: 5} <- roles,
         [signature] <- matching_signatures(si, roles),
         {:ok, surface} <- contiguous_surface(si, sentence),
         ["it", "is", verb_surface, prep_surface, _, landmark_surface] <- surface,
         {:ok, subject} <- RoleWitness.resolve(si, :subject, 0, ["pronoun"]),
         "it" <- subject.word,
         {:ok, auxiliary} <- RoleWitness.resolve(si, :auxiliary, 1, ["verb", "auxiliary"]),
         "is" <- auxiliary.word,
         {:ok, predicate} <- RoleWitness.resolve(si, :predicate, 2, ["verb"]),
         ^verb_surface <- predicate.word,
         {:ok, preposition} <- RoleWitness.resolve(si, :preposition, 3, ["preposition"]),
         ^prep_surface <- preposition.word,
         {:ok, determiner} <- RoleWitness.resolve(si, :location_determiner, 4, ["determiner"]),
         true <- determiner.word in ["a", "an", "the"],
         {:ok, landmark} <- RoleWitness.resolve(si, :location_head, 5, ["noun"]),
         ^landmark_surface <- landmark.word,
         {:ok, event} <- Event.compose(%{
           predicate: predicate.word, actor: noun,
           preposition: preposition.word, landmark: landmark.word
         }),
         :ok <- Event.verify(event) do
      %{
        version: 1,
        kind: :anaphoric_spatial_report,
        status: :provisional,
        sentence: sentence,
        definition: nil,
        speech_act: :assertion,
        source: :revalidated_cross_turn_composition,
        source_frame: @construction,
        antecedent: %{
          head: noun,
          modifiers: modifiers,
          description: reference.antecedent,
          referent_status: :candidate_same_session,
          factual_authority?: false
        },
        relation: event.semantics.spatial.relation,
        event_semantics: event.semantics,
        compositional_evidence: %{
          nominal_property: %{result: nominal.result, type: nominal.result_type,
            formally_verified?: true},
          event_property: %{result: event.result, type: event.result_type,
            formally_verified?: true},
          typed_cross_turn_join_verified?: false,
          lexical_witnesses: [subject, auxiliary, predicate, preposition,
            determiner, landmark],
          source_signature: Map.take(signature, [:id, :span, :roles, :evidence])
        },
        grounding: %{
          nominal_description: :prior_same_session_composition,
          current_event_roles: :current_turn_scored_lexical_witnesses,
          external_facts: :not_checked
        },
        unresolved: [:referent_identity, :event_occurrence,
          :exact_dictionary_senses, :typed_cross_turn_join],
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end

  def build(_), do: nil

  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :sentence_meaning)

    if is_map(saved) and saved == build(si) and
         saved.kind == :anaphoric_spatial_report and
         saved.status == :provisional and
         saved.factual_authority? == false do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp matching_signatures(si, roles) do
    attached = List.wrap(Map.get(si, :sentence_signatures))
    detected = Signatures.detect(si)

    Enum.filter(attached, fn
      %{id: @construction, source: :core_syntax, span: {0, 5},
        roles: ^roles, evidence: evidence} = signature when is_list(evidence) ->
        Enum.all?(@evidence, &(&1 in evidence)) and signature in detected

      _ -> false
    end)
  end

  defp contiguous_surface(si, sentence) do
    base =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(fn
        %{index: index, phrase: word} = token
          when is_integer(index) and is_binary(word) ->
          Map.get(token, :n, 1) == 1 and
            Map.get(token, :kind) not in [:punct, "punct"]
        _ -> false
      end)
      |> Enum.sort_by(& &1.index)

    with [0, 1, 2, 3, 4, 5] <- Enum.map(base, & &1.index),
         words <- Enum.map(base, &normalize(&1.phrase)),
         true <- Enum.all?(words, &Regex.match?(~r/\A\p{L}[\p{L}\p{M}'-]*\z/u, &1)),
         ^words <- String.replace(sentence, ~r/[.!]\s*\z/u, "")
                   |> String.downcase() |> String.split(~r/\s+/u, trim: true) do
      {:ok, words}
    else
      _ -> :unsupported
    end
  end

  defp normalize(word) do
    word
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end
end
