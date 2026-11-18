defmodule Core.Semantic.PassageAntecedents do
  @moduledoc """
  Read-only antecedent candidate discovery within one passage.

  Combines independently observed named mentions with
  provisional sentence-level pronoun subjects.

  This module does not resolve identity, change lexical winners,
  create episodic events, or write discourse memory.
  """

  alias Core.Semantic.{NamedMentions, Passage}

  @version 1
  @supported_pronouns ~w(he she they)

  @spec observe(map()) :: {:ok, map()} | {:error, term()}
  def observe(%{} = si) do
    with {:ok, %{segments: segments}} <- Passage.analyze(si),
         {:ok, %{mentions: mentions}} <- NamedMentions.observe(si) do
      observations =
        segments
        |> Enum.with_index(1)
        |> Enum.flat_map(fn {segment, sentence_index} ->
          case pronoun_subject(segment, sentence_index) do
            {:ok, pronoun} ->
              candidates =
                mentions
                |> Enum.filter(&eligible?(&1, pronoun))
                |> Enum.map(&candidate/1)

              [
                %{
                  pronoun: pronoun,
                  status: candidate_status(candidates),
                  antecedent_candidates: candidates,
                  binding: :unresolved,
                  factual_authority?: false
                }
              ]

            :not_applicable ->
              []
          end
        end)

      {:ok,
       %{
         version: @version,
         status: :observed,
         observations: observations,
         factual_authority?: false
       }}
    end
  end

  def observe(_), do: {:error, :invalid_input}

  # Only inspect a pronoun subject from an independently
  # revalidated provisional structural interpretation.
  defp pronoun_subject(
         %{
           source_word_range: {first, last},
           tokens: tokens,
           token_provenance: provenance,
           factual_authority?: false,
           sentence_meaning: %{
             kind: :structural_hypothesis,
             status: :provisional,
             source_frame: :finite_past_svo_candidate,
             unresolved: unresolved,
             participants: %{
               actor: %{
                 surface: surface,
                 token_index: local_index,
                 referent: :unresolved
               }
             }
           }
         },
         sentence_index
       )
       when is_binary(surface) and is_integer(local_index) and
              is_list(tokens) and is_list(provenance) and
              is_list(unresolved) do
    token =
      Enum.find(tokens, fn token ->
        Map.get(token, :index) == local_index
      end)

    origin =
      Enum.find(provenance, fn entry ->
        entry.local_index == local_index
      end)

    surface_norm = String.downcase(surface)

    if :actor_referent in unresolved and
         surface_norm in @supported_pronouns and
         is_map(token) and is_map(origin) and
         String.downcase(Map.get(token, :phrase, "")) ==
           surface_norm and
         origin.source_index >= first and
         origin.source_index <= last do
      {:ok,
       %{
         surface: surface_norm,
         sentence_index: sentence_index,
         source_index: origin.source_index,
         source_span: origin.source_span
       }}
    else
      :not_applicable
    end
  end

  defp pronoun_subject(_, _), do: :not_applicable

  # An antecedent must precede the pronoun and have
  # independently observed proper-name dictionary evidence.
  defp eligible?(
         %{
           status: :candidate,
           kind: :possible_proper_name,
           sentence_index: sentence_index,
           source_index: source_index,
           dictionary_witnesses: [_ | _],
           factual_authority?: false
         },
         pronoun
       ) do
    sentence_index < pronoun.sentence_index and
      source_index < pronoun.source_index
  end

  defp eligible?(_, _), do: false

  defp candidate(mention) do
    %{
      surface: mention.surface,
      sentence_index: mention.sentence_index,
      source_index: mention.source_index,
      source_span: mention.source_span,
      dictionary_witnesses: mention.dictionary_witnesses,
      status: :possible_antecedent,
      factual_authority?: false
    }
  end

  defp candidate_status([]), do: :no_candidate
  defp candidate_status([_]), do: :candidate_only
  defp candidate_status(_), do: :ambiguous
end
