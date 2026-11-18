defmodule Core.Semantic.Passage do
  @moduledoc """
  Read-only projection of canonical passage tokens into sentence-local
  contexts.

  Preserves the original token identities and source byte spans.
  Does not perform lexical selection, semantic inference, memory
  writes, or response generation.
  """

  alias Core.Text.Passage, as: TextPassage
  alias Core.Syntax.Signatures
  alias Core.Semantic.SentenceMeaning
  alias Core.Syntax.StructuralCompetition
  @max_segments 16

  @spec project(map()) ::
          {:ok, map()} | {:error, :invalid_alignment}

  def project(%{sentence: text, tokens: tokens} = si)
      when is_binary(text) and is_list(tokens) do
    with {:ok, aligned} <- TextPassage.align(si),
         true <- length(aligned) in 1..@max_segments do
      segments =
        Enum.map(aligned, fn segment ->
          {sentence_start, _} = segment.span

          {local_tokens, provenance} =
            segment.word_indices
            |> Enum.with_index()
            |> Enum.map(fn {source_index, local_index} ->
              token = Enum.at(tokens, source_index)

              {source_start, source_stop} = token.span

              local_span = {
                source_start - sentence_start,
                source_stop - sentence_start
              }

              local_token =
                token
                |> Map.put(:index, local_index)
                |> Map.put(:span, local_span)

              witness = %{
                local_index: local_index,
                source_index: source_index,
                local_span: local_span,
                source_span: token.span
              }

              {local_token, witness}
            end)
            |> Enum.unzip()

          %{
            text: segment.text,
            source_span: segment.span,
            source_word_range: segment.word_range,
            terminator: segment.terminator,
            tokens: local_tokens,
            token_provenance: provenance
          }
        end)

      {:ok,
       %{
         version: 1,
         status: :segmented_not_interpreted,
         original_text: text,
         segments: segments,
         factual_authority?: false
       }}
    else
      _ ->
        {:error, :invalid_alignment}
    end
  end

  def project(_), do: {:error, :invalid_alignment}

  @doc """
  Observe sentence-local meaning using only lexical evidence already
  selected for the original turn.

  This does not run LIFG again, create ATL winners, or write memory.
  Only independently revalidated provisional structural meanings are
  exposed. Other meanings remain unresolved.
  """
  @spec analyze(map()) ::
          {:ok, map()} | {:error, :invalid_alignment}

  def analyze(%{} = si) do
    with {:ok, projection} <- project(si) do
      choices = List.wrap(Map.get(si, :lifg_choices))
      cells = List.wrap(Map.get(si, :active_cells))

      atl_winners =
        case Map.get(si, :atl_slate) do
          %{winners: winners} when is_list(winners) -> winners
          _ -> []
        end

      segments =
        Enum.map(projection.segments, fn segment ->
          {first, last} = segment.source_word_range

          local_choices = localize(choices, first, last)
          local_winners = localize(atl_winners, first, last)

          local = %{
            sentence: segment.text,
            source: Map.get(si, :source),
            tokens: segment.tokens,
            active_cells: cells,
            lifg_choices: local_choices,
            atl_slate: %{
              winners: local_winners,
              constituents: []
            },
            episodic_candidates: [],
            trace: []
          }

          signatures = Signatures.detect(local)

          local =
            Map.put(local, :sentence_signatures, signatures)

          local =
            Map.put(
              local,
              :structural_competition,
              Brain.LIFG.StructuralCompetition.resolve(local)
            )

          meaning =
            case SentenceMeaning.build(local) do
              %{
                kind: :structural_hypothesis,
                status: :provisional,
                grounding: %{factual_authority?: false}
              } = candidate ->
                # Independently verify the structural preference.
                case StructuralCompetition.verified(local) do
                  {:ok, _} ->
                    Map.take(candidate, [
                      :kind,
                      :status,
                      :source_frame,
                      :roles,
                      :unresolved,
                      :participants,
                      :grounding
                    ])

                  _ ->
                    unresolved_meaning()
                end

              _ ->
                unresolved_meaning()
            end

          Map.merge(segment, %{
            sentence_meaning: meaning,
            lexical_evidence_source: :projected_parent_winners,
            factual_authority?: false
          })
        end)

      {:ok,
       %{
         version: 1,
         status: :read_only_semantic_observation,
         original_text: projection.original_text,
         segments: segments,
         factual_authority?: false
       }}
    else
      _ -> {:error, :invalid_alignment}
    end
  end

  def analyze(_), do: {:error, :invalid_alignment}

  defp localize(choices, first, last) do
    choices
    |> Enum.filter(fn
      %{token_index: index}
      when is_integer(index) and
             index >= first and index <= last ->
        true

      _ ->
        false
    end)
    |> Enum.map(fn choice ->
      Map.update!(choice, :token_index, &(&1 - first))
    end)
  end

  defp unresolved_meaning do
    %{
      kind: :unknown,
      status: :not_yet_interpreted
    }
  end
end
