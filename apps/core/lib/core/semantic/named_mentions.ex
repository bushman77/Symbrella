defmodule Core.Semantic.NamedMentions do
  @moduledoc """
  Read-only recognition of possible single-token proper names.

  Requires original capitalization and a token-indexed,
  dictionary-attested proper-noun alternative.

  Does not replace LIFG/ATL selections, establish identity,
  resolve pronouns, or authorize memory writes.
  """

  alias Core.Semantic.Passage

  @version 1
  @capitalized ~r/\A\p{Lu}\p{L}+\z/u
  @closed_pos ~w(pronoun determiner conjunction auxiliary preposition)

  @spec observe(map()) :: {:ok, map()} | {:error, term()}
  def observe(%{} = si) do
    with {:ok, %{segments: segments}} <- Passage.project(si) do
      tokens = List.wrap(Map.get(si, :tokens))
      cells = List.wrap(Map.get(si, :active_cells))
      lifg = List.wrap(Map.get(si, :lifg_choices))
      atl = atl_winners(si)

      mentions =
        segments
        |> Enum.with_index(1)
        |> Enum.flat_map(fn {segment, sentence_index} ->
          {first, last} = segment.source_word_range

          tokens
          |> Enum.filter(fn token ->
            index = Map.get(token, :index)

            is_integer(index) and index >= first and
              index <= last and
              Map.get(token, :n) == 1 and
              Map.get(token, :kind) != :punct
          end)
          |> Enum.flat_map(fn token ->
            index = token.index
            surface = token.phrase

            witnesses = proper_witnesses(cells, index, surface)
            lifg_id = winner_id(lifg, index)
            atl_id = winner_id(atl, index)

            if witnesses != [] and
                 Regex.match?(@capitalized, surface) and
                 not closed_class?(lifg_id) do
              [
                %{
                  status: :candidate,
                  kind: :possible_proper_name,
                  surface: surface,
                  source_index: index,
                  source_span: token.span,
                  sentence_index: sentence_index,
                  sentence_position:
                    if(index == first,
                      do: :initial,
                      else: :internal
                    ),
                  dictionary_witnesses: witnesses,
                  lifg_winner: lifg_id,
                  atl_winner: atl_id,
                  referent: :unresolved,
                  factual_authority?: false
                }
              ]
            else
              []
            end
          end)
        end)

      {:ok,
       %{
         version: @version,
         status: :observed,
         mentions: mentions,
         factual_authority?: false
       }}
    end
  end

  def observe(_), do: {:error, :invalid_input}

  defp proper_witnesses(cells, index, surface) do
    word = String.downcase(surface)

    cells
    |> Enum.filter(fn cell ->
      case cell do
        %{
          id: id,
          pos: "proper_noun",
          norm: norm,
          token_index: ^index
        }
        when is_binary(id) and is_binary(norm) ->
          case String.split(id, "|", parts: 3) do
            [^word, "proper_noun", sense] ->
              sense != "" and String.downcase(norm) == word

            _ ->
              false
          end

        _ ->
          false
      end
    end)
    |> Enum.map(& &1.id)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp winner_id(rows, index) do
    case Enum.filter(rows, fn row ->
           is_map(row) and Map.get(row, :token_index) == index
         end) do
      [winner] ->
        Map.get(winner, :id) ||
          Map.get(winner, :chosen_id)

      _ ->
        nil
    end
  end

  defp atl_winners(%{atl_slate: %{winners: winners}})
       when is_list(winners),
       do: winners

  defp atl_winners(_), do: []

  defp closed_class?(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos in @closed_pos
      _ -> false
    end
  end

  defp closed_class?(_), do: false
end
