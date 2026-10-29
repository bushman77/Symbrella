defmodule Brain.ATL.UnresolvedGrammar do
  @moduledoc """
  Observe a pre-LIFG grammatical gap in ATL without treating the gap as a
  selected sense, a resolved constituent, or an event.

  LIFG supplies the unresolved-role hypothesis. Recompute the observation
  from the current token surfaces, signatures and dictionary cells before
  admitting it to the ATL *diagnostic* slate. Any real lexical winner at the
  unresolved slot cancels the observation. No BrainCells, choices, scores,
  constituents, or persistent ATL concept counters are modified.
  """

  alias Brain.LIFG.RoleCompetition

  @max_observations 4

  @spec attach(map(), map()) :: map()
  def attach(%{} = si, %{winners: winners} = slate) when is_list(winners) do
    declared =
      si
      |> Map.get(:role_competition, %{})
      |> case do
        %{source: :core_syntax_and_attested_dictionary_pos,
          unresolved_roles: %{} = roles} -> roles
        _ -> %{}
      end

    replayed = RoleCompetition.resolve(si).unresolved_roles

    observations =
      declared
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.take(@max_observations)
      |> Enum.flat_map(fn {index, entry} ->
        if Map.get(replayed, index) == entry and
             valid_entry?(index, entry) and
             not selected_sense_at?(winners, index) do
          [%{
            token_index: index,
            role: :predicate,
            status: :unresolved,
            source: :lifg_role_competition,
            construction: :progressive_lexical_gap_candidate,
            lexical_pos_verified?: false,
            exact_dictionary_sense_verified?: false,
            referent_identity_verified?: false,
            event_occurrence_verified?: false,
            factual_authority?: false
          }]
        else
          []
        end
      end)

    slate
    |> Map.put(:unresolved_grammar, observations)
    |> Map.put(:unresolved_grammar_count, length(observations))
  end

  def attach(_si, slate), do: slate

  defp valid_entry?(index, %{
         token_index: index,
         role: :predicate,
         status: :unresolved,
         frame: :progressive_lexical_gap_candidate,
         basis: :independently_witnessed_surrounding_roles,
         lexical_pos_verified?: false,
         exact_dictionary_sense_verified?: false,
         factual_authority?: false
       }) when is_integer(index) and index >= 0, do: true

  defp valid_entry?(_, _), do: false

  # A fallback is not a lexical sense, but a genuine dictionary winner
  # at this index is stronger evidence than the outstanding gap hypothesis.
  # Do not use a winning MWE overlapping the slot as a fabricated unigram.
  defp selected_sense_at?(winners, index) do
    Enum.any?(winners, fn
      %{token_index: ^index, id: id}
      when is_binary(id) ->
        String.contains?(id, "|") and
          not String.ends_with?(id, "|fallback") and
          not String.contains?(id, "|phrase|")

      _ -> false
    end)
  end
end
