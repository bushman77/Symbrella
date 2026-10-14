defmodule Core.LIFG.RelationalFeedback do
  @moduledoc """
  Wire one evidence-gated ATL -> LIFG feedback pass into Core.

  Only a same-POS candidate that was present in the original Stage1 complete
  probability distribution may replace a lexical winner. Canonical word
  choices are updated together and ATL is rebuilt immediately. This stage
  runs once, with no recursive reanalysis and no writes to BrainCells.
  """

  alias Brain.LIFG.RelationalFeedback, as: LIFG
  alias Core.Pipeline.Trace

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    report = LIFG.evaluate(si, opts)
    apply? = Keyword.get(opts, :apply?, true)

    revisions =
      if apply? do
        report.decisions
        |> Enum.filter(&(&1.revision? == true))
        |> Enum.filter(&can_revise?(si, &1))
        |> Enum.take(4)
      else
        []
      end

    {out, applied} =
      if revisions == [] do
        {si, []}
      else
        rebuilt =
          si
          |> Map.update!(:lifg_choices, &revise_choices(&1, revisions, si))
          |> Map.update!(:semantic_choices, &revise_choices(&1, revisions, si))
          |> Core.Brain.ATL.ingest([])
          |> Core.Brain.ATL.attach_lifg_pairs([])

        # Never publish a revised sense with a stale or missing ATL slate.
        if atl_consistent?(rebuilt, revisions) do
          {rebuilt, revisions}
        else
          {si, []}
        end
      end

    result =
      report
      |> Map.put(:revisions_rejected, length(revisions) - length(applied))
      |> Map.put(:applied_revisions, Enum.map(applied, fn change ->
        %{token_index: change.token_index,
          from: change.selected_id,
          to: change.proposed_id,
          reason: :atl_relation_dictionary_context}
      end))
      |> Map.put(:applied?, applied != [])
      |> Map.put(:mode, if(apply?, do: :bounded_reanalysis, else: :observe_only))

    out
    |> Map.put(:lifg_feedback, result)
    |> Trace.append(:lifg_relational_feedback,
      decision: if(applied == [], do: :preserved, else: :revised),
      reason: :atl_structural_context_for_lifg,
      meta: result)
  end

  def attach(si, _), do: si

  defp atl_consistent?(si, revisions) do
    winners =
      si
      |> Map.get(:atl_slate, %{})
      |> case do
        %{winners: list} when is_list(list) -> list
        _ -> []
      end

    Enum.all?(revisions, fn %{token_index: index, proposed_id: id} ->
      Enum.any?(winners, fn winner ->
        Map.get(winner, :token_index) == index and Map.get(winner, :id) == id
      end)
    end)
  end

  defp can_revise?(si, decision) do
    index = decision.token_index
    old = decision.selected_id
    new = decision.proposed_id
    semantic = Map.get(si, :semantic_choices, []) |> List.wrap()
    lexical = Map.get(si, :lifg_choices, []) |> List.wrap()

    Enum.any?(semantic, fn c ->
      Map.get(c, :token_index) == index and Map.get(c, :id) == old and
        Map.get(c, :kind) != :mwe
    end) and
      Enum.any?(lexical, fn c ->
        Map.get(c, :token_index) == index and Map.get(c, :id) == old
      end) and
      is_map(candidate_for(si, index, new)) and
      pos_of(old) == pos_of(new) and old != new
  end

  defp revise_choices(choices, revisions, si) do
    Enum.map(choices, fn choice ->
      case Enum.find(revisions, &(&1.token_index == Map.get(choice, :token_index) and
          &1.selected_id == Map.get(choice, :id))) do
        nil -> choice
        decision ->
          candidate = candidate_for(si, decision.token_index, decision.proposed_id)
          choice
          |> Map.put(:id, decision.proposed_id)
          |> then(fn revised ->
            if Map.has_key?(revised, :chosen_id) do
              Map.put(revised, :chosen_id, decision.proposed_id)
            else
              revised
            end
          end)
          |> Map.put(:score, Map.get(decision.posterior, decision.proposed_id))
          |> Map.put(:candidate_probs, decision.posterior)
          |> Map.put(:definition, get(candidate, :definition))
          |> Map.put(:example, get(candidate, :example))
          |> Map.put(:source, get(candidate, :source) || Map.get(choice, :source))
          |> Map.put(:relational_revision_from, decision.selected_id)
      end
    end)
  end

  defp candidate_for(si, index, id) do
    buckets = Map.get(si, :sense_candidates) || %{}
    (Map.get(buckets, index) || Map.get(buckets, Integer.to_string(index)) || [])
    |> List.wrap()
    |> Enum.find(&(get(&1, :id) == id))
  end

  defp pos_of(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp pos_of(_), do: nil

  defp get(map, key)
  defp get(%{} = map, key),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil
end
