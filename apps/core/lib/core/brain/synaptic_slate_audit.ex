defmodule Core.Brain.SynapticSlateAudit do
  @moduledoc """
  Read-only audit of verb sense coverage in actual post-LIFG candidate slates.

  Distinguishes a mixed-POS score bucket from true same-surface verb-sense
  competition. Reports an *attested base-form lexeme* only when the selected
  surface BrainCell has the morphology tags and dictionary gloss witness
  required by Core.Semantic.VerbLemma.

  A proven surface->base-form relation does NOT identify the sense of that
  base lexeme or authorize mapping a surface synaptic edge to a base sense.
  """

  alias Core.Semantic.VerbLemma

  @max_rows 12

  @spec inspect(map()) :: map()
  def inspect(%{} = si) do
    choices = List.wrap(Map.get(si, :lifg_choices))

    choices
    |> Enum.flat_map(fn
      %{token_index: index, id: id} = choice
          when is_integer(index) and is_binary(id) ->
        if pos(id) == "verb" do
          [summarize(si, index, choice)]
        else
          []
        end

      _ ->
        []
    end)
    |> Enum.take(4)
    |> then(fn
      [] -> %{status: :no_selected_verb, verbs: []}
      verbs -> %{status: :observed, verbs: verbs}
    end)
  end

  def inspect(_), do: %{status: :invalid_input, verbs: []}

  defp summarize(si, index, choice) do
    id = choice.id
    surface = id |> String.split("|", parts: 3) |> hd()

    candidates =
      case Map.get(si, :sense_candidates, %{}) do
        %{} = buckets ->
          case Map.get(buckets, index, Map.get(buckets, to_string(index))) do
            list when is_list(list) -> list
            _ -> []
          end

        _ ->
          []
      end

    candidate_ids =
      candidates
      |> Enum.flat_map(fn candidate ->
        case field(candidate, :id) do
          candidate_id when is_binary(candidate_id) -> [candidate_id]
          _ -> []
        end
      end)

    scores =
      case Map.get(choice, :candidate_probs) do
        %{} = m -> m
        _ -> %{}
      end

    scored_ids = Map.keys(scores)
    same_surface_verbs =
      scored_ids
      |> Enum.filter(fn candidate_id ->
        case String.split(candidate_id, "|", parts: 3) do
          [^surface, "verb", sense] -> sense != "" and sense != "fallback"
          _ -> false
        end
      end)
      |> Enum.sort()

    # VerbLemma.canonical returns the surface form in the absence of a
    # dictionary witness. Never interpret that fallback as a proven base.
    attested_base =
      case VerbLemma.canonical(si, choice) do
        {:ok, base} when base != surface -> base
        _ -> nil
      end

    score_slate_complete? =
      length(candidate_ids) == length(Enum.uniq(candidate_ids)) and
        MapSet.new(candidate_ids) == MapSet.new(scored_ids) and
        scored_ids != []

    %{
      predicate_index: index,
      selected_surface_id: id,
      surface: surface,
      selected_score: Map.get(scores, id),
      candidate_count: length(candidate_ids),
      scored_count: map_size(scores),
      score_slate_complete?: score_slate_complete?,
      scored_pos_counts:
        scored_ids |> Enum.map(&pos/1) |> Enum.frequencies(),
      same_surface_verb_sense_count: length(same_surface_verbs),
      same_surface_verb_ids: Enum.take(same_surface_verbs, @max_rows),
      scored_candidates:
        scores
        |> Enum.sort_by(fn {candidate_id, score} -> {-score, candidate_id} end)
        |> Enum.take(@max_rows)
        |> Enum.map(fn {candidate_id, score} ->
          %{id: candidate_id, pos: pos(candidate_id), score: score}
        end),
      attested_base_form: attested_base,
      base_form_relation_attested?: is_binary(attested_base),
      base_sense_identity_verified?: false,
      synaptic_mapping_to_base_senses?: false
    }
  end

  defp pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> "unknown"
    end
  end

  defp field(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp field(_, _), do: nil
end
