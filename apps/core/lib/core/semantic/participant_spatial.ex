defmodule Core.Semantic.ParticipantSpatial do
  @moduledoc """
  Evidence-gated binding of participant roles in a complete progressive
  locative clause. The resulting event stores roles, not merely source prose.

  A syntactic candidate is not proof of a verb sense. Require an attested
  present-participle *verb* dictionary cell even when LIFG's selected surface
  sense is adjectival. Exact LIFG + ATL agreement is required for the subject,
  preposition and locative noun; no cells or winners are rewritten.

  Limited to unhedged singular copular-progressive clauses with a simple
  prepositional location. Idiom-versus-literal meaning is not fully solved.
  """

  alias Core.Semantic.EpisodicEvents

  @evidence [:contiguous_surface, :attested_nominal_subject,
             :attested_present_participle, :attested_preposition,
             :attested_nominal_location]
  @positions ~w(on in under beside above below)
  @participle_tags ~w(tag:form-of tag:present tag:participle)

  @spec project(map(), String.t()) :: [map()]
  def project(si, sentence) when is_map(si) and is_binary(sentence) do
    with true <- admissible?(sentence),
         [sig] <- complete_signatures(si, sentence),
         %{subject_determiner: sd, subject_head: sh, auxiliary: aux,
           predicate: pred, preposition: prep,
           location_determiner: ld, location_head: lh} <- sig.roles,
         true <- [sd, sh, aux, pred, prep, ld, lh] == Enum.to_list(0..6),
         {:ok, tokens} <- tokens(si, sentence),
         {:ok, _} <- selected(si, sd, "determiner"),
         {:ok, _} <- selected(si, sh, "noun"),
         {:ok, _} <- selected(si, aux, "auxiliary"),
         {:ok, _} <- selected(si, prep, "preposition"),
         {:ok, _} <- selected(si, ld, "determiner"),
         {:ok, _} <- selected(si, lh, "noun"),
         true <- attested_participle?(si, Map.fetch!(tokens, pred)),
         "is" <- clean(Map.fetch!(tokens, aux)),
         position when position in @positions <- clean(Map.fetch!(tokens, prep)),
         predicate <- clean(Map.fetch!(tokens, pred)),
         true <- predicate != "",
         subject <- clean(Map.fetch!(tokens, sd) <> " " <> Map.fetch!(tokens, sh)),
         location <- clean(Map.fetch!(tokens, ld) <> " " <> Map.fetch!(tokens, lh)),
         {:ok, event} <- EpisodicEvents.normalize(%{
           entity: subject, relation: "spatial_state", value: location
         }) do
      participants = %{
        "actor" => subject,
        "predicate" => predicate,
        "aspect" => "progressive",
        "position" => position,
        "location" => location
      }

      [Map.merge(event, %{
        source_frame: :progressive_spatial,
        frame_confidence: 0.8,
        participants: participants
      })]
    else
      _ -> []
    end
  end

  def project(_, _), do: []

  defp complete_signatures(si, sentence) do
    si
    |> get(:sentence_signatures)
    |> List.wrap()
    |> Enum.filter(fn
      %{id: :progressive_locative_candidate, source: :core_syntax,
        span: {0, 6}, roles: roles, evidence: evidence}
      when is_map(roles) and is_list(evidence) ->
        Enum.all?(@evidence, &(&1 in evidence)) and
          match?({:ok, _}, tokens(si, sentence))
      _ -> false
    end)
  end

  defp tokens(si, sentence) do
    nodes =
      si
      |> get(:tokens)
      |> List.wrap()
      |> Enum.with_index()
      |> Enum.flat_map(fn {t, idx} ->
        index = get(t, :index) || idx
        if (get(t, :n) in [nil, 1]) and
             get(t, :kind) not in [:punct, "punct"] and
             is_integer(index) and is_binary(get(t, :phrase)),
          do: [{index, get(t, :phrase)}], else: []
      end)
      |> Enum.sort_by(&elem(&1, 0))

    if Enum.map(nodes, &elem(&1, 0)) == Enum.to_list(0..6) and
         clean(Enum.map_join(nodes, " ", &elem(&1, 1))) == clean(sentence),
      do: {:ok, Map.new(nodes)}, else: :error
  end

  defp selected(si, index, pos) do
    selections =
      si
      |> get(:lifg_choices)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :token_index) == index))

    winners =
      si
      |> get(:atl_slate)
      |> get(:winners)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :token_index) == index))

    with [selection] <- selections,
         [winner] <- winners,
         id when is_binary(id) <- get(selection, :chosen_id) || get(selection, :id),
         true <- String.contains?(id, "|" <> pos <> "|"),
         ^id <- get(winner, :id) || get(winner, :chosen_id) do
      {:ok, selection}
    else
      _ -> :error
    end
  end

  defp attested_participle?(si, surface) do
    word = clean(surface)

    si
    |> get(:active_cells)
    |> List.wrap()
    |> Enum.any?(fn cell ->
      id = get(cell, :id)
      tags = get(cell, :semantic_atoms)
      is_binary(id) and String.starts_with?(id, word <> "|verb|") and
        is_list(tags) and Enum.all?(@participle_tags, &(&1 in tags))
    end)
  end

  defp admissible?(text) do
    String.length(text) in 1..256 and
      not String.contains?(text, ["?", "\n", "\"", "“", "”", ";", ":"]) and
      not Regex.match?(
        ~r/\b(?:not|never|maybe|perhaps|possibly|might|could|would|should|if|unless|apparently|reportedly|allegedly|supposedly)\b/iu,
        text
      )
  end

  defp clean(text) when is_binary(text),
    do: text |> String.trim() |> String.trim_trailing(".") |> String.downcase()

  defp get(%{} = map, key),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil
end
