defmodule Core.Semantic.EmbeddedClauseEvidence do
  @moduledoc """
  Read-only inspection of grammatical candidates *inside* a question's
  unresolved complement. A perfect-predicate signature does not establish
  its subject, reference, truth, or an interpreted embedded proposition.
  """
  @max_candidates 4
  @max_surface 80

  @spec inspect(map(), map()) :: [map()]
  def inspect(%{} = si, %{
        kind: :wh_topic_question,
        status: :structurally_interpreted,
        proposition: %{
          type: :information_request,
          epistemic_status: :question_not_assertion,
          content: %{complement: %{argument: %{
            type: :unparsed_topic, span: {first, last}
          }}}
        }
      })
      when is_integer(first) and is_integer(last) and first <= last do
    surfaces = indexed_surfaces(Map.get(si, :tokens, []))
    winners =
      si
      |> get_in([:atl_slate, :winners])
      |> List.wrap()

    si
    |> Map.get(:sentence_signatures, [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{id: :perfect_predicate, span: {a, b},
        roles: %{auxiliary: aux_index, verb: verb_index}} = signature
      when is_integer(a) and is_integer(b) and
             a == aux_index and b == verb_index and
             a >= first and b <= last and a < b ->
        with {:ok, aux} <- surface_at(surfaces, a),
             {:ok, verb} <- surface_at(surfaces, b) do
          [%{
            construction: :perfect_predicate,
            status: :syntactic_candidate_only,
            span: {a, b},
            auxiliary: %{surface: aux, token_index: a,
              candidate_sense_id: selected_id(winners, a)},
            predicate: %{surface: verb, token_index: b,
              candidate_sense_id: selected_id(winners, b)},
            grammatical_evidence: signature |> Map.get(:evidence, []) |> List.wrap()
              |> Enum.take(6),
            subject: :unresolved,
            embedded_clause_status: :not_interpreted,
            assertion_status: :not_asserted,
            verification: :external_unverified
          }]
        else
          _ -> []
        end

      _ ->
        []
    end)
    |> Enum.take(@max_candidates)
  end

  def inspect(_, _), do: []

  defp indexed_surfaces(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback}, acc ->
      if is_map(token) and Map.get(token, :n, 1) == 1 do
        index = Map.get(token, :index, fallback)
        surface = Map.get(token, :phrase) || Map.get(token, :norm)

        if is_integer(index) and is_binary(surface) do
          Map.put(acc, index, surface)
        else
          acc
        end
      else
        acc
      end
    end)
  end

  defp indexed_surfaces(_), do: %{}

  defp surface_at(surfaces, index) do
    case Map.get(surfaces, index) do
      word when is_binary(word) ->
        normalized =
          word
          |> String.trim()
          |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
          |> String.slice(0, @max_surface)

        if normalized == "", do: :error, else: {:ok, normalized}

      _ ->
        :error
    end
  end

  defp selected_id(winners, index) do
    case Enum.find(winners, fn winner ->
           is_map(winner) and Map.get(winner, :token_index) == index and
             is_binary(Map.get(winner, :id))
         end) do
      %{id: id} -> String.slice(id, 0, 128)
      _ -> nil
    end
  end
end
