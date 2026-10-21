defmodule Core.Syntax.ArgumentEvidence do
  @moduledoc """
  Read-only observations of recipient and direct-object slots.

  Consumes an independently supported predicate POS and only local, bounded
  determiner/pronoun structure. Unknown CamelCase heads can be surface
  constituents, but they never acquire a dictionary sense or semantic type.
  These are role hypotheses, not facts or permission to update synapses.
  """
  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @recipients MapSet.new(~w(me you him her us them))
  @max_tokens 64

  @spec detect(map(), map(), map()) :: [map()]
  def detect(%{} = si, %{roles: roles}, %{unresolved_tokens: unresolved})
      when is_map(roles) and is_list(unresolved) do
    tokens = List.wrap(Map.get(si, :tokens, []))
    cells = List.wrap(Map.get(si, :active_cells, []))
    base_tokens = Enum.filter(tokens, &(is_map(&1) and Map.get(&1, :n, 1) == 1))
    words = Map.new(base_tokens, fn t ->
      {Map.get(t, :index), normalize(Map.get(t, :phrase))}
    end)
    surfaces = Map.new(base_tokens, fn t ->
      {Map.get(t, :index), original_surface(Map.get(t, :phrase))}
    end)
    unresolved_indices = MapSet.new(Enum.map(unresolved, & &1.token_index))

    if map_size(words) > @max_tokens do
      []
    else
      roles
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.flat_map(fn
        {predicate, %{status: :supported, role: :predicate, pos: "verb"}}
        when is_integer(predicate) ->
          recipient_idx = predicate + 1
          recipient? =
            MapSet.member?(@recipients, Map.get(words, recipient_idx)) and
              witnessed?(cells, Map.get(words, recipient_idx), "pronoun")
          det = if recipient?, do: predicate + 2, else: predicate + 1

          if MapSet.member?(@determiners, Map.get(words, det)) and
               witnessed?(cells, Map.get(words, det), "determiner") do
            head =
              if witnessed?(cells, Map.get(words, det + 1), "adjective"),
                do: det + 2, else: det + 1

            known_head? = witnessed?(cells, Map.get(words, head), "noun")
            unresolved_head? = MapSet.member?(unresolved_indices, head)

            if (known_head? or unresolved_head?) and head < map_size(words) and
                 contiguous?(words, det, head) do
              [%{status: :candidate, source: :original_syntax_and_dictionary_pos,
                construction: if(recipient?, do: :recipient_object, else: :direct_object),
                predicate: predicate,
                predicate_surface: Map.get(words, predicate),
                recipient: if(recipient?, do: recipient_idx, else: nil),
                object_determiner: det,
                object_modifier: if(head == det + 2, do: det + 1, else: nil),
                object_head: head,
                object_surface: Map.get(surfaces, head),
                object_norm: Map.get(words, head),
                object_status: if(known_head?, do: :dictionary_noun_witness,
                  else: :lexically_unresolved_code_head),
                meaning_verified?: false, event_verified?: false,
                evidence: [:supported_predicate_role, :bounded_argument_order,
                  :determiner_witness, :nominal_head_surface] ++
                  if(recipient?, do: [:pronoun_recipient_witness], else: [])}]
            else
              []
            end
          else
            []
          end

        _ -> []
      end)
    end
  end

  def detect(_, _, _), do: []

  defp contiguous?(words, det, head) do
    Enum.all?(det..head, &(is_binary(Map.get(words, &1)) and Map.get(words, &1) != ""))
  end

  defp witnessed?(cells, word, pos) when is_binary(word) do
    Enum.any?(cells, fn
      %{id: id} = cell when is_binary(id) ->
        normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == word and
          normalize(Map.get(cell, :pos)) == pos and
          String.starts_with?(id, word <> "|" <> pos <> "|")
      _ -> false
    end)
  end

  defp witnessed?(_, _, _), do: false

  defp original_surface(word) when is_binary(word) do
    String.replace(word, ~r/[?!.;,]+\z/u, "")
  end

  defp original_surface(_), do: nil

  defp normalize(word) when is_binary(word) do
    word |> original_surface() |> String.downcase()
  end

  defp normalize(_), do: nil
end
