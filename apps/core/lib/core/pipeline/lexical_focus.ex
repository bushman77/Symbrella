defmodule Core.Pipeline.LexicalFocus do
  @moduledoc """
  Observational, reversible focus over the complete lexical candidate slate.

  All dictionary senses stay in :active_cells and remain available to LIFG.
  The focus is a *ranked attention hint*, never a new winner or hard filter.

  Independent supported grammatical roles prioritize an attested POS. Within
  each POS, Brain.LIFG.ContextFit supplies a non-factual lexical evidence
  score from the original sentence. A POS-diverse focus keeps alternative
  readings visible until downstream competition can adjudicate them.

  This stage never mutates :tokens, :active_cells, :sense_candidates, neural
  activation, database contents or the original source sentence.
  """

  alias Brain.LIFG.ContextFit
  alias Core.Pipeline.Trace

  @default_focus_size 8
  @max_focus_size 32
  @max_ranked_per_turn 256

  @doc "Attach an inspectable focus without removing any candidate from LIFG."
  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    if Keyword.get(opts, :lexical_focus?, true) == false do
      si
    else
      size = focus_size(Keyword.get(opts, :lexical_focus_size, @default_focus_size))
      cells = list_or_empty(Map.get(si, :active_cells))
      candidates =
        Enum.filter(cells, fn
          %{id: id, token_index: index} when is_binary(id) and
                                            byte_size(id) > 0 and
                                            is_integer(index) and index >= 0 -> true
          _ -> false
        end)

      # In the unlikely event of very large retrieval, don't make a second
      # expensive pass over it. The original candidate set is still intact.
      if length(candidates) > @max_ranked_per_turn do
        annotate(si, %{
          version: 1, mode: :observe_only, status: :skipped_large_slate,
          max_focus_size: size, total_candidates: length(candidates),
          positions: %{}, source: :full_ltm_candidates,
          sense_verified?: false
        })
      else
        tokens =
          si
          |> Map.get(:tokens, [])
          |> list_or_empty()
          |> Enum.filter(&is_map/1)
          |> Map.new(fn tok -> {Map.get(tok, :index), Map.get(tok, :phrase)} end)

        sentence = Map.get(si, :sentence)
        supported = supported_roles(Map.get(si, :role_competition))

        positions =
          candidates
          |> Enum.group_by(& &1.token_index)
          |> Map.new(fn {index, entries} ->
            focus = rank(entries, sentence, Map.get(tokens, index),
              Map.get(supported, index), size)

            {index, %{
              total_senses: length(Enum.uniq_by(entries, & &1.id)),
              focus: focus,
              deferred_senses: max(length(Enum.uniq_by(entries, & &1.id)) - length(focus), 0),
              supported_pos: Map.get(supported, index),
              status: if(Map.has_key?(supported, index), do: :grammatically_supported, else: :unresolved),
              verification: :candidate_hypotheses_only
            }}
          end)

        annotate(si, %{
          version: 1, mode: :observe_only, status: :ranked,
          max_focus_size: size, total_candidates: length(candidates),
          positions: positions, source: :full_ltm_candidates,
          sense_verified?: false
        })
      end
    end
  end

  def attach(other, _opts), do: other

  @doc """
  Recover every dictionary candidate for a token from the full slate.

  Deferred candidates are never deleted. This operation deliberately ignores
  the working-set limit so a later stage can reconsider an earlier alternative.
  """
  @spec expand(map(), non_neg_integer()) :: [map()]
  def expand(%{} = si, index) when is_integer(index) and index >= 0 do
    si
    |> Map.get(:active_cells, [])
    |> list_or_empty()
    |> Enum.filter(fn
      %{id: id, token_index: ^index} when is_binary(id) -> true
      _ -> false
    end)
    |> Enum.uniq_by(& &1.id)
  end

  def expand(_, _), do: []

  defp rank(rows, sentence, target, supported_pos, size) do
    ranked =
      rows
      |> Enum.uniq_by(& &1.id)
      |> Enum.map(fn row ->
        pos = normalize_pos(Map.get(row, :pos))
        fit =
          if is_binary(sentence) and is_binary(target) do
            ContextFit.score(sentence, target, row)
          else
            0.0
          end

        grammar? = is_binary(supported_pos) and pos == supported_pos
        evidence =
          (if grammar?, do: [:supported_dictionary_pos], else: []) ++
            (if fit > 0.0, do: [:dictionary_context_fit], else: [])

        %{
          id: row.id, pos: pos, context_fit: fit,
          grammatical_support?: grammar?,
          evidence: evidence
        }
      end)
      |> Enum.sort_by(fn candidate ->
        {if(candidate.grammatical_support?, do: 0, else: 1),
         -candidate.context_fit, candidate.id}
      end)

    # Reserve one slot per attested POS when possible. This is an attention
    # policy, not a claim that the different POS senses are equally probable.
    pos_representatives =
      ranked
      |> Enum.uniq_by(& &1.pos)
      |> Enum.take(size)

    chosen =
      (pos_representatives ++ ranked)
      |> Enum.uniq_by(& &1.id)
      |> Enum.take(size)

    # Return focus in evidence order, not order of POS enumeration.
    order = Map.new(Enum.with_index(ranked), fn {candidate, i} -> {candidate.id, i} end)
    Enum.sort_by(chosen, &Map.fetch!(order, &1.id))
  end

  defp supported_roles(%{
         source: :core_syntax_and_attested_dictionary_pos,
         roles: roles
       }) when is_map(roles) do
    roles
    |> Enum.flat_map(fn
      {index, %{status: :supported, pos: pos}}
      when is_integer(index) and index >= 0 and is_binary(pos) ->
        [{index, normalize_pos(pos)}]
      _ -> []
    end)
    |> Map.new()
  end

  defp supported_roles(_), do: %{}

  defp annotate(si, projection) do
    total_focus =
      projection.positions
      |> Map.values()
      |> Enum.reduce(0, fn entry, n -> n + length(entry.focus) end)

    si
    |> Map.put(:lexical_focus, projection)
    |> Trace.append(:lexical_focus,
      decision: projection.status,
      reason: :non_destructive_grammar_and_definition_focus,
      meta: %{
        full_candidates: projection.total_candidates,
        focused_candidates: total_focus,
        deferred_candidates: projection.total_candidates - total_focus,
        focused_positions: map_size(projection.positions),
        sense_verified?: false
      })
  end

  defp normalize_pos(value) when is_binary(value), do: String.downcase(String.trim(value))
  defp normalize_pos(value) when is_atom(value), do: Atom.to_string(value)
  defp normalize_pos(_), do: "unknown"

  defp focus_size(n) when is_integer(n) and n > 0, do: min(n, @max_focus_size)
  defp focus_size(_), do: @default_focus_size

  defp list_or_empty(values) when is_list(values), do: values
  defp list_or_empty(_), do: []
end
