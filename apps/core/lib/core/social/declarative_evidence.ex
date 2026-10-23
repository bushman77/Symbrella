defmodule Core.Social.DeclarativeEvidence do
  @moduledoc """
  Read-only, bounded evidence for local transitive clauses in external text.

  Unlike the strict whole-sentence DeclarativeProposition interpreter, this
  identifies possible *subsentence* subject/verb/object structures in longer
  text. They are not attributed propositions, complete sentence meanings,
  assertions of an event, or independent factual verification.
  """

  @max_frames 16
  @max_candidates 4
  @max_rejections 6
  @max_surface 80
  @families [:finite_present_svo_candidate, :bounded_regular_past_transitive]

  @evidence %{
    finite_present_svo_candidate: [:contiguous_surface, :attested_nominal_subject,
      :attested_nominal_object, :attested_finite_present_verb],
    bounded_regular_past_transitive: [:contiguous_surface,
      :attested_nominal_subject, :attested_dictionary_verb,
      :bounded_past_surface, :attested_nominal_object]
  }

  @spec analyze(map()) :: map()
  def analyze(%{source: :colony_external} = si) do
    frames =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :id) in @families))
      |> Enum.take(@max_frames)

    {candidates, rejected} =
      Enum.reduce(frames, {[], []}, fn frame, {accepted, declined} ->
        case candidate(si, frame) do
          {:ok, result} -> {[result | accepted], declined}
          {:error, reason} ->
            {accepted, [%{frame: frame.id, span: Map.get(frame, :span), reason: reason} | declined]}
        end
      end)

    candidates = candidates |> Enum.reverse() |> Enum.take(@max_candidates)

    %{
      mode: :read_only,
      scope: :local_clause_not_whole_sentence,
      status: if(candidates == [], do: :no_supported_local_clause,
        else: :candidate_local_clauses),
      examined_frames: length(frames),
      candidates: candidates,
      abstentions: rejected |> Enum.reverse() |> Enum.take(@max_rejections),
      verification: :external_unverified
    }
  end

  def analyze(_), do: %{
    mode: :read_only, scope: :local_clause_not_whole_sentence,
    status: :unsupported_source, examined_frames: 0,
    candidates: [], abstentions: [], verification: :external_unverified
  }

  defp candidate(si, %{id: family, source: :core_syntax,
      span: {first, last}, roles: roles, evidence: evidence})
      when family in @families and is_integer(first) and is_integer(last) and
        first >= 0 and last >= first and last - first <= 7 and
        is_map(roles) and is_list(evidence) do
    required = Map.fetch!(@evidence, family)
    all_indices = Enum.to_list(first..last)

    with true <- Enum.all?(required, &(&1 in evidence)) || {:error, :missing_grammar_evidence},
         true <- Enum.all?(roles, fn {role, index} ->
           is_atom(role) and is_integer(index) and index >= first and index <= last
         end) || {:error, :invalid_grammar_indices},
         true <- Enum.all?([:subject_head, :predicate, :object_head], &Map.has_key?(roles, &1))
           || {:error, :missing_grammatical_roles},
         {:ok, token_map} <- bounded_token_map(si, all_indices),
         true <- source_surface_witnessed?(si, token_map, all_indices) ||
           {:error, :local_surface_not_in_original_source},
         {:ok, bound} <- bind_roles(si, roles, token_map),
         true <- complete_local_surface?(token_map, all_indices) || {:error, :incomplete_local_span} do
      subject = Map.fetch!(bound, :subject_head)
      predicate = Map.fetch!(bound, :predicate)
      object = Map.fetch!(bound, :object_head)

      {:ok, %{
        type: :local_transitive_clause_candidate,
        status: :grammatically_and_lexically_witnessed_not_interpreted,
        source_frame: family,
        span: {first, last},
        surface: all_indices |> Enum.map(&Map.fetch!(token_map, &1)) |> Enum.join(" "),
        subject: subject,
        predicate: predicate,
        object: object,
        tense_hypothesis: if(family == :bounded_regular_past_transitive,
          do: :past, else: :finite_present),
        polarity: :not_composed,
        modality: :not_interpreted,
        clause_attachment: :not_established,
        author_attribution: :external_source_only,
        assertion_status: :not_asserted,
        statement_truth: :not_evaluated,
        verification: :external_unverified
      }}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :insufficient_clause_evidence}
    end
  end

  defp candidate(_, _), do: {:error, :unrecognized_or_unattested_signature}

  defp bounded_token_map(si, indices) do
    wanted = MapSet.new(indices)

    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.flat_map(fn
        %{index: index, phrase: surface} = token
        when is_integer(index) and is_binary(surface) ->
          if MapSet.member?(wanted, index) and Map.get(token, :n, 1) == 1 and
               Map.get(token, :kind, :word) not in [:punct, "punct"] do
            cleaned = surface |> String.trim() |>
              String.replace(~r/^\\p{P}+|\\p{P}+$/u, "") |> String.slice(0, @max_surface)
            if cleaned == "", do: [], else: [{index, cleaned}]
          else
            []
          end

        _ -> []
      end)

    map = Map.new(tokens)

    if length(tokens) == length(indices) and map_size(map) == length(indices) do
      {:ok, map}
    else
      {:error, :noncontiguous_or_missing_lexical_tokens}
    end
  end

  defp complete_local_surface?(token_map, indices) do
    Enum.all?(indices, &(is_binary(Map.get(token_map, &1)) and
      Map.get(token_map, &1) != ""))
  end

  defp source_surface_witnessed?(si, token_map, indices) do
    case Map.get(si, :sentence) do
      sentence when is_binary(sentence) and byte_size(sentence) <= 2400 ->
        words =
          indices
          |> Enum.map(&Map.fetch!(token_map, &1))
          |> Enum.map(&Regex.escape/1)

        expression =
          "(?<![\\p{L}\\p{N}])" <> Enum.join(words, ~S(\s+)) <>
            "(?![\\p{L}\\p{N}])"

        Regex.match?(Regex.compile!(expression, "iu"), sentence)

      _ -> false
    end
  end

  defp bind_roles(si, roles, token_map) do
    roles
    |> Enum.sort_by(fn {key, _} -> Atom.to_string(key) end)
    |> Enum.reduce_while({:ok, %{}}, fn {role, index}, {:ok, collected} ->
      expected = expected_pos(role)
      case selected_role(si, index, expected, token_map) do
        {:ok, bound} -> {:cont, {:ok, Map.put(collected, role, bound)}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp expected_pos(:subject_head), do: ["noun", "proper_noun", "pronoun"]
  defp expected_pos(:object_head), do: ["noun", "proper_noun"]
  defp expected_pos(:predicate), do: ["verb"]
  defp expected_pos(:subject_determiner), do: ["determiner"]
  defp expected_pos(:object_determiner), do: ["determiner"]
  defp expected_pos(_), do: []

  defp selected_role(si, index, expected, token_map) do
    winners = get_in(si, [:atl_slate, :winners]) |> List.wrap()
    matches = Enum.filter(winners, &(is_map(&1) and Map.get(&1, :token_index) == index))

    case matches do
      [%{id: id} = winner] when is_binary(id) ->
        pos = selected_pos(id)
        cells = Map.get(si, :active_cells, []) |> List.wrap()

        cond do
          pos not in expected -> {:error, :selected_pos_incompatible}
          Map.get(winner, :kind) == :mwe or
            Map.get(winner, :selection_scope) == :lexical_mwe ->
              {:error, :lexical_mwe_cannot_bind_base_role}
          not Enum.any?(cells, fn cell ->
            is_map(cell) and Map.get(cell, :token_index) == index and
              Map.get(cell, :id) == id and Map.get(cell, :pos) == pos
          end) -> {:error, :selected_sense_not_attested_at_token}
          true ->
            {:ok, %{
              surface: Map.fetch!(token_map, index), token_index: index,
              selected_sense_id: String.slice(id, 0, 128),
              selected_pos: pos, grounding: :selected_and_dictionary_attested_only
            }}
        end

      _ -> {:error, :missing_or_ambiguous_lexical_selection}
    end
  end

  defp selected_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_lemma, pos, sense] when sense != "" and sense != "fallback" -> pos
      _ -> nil
    end
  end

  defp selected_pos(_), do: nil
end
