defmodule Core.Semantic.RelationalEvidence do
  @moduledoc """
  Relational Evidence V1: read-only sentence-conditioned dictionary diagnostics.

  Consumes independently produced Core.Syntax.Signatures for complete finite
  subject-predicate-object clauses. Validates each signature against original
  indexed tokens and exact-surface dictionary POS witnesses, then requests a
  LexicalBridge report between the two grammatical arguments.

  A gloss overlap never proves a semantic relation. A predicate is an attested
  grammatical link, not a verified verb meaning. Alternative senses, weak
  overlaps and absent bridges remain unresolved. PP attachments and incomplete
  frames deliberately abstain in V1.

  Never selects senses, updates LIFG, trains, persists, or modifies the SI.
  """

  alias Core.Pipeline.LexicalBridge
  alias Core.Semantic.PredicateLemmaEvidence

  @max_tokens 64
  @max_cells 4096
  @max_frames 8

  @supported %{
    finite_present_svo_candidate: [
      :contiguous_surface, :attested_nominal_subject,
      :attested_nominal_object, :attested_finite_present_verb
    ],
    finite_past_svo_candidate: [
      :contiguous_surface, :attested_nominal_subject,
      :attested_nominal_object, :attested_simple_past_verb
    ],
    bounded_regular_past_transitive: [
      :contiguous_surface, :attested_nominal_subject,
      :attested_dictionary_verb, :bounded_past_surface,
      :attested_nominal_object
    ]
  }

  @doc """
  Audit an SI with already-attested sentence signatures and dictionary cells.
  Option :lookup passes a checked dictionary callback to LexicalBridge.
  Option :via allows exactly two explicit intermediate lemmas for experiments;
  intermediate lemmas are never invented by V1.
  """
  @spec audit(map(), keyword()) :: map()
  def audit(si, opts \\ [])

  def audit(%{} = si, opts) when is_list(opts) do
    tokens = base_tokens(Map.get(si, :tokens))
    cells = Map.get(si, :active_cells)
    signatures = Map.get(si, :sentence_signatures)
    via = Keyword.get(opts, :via, [])

    cond do
      not is_list(cells) or not is_list(signatures) ->
        base(:missing_syntax_or_dictionary_evidence)

      tokens == [] or length(tokens) > @max_tokens or
          length(cells) > @max_cells or length(signatures) > 128 ->
        base(:evidence_bounds_exceeded)

      not valid_tokens?(tokens) ->
        base(:invalid_token_alignment)

      not is_list(via) or length(via) not in [0, 2] ->
        base(:invalid_intermediates)

      true ->
        eligible =
          signatures
          |> Enum.filter(&supported_signature?/1)
          |> Enum.uniq_by(&{&1.id, &1.span, &1.roles})
          |> Enum.take(@max_frames + 1)

        if length(eligible) > @max_frames do
          base(:too_many_supported_frames)
        else
          reports =
            eligible
            |> Enum.flat_map(fn signature ->
              case validate_frame(signature, tokens, cells) do
                {:ok, nouns, predicate} ->
                  [%{
                    signature_id: signature.id,
                    span: signature.span,
                    grammatical_role_source: :core_syntax,
                    grammatical_evidence: signature.evidence,
                    subject: nouns.subject,
                    predicate: predicate,
                    object: nouns.object
                  }]

                :unsupported -> []
              end
            end)
            |> Enum.map(fn frame ->
              bridge_opts = Keyword.take(opts, [:lookup]) ++ [via: via]
              bridge = LexicalBridge.audit(frame.subject.word, frame.object.word, bridge_opts)
              frame
              |> Map.put(:dictionary_bridge, bridge_projection(bridge))
              |> Map.put(:predicate_lemma_evidence,
                PredicateLemmaEvidence.audit(frame.predicate, cells, opts))
            end)

          %{
            version: 1, mode: :observation_only,
            status: report_status(reports),
            reason: if(reports == [], do: :no_attested_complete_clause, else: :candidate_only),
            evaluated_frames: length(reports),
            frames: reports,
            intended_sense_verified?: false,
            predicate_meaning_verified?: false,
            semantic_relation_verified?: false,
            modified_lifg?: false,
            response_changed?: false
          }
        end
    end
  end

  def audit(_, _), do: base(:invalid_input)

  defp report_status([]), do: :unresolved
  defp report_status(reports) do
    cond do
      Enum.any?(reports, fn frame ->
        frame.dictionary_bridge.status == :lookup_error or
          frame.predicate_lemma_evidence.status == :lookup_error
      end) ->
        :dictionary_lookup_error
      Enum.any?(reports, &(&1.dictionary_bridge.candidate_count > 0)) ->
        :candidate_relational_evidence
      true -> :no_dictionary_bridge
    end
  end

  defp bridge_projection({:ok, report}) do
    %{
      status: report.status,
      candidate_count: report.candidate_count,
      candidates: report.candidates,
      truncated?: report.truncated?,
      evidence_quality_counts: report.evidence_quality_counts,
      category_evidence_counts: report.category_evidence_counts,
      relevance_verified?: false
    }
  end

  defp bridge_projection({:error, reason}) do
    %{status: :lookup_error, reason: reason, candidate_count: 0,
      candidates: [], relevance_verified?: false}
  end

  defp supported_signature?(%{id: id, source: :core_syntax,
        span: {first, last}, roles: roles, evidence: evidence})
       when is_atom(id) and is_integer(first) and first >= 0 and
            is_integer(last) and last >= first and is_map(roles) and
            is_list(evidence) do
    case Map.fetch(@supported, id) do
      {:ok, required} ->
        Enum.all?(required, &(&1 in evidence)) and
          Enum.all?(evidence, &is_atom/1) and
          is_integer(Map.get(roles, :subject_head)) and
          is_integer(Map.get(roles, :predicate)) and
          is_integer(Map.get(roles, :object_head))
      :error -> false
    end
  end

  defp supported_signature?(_), do: false

  defp validate_frame(%{roles: roles, span: {first, last}}, tokens, cells) do
    subject = Map.get(roles, :subject_head)
    predicate = Map.get(roles, :predicate)
    object = Map.get(roles, :object_head)
    token_map = Map.new(tokens, &{&1.index, &1.word})

    with true <- first <= subject and subject < predicate and
                   predicate < object and object <= last,
         true <- Enum.all?(first..last, &Map.has_key?(token_map, &1)),
         word_s when is_binary(word_s) <- Map.get(token_map, subject),
         word_p when is_binary(word_p) <- Map.get(token_map, predicate),
         word_o when is_binary(word_o) <- Map.get(token_map, object),
         [_ | _] = subject_ids <- attested_ids(cells, subject, word_s, "noun"),
         [_ | _] = verb_ids <- attested_ids(cells, predicate, word_p, "verb"),
         [_ | _] = object_ids <- attested_ids(cells, object, word_o, "noun") do
      {:ok,
        %{
          subject: %{token_index: subject, word: word_s, attested_noun_ids: subject_ids},
          object: %{token_index: object, word: word_o, attested_noun_ids: object_ids}
        },
        %{token_index: predicate, word: word_p, attested_verb_ids: verb_ids,
          semantic_role: :grammatical_predicate_only}}
    else
      _ -> :unsupported
    end
  end

  defp attested_ids(cells, index, word, pos) do
    cells
    |> Enum.flat_map(fn
      %{token_index: ^index, id: id, pos: ^pos} = row when is_binary(id) ->
        if normalize(Map.get(row, :norm)) == word and
             String.starts_with?(id, word <> "|" <> pos <> "|"),
          do: [id], else: []
      _ -> []
    end)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp base_tokens(tokens) when is_list(tokens) do
    Enum.flat_map(tokens, fn
      %{index: index, phrase: phrase} = token
          when is_integer(index) and index >= 0 and is_binary(phrase) ->
        if Map.get(token, :n, 1) == 1,
          do: [%{index: index, word: normalize(phrase)}], else: []
      _ -> []
    end)
  end
  defp base_tokens(_), do: []

  defp valid_tokens?(tokens) do
    indices = Enum.map(tokens, & &1.index)
    Enum.uniq(indices) == indices and
      Enum.sort(indices) == Enum.to_list(0..(length(indices) - 1)) and
      Enum.all?(tokens, fn t ->
        t.word != "" and
          Regex.match?(~r/\A[\p{L}]+(?:[-'’][\p{L}]+)*\z/u, t.word)
      end)
  end

  defp normalize(value) when is_binary(value),
    do: value |> String.trim() |> String.downcase()
  defp normalize(_), do: ""

  defp base(reason) do
    %{
      version: 1, mode: :observation_only, status: :unresolved,
      reason: reason, evaluated_frames: 0, frames: [],
      intended_sense_verified?: false, predicate_meaning_verified?: false,
      semantic_relation_verified?: false, modified_lifg?: false,
      response_changed?: false
    }
  end
end
