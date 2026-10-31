defmodule Core.Response.FinitePastDiscourseReadout do
  @moduledoc """
  Phase 3: narrow, read-only surface realization of a *reported* finite-past
  discourse description. This is NOT wired to Core.Response.plan/2.

  The wording describes an earlier user sentence. It never asserts that an
  event occurred or that the noun kind denotes a known individual.

  Ambiguity is an explicit result, rather than a silent fallback to a
  different (possibly stale) memory source. Live planner integration must
  preserve that behavior before this is enabled for chat.
  """

  alias Core.Semantic.FinitePastDiscourseQuestion

  @actor_words %{bird: "bird", cat: "cat", dog: "dog"}
  @action_words %{sit: "sitting", sleep: "sleeping"}
  @relation_words %{on: "on", in: "in", under: "under",
    beside: "beside", above: "above", below: "below"}
  @landmark_words %{fence: "fence", tree: "tree", table: "table"}

  @type readout :: {:ok, String.t(), map()}
                 | {:ambiguous, String.t(), map()}
                 | :not_applicable

  @spec read(term(), term()) :: readout()
  def read(session, question) do
    case FinitePastDiscourseQuestion.project(session, question) do
      {:ok, %{status: :speaker_description_candidate,
              actor_kind: actor,
              description: description,
              source_turn_seq: seq,
              source_turn_sha256: digest,
              typed_proof_sha256: proof_digest,
              verification: :conversation_context_only,
              event_occurrence_verified?: false,
              referent_identity_verified?: false,
              factual_authority?: false}}
          when is_integer(seq) and seq > 0 and
               is_binary(digest) and is_binary(proof_digest) ->
        with {:ok, actor_word} <- word(@actor_words, actor),
             {:ok, answer} <- describe(description, actor_word) do
          {:ok, answer, %{
            response_source: :symbolic_finite_past_report_readout,
            status: :speaker_description_candidate,
            source_turn_seq: seq,
            source_turn_sha256: digest,
            typed_proof_sha256: proof_digest,
            verification: :conversation_context_only,
            event_occurrence_verified?: false,
            referent_identity_verified?: false,
            factual_authority?: false,
            llm_called?: false
          }}
        else
          _ -> :not_applicable
        end

      {:ambiguous, sources} when is_list(sources) and length(sources) >= 2 ->
        # Never select the latest source merely because it is most recent.
        case FinitePastDiscourseQuestion.parse_question(question) do
          {:ok, actor, _role} ->
            with {:ok, actor_word} <- word(@actor_words, actor),
                 true <- Enum.all?(sources, &ambiguous_source?(&1, actor)) do
              {:ambiguous,
               "There are multiple descriptions of a #{actor_word} in this " <>
                 "conversation. I cannot establish which one you mean.",
               %{
                 response_source: :symbolic_finite_past_report_readout,
                 status: :referent_ambiguous,
                 source_turn_sequences: Enum.map(sources, & &1.source_turn_seq),
                 verification: :conversation_context_only,
                 event_occurrence_verified?: false,
                 referent_identity_verified?: false,
                 factual_authority?: false,
                 llm_called?: false
               }}
            else
              _ -> :not_applicable
            end

          _ -> :not_applicable
        end

      _ -> :not_applicable
    end
  end

  defp describe(%{kind: :reported_predicate, lemma: lemma, tense: :past},
                actor_word) do
    with {:ok, action} <- word(@action_words, lemma) do
      {:ok, "Your earlier sentence described a #{actor_word} as #{action}. " <>
        "That is a description, not an independently verified event."}
    end
  end

  defp describe(%{kind: :reported_location,
                  relation: relation, landmark_kind: landmark, tense: :past},
                actor_word) do
    with {:ok, rel} <- word(@relation_words, relation),
         {:ok, place} <- word(@landmark_words, landmark) do
      {:ok, "Your earlier sentence described a #{actor_word} as being " <>
        "#{rel} a #{place}. That is a description, not an independently " <>
        "verified event."}
    end
  end

  defp describe(_, _), do: :not_applicable

  defp word(dictionary, key) do
    case Map.fetch(dictionary, key) do
      {:ok, value} -> {:ok, value}
      _ -> :not_applicable
    end
  end

  defp ambiguous_source?(%{
         actor_kind: actor,
         source_turn_seq: seq,
         source_turn_sha256: digest,
         factual_authority?: false,
         referent_identity_verified?: false
       }, actor)
       when is_integer(seq) and seq > 0 and is_binary(digest), do: true

  defp ambiguous_source?(_, _), do: false
end
