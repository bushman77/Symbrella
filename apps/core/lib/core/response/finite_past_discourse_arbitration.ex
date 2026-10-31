defmodule Core.Response.FinitePastDiscourseArbitration do
  @moduledoc """
  Phase 3: explicit, default-off arbitration contract for past-tense
  speaker-description questions. Not connected to the production planner.

  If the same session has an admitted finite-past report about the requested
  actor-kind, later memory systems must NOT override a declined readout with
  owner-wide episodic guesses. Ambiguity is a clarification, not an answer.

  Without such scoped evidence, or unless explicitly opted in, existing memory
  handling is untouched. This module does not write anything, infer identity,
  call an LLM, or assert that an event occurred.
  """

  alias Core.Discourse.Ledger
  alias Core.Response.FinitePastDiscourseReadout
  alias Core.Semantic.FinitePastDiscourseQuestion

  @kind :reported_finite_past_locative_candidate

  @type decision ::
          {:answer, String.t(), map()}
          | {:clarify, String.t(), map()}
          | {:abstain, String.t(), map()}
          | :pass

  @spec decide(term(), term(), keyword()) :: decision()
  def decide(session, question, opts \\ [])

  def decide(session, question, opts)
      when is_binary(session) and is_binary(question) and is_list(opts) do
    with true <- Keyword.keyword?(opts),
         true <- Keyword.get(opts, :finite_past_response_arbitration?, false) == true,
         true <- byte_size(session) in 1..128,
         {:ok, actor, _role} <- FinitePastDiscourseQuestion.parse_question(question),
         true <- Enum.any?(Ledger.entries(session), &admitted_kind?(&1, actor)) do
      case FinitePastDiscourseReadout.read(session, question) do
        {:ok, text, %{status: :speaker_description_candidate,
                      factual_authority?: false,
                      event_occurrence_verified?: false,
                      referent_identity_verified?: false,
                      llm_called?: false} = meta}
            when is_binary(text) ->
          {:answer, text, Map.put(meta, :arbitration, :finite_past_report)}

        {:ambiguous, text, %{status: :referent_ambiguous,
                             factual_authority?: false,
                             referent_identity_verified?: false,
                             llm_called?: false} = meta}
            when is_binary(text) ->
          {:clarify, text, Map.put(meta, :arbitration, :finite_past_report)}

        _ ->
          # A relevant report exists but cannot safely be joined. This is a
          # *blocking* answer boundary, not permission to ask an older,
          # owner-wide Hippocampus or episodic source for a plausible reply.
          {:abstain,
           "I cannot safely connect that question to a specific earlier " <>
             "description in this conversation.",
           %{
             response_source: :symbolic_finite_past_report_arbitration,
             arbitration: :finite_past_report,
             status: :continuity_unresolved,
             verification: :conversation_context_only,
             event_occurrence_verified?: false,
             referent_identity_verified?: false,
             factual_authority?: false,
             llm_called?: false
           }}
      end
    else
      _ -> :pass
    end
  end

  def decide(_, _, _), do: :pass

  @doc """
  Evaluation-order contract for a future Memory.reply_for integration.
  A fallback is invoked *exactly once* only if arbitration returns :pass.
  It is never invoked after an answer, ambiguity, or continuity abstention.

  The fallback callback is a test/integration seam, not a source of semantic
  evidence. Caller must preserve the decision tag and its policy metadata.
  """
  @spec route(term(), term(), (() -> term()), keyword()) ::
          decision() | {:fallback, term()}
  def route(session, question, fallback, opts \\ [])

  def route(session, question, fallback, opts)
      when is_function(fallback, 0) do
    case decide(session, question, opts) do
      :pass -> {:fallback, fallback.()}
      result -> result
    end
  end

  def route(_, _, _, _), do: :pass

  defp admitted_kind?(%{kind: @kind, actor_kind: candidate}, actor)
       when candidate == actor, do: true
  defp admitted_kind?(_, _), do: false
end
