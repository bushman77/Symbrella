defmodule Core.Response.FinitePastResponseGate do
  @moduledoc """
  Phase 3 opt-in integration between response attachment and the existing
  response planner. This function runs *before* legacy memory planning.
  A qualifying finite-past answer, ambiguity or continuity abstention is
  terminal: older episodic memory is never queried as a fallback.

  The default route is unchanged. The flag is per-call and is not inferred
  from the presence of an admitted record.
  """

  alias Core.Response.FinitePastDiscourseArbitration, as: Arbitration
  alias Core.Response.SideEffects

  @doc """
  The LiveView's independent memory fast path must defer to Core whenever
  opted-in arbitration has a terminal answer, ambiguity or abstention.
  Merely finding a past-tense question is insufficient: the decision must
  be backed by relevant same-session admitted evidence.
  """
  @spec skip_legacy_fast_path?(term(), term(), boolean()) :: boolean()
  def skip_legacy_fast_path?(session, question, true) do
    Arbitration.decide(session, question,
      finite_past_response_arbitration?: true) != :pass
  end

  def skip_legacy_fast_path?(_, _, _), do: false

  @spec route(map(), keyword(), (() -> term())) :: term()
  def route(si, opts, original_plan)
      when is_map(si) and is_list(opts) and is_function(original_plan, 0) do
    case decision(si, opts) do
      :pass -> original_plan.()
      {outcome, text, %{factual_authority?: false,
                         event_occurrence_verified?: false,
                         referent_identity_verified?: false,
                         llm_called?: false} = evidence}
          when outcome in [:answer, :clarify, :abstain] and is_binary(text) ->
        session = Map.fetch!(si, :session_id)
        question = Map.get(si, :sentence)

        meta =
          evidence
          |> Map.put(:action, if(outcome == :answer, do: :conversation_recall, else: :clarify))
          |> Map.put(:intent_inferred, :question)
          |> Map.put(:session_id, session)
          |> Map.put(:memory_source, :same_session_finite_past_report)
          |> Map.put(:retrieval_found?, outcome == :answer)
          |> Map.put(:response_source, :symbolic_finite_past_report)
          |> Map.put(:semantic_verified?, false)
          |> Map.put(:confidence, Map.get(si, :confidence, 0.0))

        # Match the memory-reply bookkeeping while preserving the existing
        # response attachment's separate final guardrail pass.
        SideEffects.record_turn(session, question, text, si)
        SideEffects.emit_plan(meta)
        {:warm, text, meta}
    end
  end

  def route(_si, _opts, original_plan) when is_function(original_plan, 0),
    do: original_plan.()

  defp decision(si, opts) do
    session = Map.get(si, :session_id)
    question = Map.get(si, :sentence)
    enabled? = Keyword.keyword?(opts) and
      Keyword.get(opts, :finite_past_response_arbitration?, false) == true

    if enabled? and Map.get(si, :source) in [:user, :prod] and
         is_binary(session) and byte_size(session) in 1..128 and
         is_binary(question) do
      Arbitration.decide(session, question, finite_past_response_arbitration?: true)
    else
      :pass
    end
  end
end
