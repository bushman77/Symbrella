defmodule Core.Response.OpenQuestionBoundary do
  @moduledoc """
  Distinguish a recognizable open-ended *request* from a lexical failure
  when the LLM is intentionally disabled and no grounded responder succeeded.

  A question construction is not proof of its topic or of any world event.
  This fallback records the requested operation and its evidence deficit;
  it never generates opinions, current news, assistant update claims, or
  facts from topic text. Existing evidence-bound routes take precedence.
  """

  @question_intents [:ask, :ask_info, :question, :query, :unknown,
                     :information_request, :smalltalk, :brain_introspect]

  # Surface-only request frames. Capture isn't treated as entity resolution:
  # the topic remains an unverified span, never a fact or dictionary sense.
  @opinion ~r/\A\s*(?:what\s+do\s+you\s+think\s+(?:of|about)|what(?:['’]s| is)\s+your\s+(?:take|opinion)\s+on|how\s+do\s+you\s+(?:feel|think)\s+about)\s+(?<topic>.+?)\s*[?.]?\s*\z/iu
  @current ~r/\A\s*(?:what(?:['’]s| is)\s+(?:new|happening|going\s+on)\s+(?:with|in|about)|what(?:['’s]| is)\s+the\s+latest\s+(?:on|about)|what\s+(?:has\s+been|is)\s+happening\s+(?:with|in))\s+(?<topic>.+?)\s*[?.]?\s*\z/iu
  @self_update ~r/\A\s*(?:what(?:['’s]| is)\s+new\s+(?:(?:with|about|in\s+the\s+world\s+of)\s+(?:you|yourself|symbrella))|what\s+have\s+you\s+been\s+up\s+to)\s*[?.]?\s*\z/iu

  @spec recognize(String.t()) :: {:ok, map()} | :not_applicable
  def recognize(text) when is_binary(text) and byte_size(text) <= 320 do
    cond do
      Regex.match?(@self_update, text) ->
        {:ok, %{operation: :assistant_update_request, evidence_needed: [:runtime_capabilities]}}

      match = Regex.named_captures(@opinion, text) ->
        topic_request(:assessment_request, match)

      match = Regex.named_captures(@current, text) ->
        topic_request(:current_information_request, match)

      true ->
        :not_applicable
    end
  end

  def recognize(_), do: :not_applicable

  defp topic_request(operation, %{"topic" => raw}) do
    topic = raw |> String.trim() |> String.trim_trailing(".")
                 |> String.trim_trailing("?") |> String.trim()

    # The capture is a bounded surface fragment, not an inferred geopolitical
    # event. Avoid promoting empty, control-laden, or oversized material.
    if String.length(topic) in 2..120 and
         not Regex.match?(~r/[\x00-\x1F\x7F]/u, topic) do
      {:ok, %{operation: operation, topic_surface: topic,
              evidence_needed: [:specific_claim, :dated_sources]}}
    else
      :not_applicable
    end
  end

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(text, %{risk_bucket: :low}, %{action: action}, si)
      when is_binary(text) and is_map(si) and
           action in [:answer, :offer_options, :act_first, :ask_first] do
    with true <- Map.get(si, :sentence) == text,
         true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- Map.get(si, :intent, :unknown) in @question_intents,
         {:ok, request} <- recognize(text) do
      response =
        case request.operation do
          :assistant_update_request ->
            "Are you asking what my current capabilities are, or what has changed in my system? I can't establish recent changes without verified runtime or version information."

          :assessment_request ->
            "You're asking for an assessment. I can help examine a specific claim or event, but I don't yet have verified evidence for an informed conclusion. Which aspect would you like to consider?"

          :current_information_request ->
            "You're asking for recent information. I need dated, attributable sources before I can give a reliable update. Which development would you like to examine?"
        end

      {:ok, response, %{
        status: :open_question_evidence_boundary,
        provenance: :current_turn_request_construction,
        requested_operation: request.operation,
        requested_topic_surface: Map.get(request, :topic_surface),
        missing_evidence: request.evidence_needed,
        topic_grounded?: false,
        semantic_verified?: false,
        external_facts_verified?: false,
        factual_claims?: false,
        llm_called?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  def generate(_, _, _, _), do: :not_applicable
end
