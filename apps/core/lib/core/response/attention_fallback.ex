defmodule Core.Response.AttentionFallback do
  @moduledoc """
  Native continuation from the current session's admitted comprehension and
  attention frame. No lexical guess, external fact, LLM, or cross-session
  global-WM item may authorize a response.

  This is deliberately weaker than answer generation: a focus is a topic,
  not an answer. Keep the distinction observable through response metadata.
  """

  alias Core.Brain.WM.Comprehension, as: WMComprehension

  @max_terms 2
  @question_intents [:ask, :ask_info, :question, :query, :help, :information_request]

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(text, %{risk_bucket: :low} = features, %{action: action}, si)
      when is_binary(text) and is_map(si) and
             action in [:answer, :offer_options, :act_first] do
    with session when is_binary(session) and byte_size(session) in 1..128 <-
           Map.get(si, :session_id),
         ^text <- Map.get(si, :sentence),
         true <- Map.get(si, :source) in [:user, :prod, :test],
         %{} = context <- Map.get(si, :wm_context),
         %{} = admitted <- WMComprehension.admitted_context(session),
         true <- context == admitted,
         :conversation_context_only <- Map.get(context, :verification),
         %{scope: :session} <- Map.get(context, :attention),
         true <- Map.get(context, :focus_transition) != :release,
         {:ok, kind, terms} <- focused_terms(context) do
      question? = Map.get(features, :intent) in @question_intents or
        String.ends_with?(String.trim(text), "?")

      reply = realize(kind, terms, question?)

      {:ok, reply, %{
        status: :attention_continuation,
        provenance: :admitted_same_session_comprehension,
        focus_kind: kind,
        focus_terms: terms,
        evidence_level: :conversation_context_only,
        semantic_verified?: false,
        factual_claims?: false,
        llm_called?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  def generate(_, _, _, _), do: :not_applicable

  defp focused_terms(context) do
    topic = safe_term(Map.get(context, :topic))
    entities = safe_terms(Map.get(context, :entities))
    recognized = safe_terms(Map.get(context, :recognized_topics))

    cond do
      is_binary(topic) -> {:ok, :topic, [topic]}
      entities != [] -> {:ok, :entities, entities}
      recognized != [] -> {:ok, :recognized_topics, recognized}
      true -> :not_applicable
    end
  end

  defp safe_terms(values) when is_list(values) do
    values
    |> Enum.map(&safe_term/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.take(@max_terms)
  end

  defp safe_terms(_), do: []

  defp safe_term(term) when is_binary(term) do
    term = String.trim(term)

    if byte_size(term) <= 96 and
         Regex.match?(~r/\A[\p{L}\p{N}][\p{L}\p{M}\p{N} ',.\-]*\z/u, term) do
      term
    end
  end

  defp safe_term(_), do: nil

  defp realize(:topic, [topic], true),
    do: "I'm following the topic of #{topic}, but I don't have a grounded answer to that yet."

  defp realize(:topic, [topic], false),
    do: "I'm keeping #{topic} in focus, though I haven't fully interpreted your point yet."

  defp realize(_kind, terms, true),
    do: "I'm keeping #{join_terms(terms)} in mind, but I can't answer that reliably yet."

  defp realize(_kind, terms, false),
    do: "I'm following what you're saying about #{join_terms(terms)}, but I haven't established the relationship yet."

  defp join_terms([first, second]), do: first <> " and " <> second
  defp join_terms([only]), do: only
end
