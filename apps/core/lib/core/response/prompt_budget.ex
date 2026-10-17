defmodule Core.Response.PromptBudget do
  @moduledoc """
  Estimated LLM input budget for prompt sections and alternate history windows.

  Uses a deterministic, dependency-free characters/4 estimate. These are NOT
  tokenizer-exact counts or provider-billed tokens; message-role overhead and
  any model-specific chat template are excluded.

  Prompt attribution uses counterfactual rendering: rebuild the identical
  system prompt without recollections, then without retrieved source material,
  and finally without verified response-frame conclusions. This accounts for the real wording and citations injected by
  ResponseFrame and LlmPrompt instead of attributing raw SI map sizes.
  """

  @estimator "ceil_chars_div_4"

  @spec measure(map(), (map() -> String.t()), list(), list(), list(), String.t()) :: map()
  def measure(context, build_prompt, full_history, selective_history, active_history, user_text)
      when is_map(context) and is_function(build_prompt, 1) and
             is_list(full_history) and is_list(selective_history) and
             is_list(active_history) and is_binary(user_text) do
    system = build_prompt.(context)
    without_episodes = build_prompt.(strip_episodes(context))
    without_retrieval = build_prompt.(context |> strip_episodes() |> strip_retrieval())
    without_evidence =
      build_prompt.(context |> strip_episodes() |> strip_retrieval() |> strip_verified())

    system_chars = String.length(system)
    episode_chars = max(system_chars - String.length(without_episodes), 0)
    retrieval_chars = max(String.length(without_episodes) - String.length(without_retrieval), 0)
    verified_chars = max(String.length(without_retrieval) - String.length(without_evidence), 0)
    instruction_chars =
      max(system_chars - episode_chars - retrieval_chars - verified_chars, 0)

    sections = %{
      instructions: estimate_chars(instruction_chars),
      episodic_recall: estimate_chars(episode_chars),
      retrieved_evidence: estimate_chars(retrieval_chars),
      verified_evidence: estimate_chars(verified_chars),
      current_user: estimate_chars(String.length(user_text))
    }

    full = budget(sections, full_history)
    selective = budget(sections, selective_history)
    actual = budget(sections, active_history)

    %{
      estimator: @estimator,
      system_chars: system_chars,
      section_estimated_tokens: sections,
      full: full,
      selective: selective,
      actual: actual,
      potential_savings_tokens: max(full.total - selective.total, 0),
      potential_savings_percent:
        if(full.total > 0,
          do: Float.round(100 * max(full.total - selective.total, 0) / full.total, 2),
          else: 0.0)
    }
  end

  defp budget(sections, history) do
    history_tokens =
      Enum.reduce(history, 0, fn message, acc ->
        acc + estimate_chars(String.length(message_text(message)))
      end)

    components = Map.put(sections, :history, history_tokens)
    %{total: Enum.sum(Map.values(components)), components: components,
      history_message_count: length(history)}
  end

  defp message_text(%{"content" => text}) when is_binary(text), do: text
  defp message_text(%{content: text}) when is_binary(text), do: text
  defp message_text(_), do: ""

  defp estimate_chars(chars) when is_integer(chars) and chars > 0, do: div(chars + 3, 4)
  defp estimate_chars(_), do: 0

  defp strip_episodes(context) do
    context
    |> Map.delete(:episodic_recall)
    |> update_features(&Map.delete(&1, :episodic_recall))
  end

  defp strip_retrieval(context) do
    context
    |> Map.delete(:web_evidence)
    |> update_features(&Map.delete(&1, :web_evidence))
  end

  defp strip_verified(context) do
    context
    |> Map.update(:response_frame, nil, &remove_frame_evidence/1)
    |> update_features(fn features ->
      Map.update(features, :response_frame, nil, &remove_frame_evidence/1)
    end)
  end

  defp remove_frame_evidence(%{} = frame), do: Map.delete(frame, :evidence)
  defp remove_frame_evidence(other), do: other

  defp update_features(context, update) do
    case Map.get(context, :features) do
      %{} = features -> Map.put(context, :features, update.(features))
      _ -> context
    end
  end
end
