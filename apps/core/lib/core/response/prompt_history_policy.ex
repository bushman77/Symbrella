defmodule Core.Response.PromptHistoryPolicy do
  @moduledoc """
  Pure, conservative comparison policy for hippocampal prompt compression.

  The bounded same-session LLM history remains untouched unless the caller
  opts into :selective_recall. Only a relevant same-session episodic projection
  makes a shorter history eligible. Never trim an ambiguous or corrective
  follow-up, or a turn that requires a previous question as its answer target.
  """

  @default_recent_pairs 2

  @spec candidate(list(), map(), String.t(), String.t() | nil, keyword()) :: map()
  def candidate(history, context, user_text, answer_target, opts \\ [])
      when is_list(history) and is_map(context) and is_binary(user_text) do
    keep = positive(Keyword.get(opts, :recent_pairs, @default_recent_pairs),
                    @default_recent_pairs)

    episodes = Map.get(context, :episodic_recall, [])
    usable? = is_list(episodes) and Enum.any?(episodes, &usable_episode?/1)
    original_pairs = div(length(history), 2)
    trimmed = Enum.take(history, -2 * keep)

    {eligible?, reason} =
      cond do
        not usable? -> {false, :no_relevant_episode}
        is_binary(answer_target) and String.trim(answer_target) != "" and
            String.trim(answer_target) != String.trim(user_text) ->
          {false, :prior_question_dependency}
        ambiguous?(user_text) -> {false, :conversational_dependency}
        length(trimmed) >= length(history) -> {false, :within_recent_window}
        true -> {true, :episodic_recall}
      end

    %{
      eligible?: eligible?,
      reason: reason,
      history: if(eligible?, do: trimmed, else: history),
      original_pairs: original_pairs,
      retained_pairs: div(length(if(eligible?, do: trimmed, else: history)), 2)
    }
  end

  @spec active_history(list(), map(), String.t(), String.t() | nil, keyword()) :: map()
  def active_history(history, context, user_text, answer_target, opts \\ []) do
    trial = candidate(history, context, user_text, answer_target, opts)

    mode =
      case Keyword.get(opts, :mode, :full) do
        :selective_recall -> :selective_recall
        _ -> :full
      end

    applied? = mode == :selective_recall and trial.eligible?

    trial
    |> Map.put(:mode, mode)
    |> Map.put(:applied?, applied?)
    |> Map.put(:actual_history, if(applied?, do: trial.history, else: history))
  end

  defp usable_episode?(%{source: :hippocampal_episode, text: text})
       when is_binary(text), do: String.trim(text) != ""
  defp usable_episode?(_), do: false

  defp ambiguous?(text) do
    Regex.match?(
      ~r/\b(?:it|its|this|that|those|these|they|them|their|he|she|him|her|his|hers|there|same|previous|earlier|above|again)\b|\b(?:what about|how about|you tell me|go on|continue|as i said|you're wrong|you are wrong|that's wrong|that is wrong)\b/iu,
      text
    )
  end

  defp positive(x, _default) when is_integer(x) and x > 0, do: x
  defp positive(_x, default), do: default
end
