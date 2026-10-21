defmodule Core.Response.UserLexicalMemory do
  @moduledoc """
  Owner-scoped, user-attributed definitions of explicitly named terms.

  These are conversation-supplied provisional meanings, never dictionary
  senses or externally verified facts. Writes use the existing durable
  Hippocampus user-event contract; reads require the exact owner, term,
  relation and source-frame. No LLM response is consulted.
  """

  alias Core.Response.EpisodicMemory

  @relation "user_defined_meaning"
  @source_frame "explicit_user_definition"
  @term ~r/^[\p{L}][\p{L}\p{N}_-]{4,31}$/u
  @ignored_terms ~w(about after before because could would should which their these those where there someone something myself itself)
  @definition ~r/^\s*by\s+(?<term>[\p{L}][\p{L}\p{N}_-]{4,31})\s*,?\s+i\s+mean\s+(?<meaning>[^?\r\n]{5,240})\s*$/iu
  @question [
    ~r/^\s*what\s+(?:is|was)\s+(?<term>[\p{L}][\p{L}\p{N}_-]{4,31})\s*\?\s*$/iu,
    ~r/^\s*what\s+does\s+(?<term>[\p{L}][\p{L}\p{N}_-]{4,31})\s+mean\s*\?\s*$/iu,
    ~r/^\s*do\s+you\s+remember\s+what\s+(?<term>[\p{L}][\p{L}\p{N}_-]{4,31})\s+(?:is|means)\s*\?\s*$/iu,
    ~r/^\s*what\s+did\s+i\s+(?:tell|say)\s+you\s+(?<term>[\p{L}][\p{L}\p{N}_-]{4,31})\s+(?:is|means)\s*\?\s*$/iu
  ]

  @spec definition_from(String.t()) :: {String.t(), String.t()} | nil
  def definition_from(text) when is_binary(text) do
    with %{"term" => term, "meaning" => meaning} <- Regex.named_captures(@definition, text),
         true <- valid_term?(term),
         meaning <- meaning |> String.trim() |> String.trim_trailing(".") |> String.trim(),
         true <- byte_size(meaning) in 5..240 do
      {String.downcase(term), meaning}
    else
      _ -> nil
    end
  end

  def definition_from(_), do: nil

  @spec query_term(String.t()) :: String.t() | nil
  def query_term(text) when is_binary(text) do
    Enum.find_value(@question, fn pattern ->
      case Regex.named_captures(pattern, text) do
        %{"term" => term} ->
          if valid_term?(term), do: String.downcase(term), else: nil
        _ -> nil
      end
    end)
  end

  def query_term(_), do: nil

  # Only the completed, explicitly user-originated SI can create a durable
  # memory. Never persist an extracted model, synthetic or replayed sentence.
  @spec remember_user_turn(String.t(), term(), map() | nil) ::
          :ok | {:error, atom()}
  def remember_user_turn(text, sid, %{} = si) when is_binary(text) do
    with true <- user_originated?(si),
         ^text <- Map.get(si, :sentence),
         {term, meaning} <- definition_from(text) do
      EpisodicMemory.remember_event(%{
        entity: term,
        relation: @relation,
        value: meaning,
        source_frame: :explicit_user_definition
      }, sid, text)
    else
      _ -> :ok
    end
  end

  def remember_user_turn(_, _, _), do: :ok

  @spec recall(String.t()) :: map() | nil
  def recall(term) when is_binary(term) do
    if valid_term?(term) do
      case EpisodicMemory.recall(String.downcase(term), @relation) do
        %{status: "asserted", provenance: :user_reported, value: meaning,
          source_frame: source_frame} = event
        when is_binary(meaning) and source_frame in [@source_frame, :explicit_user_definition] ->
          if byte_size(meaning) in 5..240, do: event, else: nil

        _ -> nil
      end
    else
      nil
    end
  end

  def recall(_), do: nil

  @spec reply(String.t()) :: {String.t(), map()} | nil
  def reply(text) when is_binary(text) do
    with term when is_binary(term) <- query_term(text),
         %{value: meaning, id: id} <- recall(term) do
      {"You told me that \"#{term}\" means #{meaning}. This is your description, not an independently verified definition.",
       %{action: :conversation_recall, memory_source: :hippocampal_user_definition,
         response_source: :symbolic_conversation, provenance: :user_reported,
         source: :user_reported_event, verification: :conversation_context_only,
         retrieval_found?: true, memory_event_id: id}}
    else
      _ -> nil
    end
  end

  def reply(_), do: nil

  defp user_originated?(si) do
    Map.get(si, :source) == :user or
      (Map.get(si, :source) == :prod and
         Map.get(si, :experience_origin) == :user_turn)
  end

  defp valid_term?(term) when is_binary(term) do
    Regex.match?(@term, term) and String.downcase(term) not in @ignored_terms
  end

  defp valid_term?(_), do: false
end
