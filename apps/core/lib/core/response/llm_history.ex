defmodule Core.Response.LlmHistory do
  @moduledoc """
  Pure helpers for bounded LLM chat history.
  """

  alias Core.Response.Topics

  @default_max_item_chars 1_600

  @spec append_turn(list(), String.t(), String.t(), pos_integer(), pos_integer()) :: list()
  def append_turn(
        messages,
        user_text,
        assistant_text,
        turn_pairs,
        max_item_chars \\ @default_max_item_chars
      )
      when is_list(messages) and is_integer(turn_pairs) do
    messages
    |> Kernel.++([
      %{"role" => "user", "content" => clamp_text(user_text, max_item_chars)},
      %{"role" => "assistant", "content" => clamp_text(assistant_text, max_item_chars)}
    ])
    |> trim(turn_pairs)
  end

  @spec recent(list(), pos_integer()) :: list()
  def recent(messages, turn_pairs)
      when is_list(messages) and is_integer(turn_pairs) and turn_pairs > 0 do
    take_n = min(length(messages), turn_pairs * 2)
    Enum.take(messages, -take_n)
  end

  def recent(_messages, _turn_pairs), do: []

  @doc """
  Keep the newest complete user/assistant pairs within the available prompt
  characters. An older pair never displaces a more recent correction or
  referential antecedent. The stored history is not modified.
  """
  @spec within_char_budget(list(), non_neg_integer()) :: list()
  def within_char_budget(messages, max_chars)
      when is_list(messages) and is_integer(max_chars) and max_chars >= 0 do
    complete = Enum.take(messages, length(messages) - rem(length(messages), 2))

    complete
    |> Enum.chunk_every(2)
    |> Enum.reverse()
    |> Enum.reduce_while({[], 0}, fn
      [%{"role" => "user"}, %{"role" => "assistant"}] = pair, {kept, used} ->
        size = Enum.reduce(pair, 0, fn message, acc ->
          acc + message_size(message)
        end)

        if used + size <= max_chars do
          {:cont, {[pair | kept], used + size}}
        else
          {:halt, {kept, used}}
        end

      _, acc ->
        {:halt, acc}
    end)
    |> elem(0)
    |> List.flatten()
  end

  def within_char_budget(_, _), do: []

  defp message_size(%{"content" => text}) when is_binary(text),
    do: String.length(text) + 24

  defp message_size(_), do: 24

  @spec trim(list(), integer()) :: list()
  def trim(messages, turn_pairs) when is_list(messages) and is_integer(turn_pairs) do
    max_messages = max(0, turn_pairs * 2)
    if length(messages) <= max_messages, do: messages, else: Enum.take(messages, -max_messages)
  end

  @spec status(boolean(), list()) :: map()
  def status(table_present?, messages) when is_boolean(table_present?) and is_list(messages) do
    %{
      table_present?: table_present?,
      history_present?: messages != [],
      message_count: length(messages),
      turn_pairs: div(length(messages), 2),
      topics: Topics.from_messages(messages)
    }
  end

  @spec missing_status() :: map()
  def missing_status do
    %{
      table_present?: false,
      history_present?: false,
      message_count: 0,
      turn_pairs: 0
    }
  end

  @spec clamp_text(term(), pos_integer()) :: String.t()
  def clamp_text(text, max_chars \\ @default_max_item_chars)

  def clamp_text(text, max_chars)
      when is_binary(text) and is_integer(max_chars) and max_chars > 0 do
    text = String.trim(text)

    cond do
      text == "" -> ""
      String.length(text) <= max_chars -> text
      true -> String.slice(text, 0, max_chars) <> "…"
    end
  end

  def clamp_text(other, max_chars), do: other |> to_string() |> clamp_text(max_chars)
end
