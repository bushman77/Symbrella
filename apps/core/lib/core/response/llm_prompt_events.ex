defmodule Core.Response.LlmPromptEvents do
  @moduledoc """
  Pure builders for LLM prompt and completion telemetry payloads.
  """

  @max_system_chars 8_000
  @max_user_chars 2_000

  @spec prompt(String.t(), term(), map()) :: {map(), map()}
  def prompt(system_prompt, user_text, context)
      when is_binary(system_prompt) and is_map(context) do
    {system_preview, system_truncated?} = cap_text(system_prompt, @max_system_chars)
    {user_preview, user_truncated?} = cap_text(user_text, @max_user_chars)
    prompt_fields = prompt_fields(system_prompt)

    measurements = %{
      system_chars: String.length(system_prompt),
      user_chars: String.length(to_string(user_text || ""))
    }

    metadata = %{
      session_id: Map.get(context, :session_id, :global),
      intent: get_in_map(context, [:features, :intent]),
      mode: get_in_map(context, [:decision, :mode]),
      tone: get_in_map(context, [:decision, :tone]),
      response_profile: Map.get(prompt_fields, :response_profile),
      simulated_affect: Map.get(prompt_fields, :simulated_affect),
      personality_state: Map.get(prompt_fields, :personality_state),
      system_sha256: sha256_hex(system_prompt),
      system_prompt: system_preview,
      system_truncated?: system_truncated?,
      user_text: user_preview,
      user_truncated?: user_truncated?
    }

    {measurements, metadata}
  end

  @spec complete(term(), term(), map(), String.t(), map() | nil) :: {map(), map()}
  def complete(user_text, assistant_text, context, system_prompt, reflection)
      when is_map(context) and is_binary(system_prompt) do
    {user_preview, user_truncated?} = cap_text(user_text, @max_user_chars)
    {assistant_preview, assistant_truncated?} = cap_text(assistant_text, @max_user_chars)
    prompt_fields = prompt_fields(system_prompt)
    prompt_profile = Map.get(prompt_fields, :response_profile)

    measurements = %{
      user_chars: String.length(to_string(user_text || "")),
      assistant_chars: String.length(to_string(assistant_text || ""))
    }

    metadata = %{
      session_id: Map.get(context, :session_id, :global),
      intent: get_in_map(context, [:features, :intent]),
      mode: get_in_map(context, [:decision, :mode]),
      tone: get_in_map(context, [:decision, :tone]),
      response_profile: response_profile_value(prompt_profile, context),
      prompt_response_profile: prompt_profile,
      simulated_affect: Map.get(prompt_fields, :simulated_affect),
      personality_state: Map.get(prompt_fields, :personality_state),
      system_sha256: sha256_hex(system_prompt),
      symbolic_frame: Map.get(context, :symbolic_frame),
      user_text: user_preview,
      user_truncated?: user_truncated?,
      assistant_text: assistant_preview,
      assistant_truncated?: assistant_truncated?,
      reflection: reflection_summary(reflection)
    }

    {measurements, metadata}
  end

  @spec cap_text(term(), pos_integer()) :: {String.t(), boolean()}
  def cap_text(text, max_chars)
      when is_binary(text) and is_integer(max_chars) and max_chars > 0 do
    if String.length(text) <= max_chars do
      {text, false}
    else
      {String.slice(text, 0, max_chars) <> "…", true}
    end
  end

  def cap_text(other, max_chars), do: cap_text(to_string(other || ""), max_chars)

  @spec sha256_hex(String.t()) :: String.t()
  def sha256_hex(text) when is_binary(text) do
    :crypto.hash(:sha256, text)
    |> Base.encode16(case: :lower)
  end

  @spec prompt_fields(String.t()) :: map()
  def prompt_fields(system_prompt) when is_binary(system_prompt) do
    %{
      response_profile: prompt_line_value(system_prompt, "Response profile:"),
      simulated_affect: prompt_line_value(system_prompt, "Simulated affect:"),
      personality_state: prompt_line_value(system_prompt, "Personality state:")
    }
  end

  def prompt_fields(_), do: %{}

  @spec extract_system_user(list()) :: {String.t(), String.t()}
  def extract_system_user(messages) when is_list(messages) do
    system =
      messages
      |> Enum.find(%{}, fn message -> Map.get(message, "role") == "system" end)
      |> Map.get("content", "")

    user =
      messages
      |> Enum.reverse()
      |> Enum.find(%{}, fn message -> Map.get(message, "role") == "user" end)
      |> Map.get("content", "")

    {to_string(system || ""), to_string(user || "")}
  end

  def extract_system_user(_), do: {"", ""}

  defp prompt_line_value(prompt, prefix) when is_binary(prompt) and is_binary(prefix) do
    prompt
    |> String.split("\n")
    |> Enum.find("", &String.starts_with?(&1, prefix))
    |> String.replace_prefix(prefix, "")
    |> String.trim()
    |> String.trim_trailing(".")
    |> blank_to_nil()
  end

  defp response_profile_value(prompt_profile, context) do
    prompt_profile
    |> existing_atom_value()
    |> case do
      nil -> get_in_map(context, [:decision, :response_profile])
      value -> value
    end
  end

  defp existing_atom_value(value) when is_atom(value), do: value

  defp existing_atom_value(value) when is_binary(value) do
    try do
      String.to_existing_atom(value)
    rescue
      ArgumentError -> value
    end
  end

  defp existing_atom_value(_), do: nil

  defp reflection_summary(%{} = reflection) do
    Map.take(reflection, [
      :v,
      :status,
      :confidence,
      :issues,
      :critique,
      :repair_instruction,
      :applied?,
      :repair_count,
      :draft_sha256,
      :final_sha256,
      :draft_text,
      :final_text,
      :at_ms
    ])
  end

  defp reflection_summary(_), do: nil

  defp blank_to_nil(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp blank_to_nil(value), do: value

  defp get_in_map(map, keys) when is_map(map) and is_list(keys) do
    Enum.reduce_while(keys, map, fn key, acc ->
      case map_get(acc, key) do
        nil -> {:halt, nil}
        value -> {:cont, value}
      end
    end)
  end

  defp get_in_map(_, _), do: nil

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
