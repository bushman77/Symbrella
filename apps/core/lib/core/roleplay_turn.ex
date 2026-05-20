defmodule Core.RoleplayTurn do
  @moduledoc """
  First roleplay-turn seam for Symbrella's OpenAI-compatible gateway.

  This module receives an OpenAI-style chat-completion request from
  SymbrellaWeb, normalizes the messages, sends them to the local LLM layer,
  and returns an OpenAI-compatible response map.

  First-layer responsibility:

      SillyTavern
        -> SymbrellaWeb /v1/chat/completions
        -> Core.RoleplayTurn.run/1
        -> Llm.chat/2
        -> OpenAI-compatible response

  This module intentionally does not touch Brain, DB, Hippocampus, LIFG,
  PMTG, ATL, WorkingMemory, or MoodCore yet.
  """

  @default_model "symbrella-rp"
  @default_temperature 0.7
  @default_timeout 120_000

  @type request :: map()
  @type response :: map()

  @doc """
  Runs one roleplay turn from an OpenAI-compatible chat-completion request.
  """
  @spec run(request()) :: {:ok, response()} | {:error, map()}
  def run(request) when is_map(request) do
    model = normalize_model(Map.get(request, "model"))

    messages =
      request
      |> Map.get("messages", [])
      |> normalize_messages()
      |> attach_brain_context()

    opts = [
      temperature: normalize_temperature(Map.get(request, "temperature")),
      timeout: normalize_timeout(Map.get(request, "timeout")),
      stop: normalize_stop(Map.get(request, "stop"))
    ]

    case Llm.chat(messages, opts) do
      {:ok, %{content: content, raw: raw}} ->
        {:ok, completion_response(model, content, raw)}

      {:error, reason} ->
        {:error, llm_error(reason)}
    end
  end

  def run(_request) do
    {:error,
     %{
       message: "Expected an OpenAI-compatible chat-completion request map.",
       type: "invalid_request_error",
       param: nil,
       code: "invalid_request"
     }}
  end

  defp normalize_model(model) when is_binary(model) and byte_size(model) > 0, do: model
  defp normalize_model(_model), do: @default_model

  defp normalize_messages(messages) when is_list(messages) do
    Enum.map(messages, fn
      %{"role" => role, "content" => content}
      when is_binary(role) and is_binary(content) ->
        %{"role" => role, "content" => content}

      %{role: role, content: content}
      when is_binary(role) and is_binary(content) ->
        %{"role" => role, "content" => content}

      other ->
        %{"role" => "user", "content" => inspect(other)}
    end)
  end

  defp normalize_messages(_messages), do: []

  defp normalize_temperature(value) when is_float(value), do: value
  defp normalize_temperature(value) when is_integer(value), do: value / 1
  defp normalize_temperature(_value), do: @default_temperature

  defp normalize_timeout(value) when is_integer(value) and value > 0, do: value
  defp normalize_timeout(_value), do: @default_timeout

  defp completion_response(model, content, raw) do
    %{
      id: completion_id(),
      object: "chat.completion",
      created: System.system_time(:second),
      model: model,
      choices: [
        %{
          index: 0,
          message: %{
            role: "assistant",
            content: content
          },
          finish_reason: "stop"
        }
      ],
      usage: usage_from_raw(raw)
    }
  end

  defp usage_from_raw(%{"usage" => usage}) when is_map(usage) do
    %{
      prompt_tokens: Map.get(usage, "prompt_tokens", 0),
      completion_tokens: Map.get(usage, "completion_tokens", 0),
      total_tokens: Map.get(usage, "total_tokens", 0)
    }
  end

  defp usage_from_raw(_raw) do
    %{
      prompt_tokens: 0,
      completion_tokens: 0,
      total_tokens: 0
    }
  end

  defp llm_error(reason) do
    %{
      message: "Local LLM call failed: #{inspect(reason)}",
      type: "llm_backend_error",
      param: nil,
      code: "llm_call_failed"
    }
  end

  defp completion_id do
    "chatcmpl-symbrella-#{System.unique_integer([:positive])}"
  end

  defp normalize_stop(stops) when is_list(stops) and stops != [] do
    stops
    |> Enum.filter(&is_binary/1)
    |> case do
      [] -> default_stop_sequences()
      clean -> clean ++ default_stop_sequences()
    end
  end

  defp normalize_stop(stop) when is_binary(stop) do
    [stop | default_stop_sequences()]
  end

  defp normalize_stop(_), do: default_stop_sequences()

  defp attach_brain_context(messages) when is_list(messages) do
    case latest_user_content(messages) do
      nil ->
        messages

      text ->
        case resolve_roleplay_input(text) do
          {:ok, brain_context} when brain_context != "" ->
            insert_brain_context(messages, brain_context)

          _ ->
            messages
        end
    end
  end

  defp latest_user_content(messages) do
    messages
    |> Enum.reverse()
    |> Enum.find(fn
      %{"role" => "user", "content" => content} when is_binary(content) ->
        String.trim(content) != ""

      _ ->
        false
    end)
    |> case do
      %{"content" => content} -> content
      _ -> nil
    end
  end

  defp resolve_roleplay_input(text) do
    si =
      Core.resolve_input(text,
        mode: :prod,
        enrich_lexicon?: true,
        lexicon_stage?: true
      )

    {:ok, brain_context_from_si(si)}
  rescue
    error ->
      {:error, error}
  catch
    kind, reason ->
      {:error, {kind, reason}}
  end

  defp brain_context_from_si(si) when is_map(si) do
    tokens = preview_list(Map.get(si, :tokens, []), &token_text/1)
    choices = preview_list(Map.get(si, :lifg_choices, []), &choice_text/1)

    [
      "Symbrella brain-state context for the latest player input:",
      "intent=#{safe_inspect(Map.get(si, :intent, :unknown))}",
      "confidence=#{safe_inspect(Map.get(si, :confidence, 0.0))}",
      "keyword=#{safe_inspect(Map.get(si, :keyword, ""))}",
      "source=#{safe_inspect(Map.get(si, :source, :unknown))}",
      "tokens=#{tokens}",
      "lifg_choices=#{choices}",
      "",
      "Use this as hidden control context. Do not quote it directly.",
      "The player's input is still only an attempted action, not guaranteed success."
    ]
    |> Enum.join("\n")
  end

  defp brain_context_from_si(_), do: ""

  defp insert_brain_context([%{"role" => "system"} = system | rest], brain_context) do
    [system, %{"role" => "system", "content" => brain_context} | rest]
  end

  defp insert_brain_context(messages, brain_context) do
    [%{"role" => "system", "content" => brain_context} | messages]
  end

  defp preview_list(list, formatter) when is_list(list) do
    list
    |> Enum.take(8)
    |> Enum.map(formatter)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(", ")
  end

  defp preview_list(_, _), do: ""

  defp token_text(%{phrase: phrase}) when is_binary(phrase), do: phrase
  defp token_text(%{text: text}) when is_binary(text), do: text
  defp token_text(%{surface: surface}) when is_binary(surface), do: surface
  defp token_text(token) when is_binary(token), do: token
  defp token_text(_), do: ""

  defp choice_text(choice) when is_map(choice) do
    lemma = choice[:lemma] || choice[:token] || choice[:surface] || ""
    pos = choice[:pos] || choice[:chosen_pos] || ""
    score = choice[:score]

    [lemma, pos, fmt_score(score)]
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.join("/")
  end

  defp choice_text(_), do: ""

  defp fmt_score(nil), do: ""
  defp fmt_score(score) when is_float(score), do: :erlang.float_to_binary(score, decimals: 2)
  defp fmt_score(score), do: to_string(score)

  defp safe_inspect(value), do: inspect(value, limit: 20, printable_limit: 80)

  defp default_stop_sequences do
    [
      "<|im_end|>",
      "<|im_start|>",
      "<|endoftext|>"
    ]
  end
end
