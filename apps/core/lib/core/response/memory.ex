defmodule Core.Response.Memory do
  @moduledoc """
  Name and fact memory reply policy for `Core.Response`.

  Parsing and reply shaping live here; durable writes and reads stay behind
  `Core.Response.SideEffects`.
  """

  alias Core.Response.SideEffects

  @spec reply(map()) :: {atom(), String.t(), map()} | nil
  def reply(si) when is_map(si) do
    text_in = si_text(si)

    reply_for(%{
      intent0: Map.get(si, :intent, :unknown),
      conf: clamp01(Map.get(si, :confidence, 0.0)),
      text_in: text_in,
      session_id: session_id(si),
      extracted_name: extract_user_name(text_in),
      remember_fact: extract_remember_fact(text_in),
      direct_fact: extract_direct_fact(text_in),
      fact_query_key: extract_fact_query_key(text_in)
    })
  end

  def reply(_), do: nil

  @spec reply_for(map()) :: {atom(), String.t(), map()} | nil
  def reply_for(%{
        intent0: intent0,
        conf: conf,
        text_in: text_in,
        session_id: session_id,
        extracted_name: extracted_name,
        remember_fact: remember_fact,
        direct_fact: direct_fact,
        fact_query_key: fact_query_key
      }) do
    cond do
      asking_for_user_name?(text_in) ->
        name = SideEffects.recalled_user_name(extracted_name)

        text =
          if is_binary(name) and name != "" do
            "Your name is #{name}."
          else
            "I don’t know your name yet—tell me “my name is …” and I’ll remember it."
          end

        meta = %{
          action: :identity,
          intent_inferred: :name_query,
          response_source: :memory,
          memory_key: :user_name,
          memory_source: :hippocampus_fact,
          session_id: session_id,
          user_name: name,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_reply(session_id, text_in, text, meta)

      remember_fact ->
        {key, label, value} = remember_fact
        text = remember_fact_response(key, label, value, text_in, true)

        meta = %{
          action: :remember_fact,
          intent_inferred: :memory_write,
          response_source: :memory,
          memory_key: key,
          memory_source: :hippocampus_fact,
          session_id: session_id,
          fact_key: key,
          fact_label: label,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_reply(session_id, text_in, text, meta)

      direct_fact ->
        {key, label, value} = direct_fact
        text = remember_fact_response(key, label, value, text_in, false)

        meta = %{
          action: :remember_fact,
          intent_inferred: :memory_write,
          response_source: :memory,
          memory_key: key,
          memory_source: :hippocampus_fact,
          session_id: session_id,
          fact_key: key,
          fact_label: label,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_reply(session_id, text_in, text, meta)

      fact_query_key ->
        {key, label} = fact_query_key
        value = SideEffects.recalled_fact(key)

        text =
          if is_binary(value) and value != "" do
            "Your #{label} is #{value}."
          else
            "I don’t know your #{label} yet—tell me “remember that my #{label} is …” and I’ll remember it."
          end

        meta = %{
          action: :recall_fact,
          intent_inferred: :fact_query,
          response_source: :memory,
          memory_key: key,
          memory_source: :hippocampus_fact,
          session_id: session_id,
          fact_key: key,
          fact_label: label,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_reply(session_id, text_in, text, meta)

      true ->
        nil
    end
  end

  @spec forced_identity_text(String.t(), String.t() | nil, boolean()) :: String.t() | nil
  def forced_identity_text(text_in, extracted_name, name_claim?) do
    cond do
      asking_for_user_name?(text_in) ->
        name =
          normalize_name(extracted_name) ||
            SideEffects.recalled_user_name(nil)

        if is_binary(name) and name != "" do
          "Your name is #{name}."
        else
          "I don’t know your name yet—tell me “my name is …” and I’ll remember it."
        end

      name_claim? ->
        name = normalize_name(extracted_name)
        if name, do: "Nice to meet you, #{name}. I’ll remember that.", else: nil

      true ->
        nil
    end
  end

  @spec extract_user_name(String.t()) :: String.t() | nil
  def extract_user_name(text) when is_binary(text) do
    case Regex.run(~r/\bmy name is\s+([A-Za-z][A-Za-z'\- ]{0,40})\b/i, text) do
      [_, name] ->
        name =
          name
          |> String.trim()
          |> String.replace(~r/\s+/u, " ")
          |> String.split(" ", trim: true)
          |> Enum.take(3)
          |> Enum.join(" ")

        if name == "", do: nil, else: name

      _ ->
        nil
    end
  end

  def extract_user_name(_), do: nil

  @spec extract_remember_fact(String.t()) :: {String.t(), String.t(), String.t()} | nil
  def extract_remember_fact(text) when is_binary(text) do
    with [_, body] <- Regex.run(~r/^\s*rem?em?ber(?:\s+that)?\s+(.+?)\s*[\.\!]*\s*$/iu, text),
         {label, value} <- split_fact_body(body),
         key when is_binary(key) <- fact_key(label) do
      {key, label, value}
    else
      _ -> nil
    end
  end

  def extract_remember_fact(_), do: nil

  @spec extract_direct_fact(String.t()) :: {String.t(), String.t(), String.t()} | nil
  def extract_direct_fact(text) when is_binary(text) do
    cond do
      asking_for_user_name?(text) ->
        nil

      Core.Response.OverrideSkills.self_check_query?(text) ->
        nil

      question_shaped?(text) ->
        nil

      extract_fact_query_key(text) ->
        nil

      location = extract_location_fact(text) ->
        location

      true ->
        with {label, value} <- split_fact_body(text),
             key when is_binary(key) <- fact_key(label) do
          {key, label, value}
        else
          _ -> nil
        end
    end
  end

  def extract_direct_fact(_), do: nil

  @spec extract_fact_query_key(String.t()) :: {String.t(), String.t()} | nil
  def extract_fact_query_key(text) when is_binary(text) do
    cond do
      Regex.match?(~r/^\s*where\s+do\s+i\s+live\s*\??\s*$/iu, text) ->
        {"location", "location"}

      true ->
        with [_, label] <- Regex.run(~r/^\s*what\s+is\s+my\s+(.+?)\s*\??\s*$/iu, text),
             key when is_binary(key) <- fact_key(label) do
          {key, normalize_fact_label(label)}
        else
          _ -> nil
        end
    end
  end

  def extract_fact_query_key(_), do: nil

  @spec asking_for_user_name?(String.t()) :: boolean()
  def asking_for_user_name?(text) when is_binary(text) do
    fuzzy = Core.Text.Fuzzy.interpret(text)

    t =
      fuzzy.text
      |> String.replace(~r/[^\p{L}\p{N}\s\?]/u, "")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    t == "what is my name" or t == "what is my name?" or
      t == "whats my name" or t == "whats my name?" or
      String.contains?(t, "what is my name") or
      String.contains?(t, "whats my name") or
      :asking_for_user_name in fuzzy.aliases
  end

  def asking_for_user_name?(_), do: false

  defp finish_reply(session_id, text_in, text, meta) do
    meta =
      meta
      |> Map.put_new(:response_source, :memory)
      |> Map.put_new(:intent_inferred, Map.get(meta, :action))

    SideEffects.record_turn(session_id, text_in, text)
    SideEffects.emit_plan(meta)
    {:warm, text, meta}
  end

  defp remember_fact_response(key, label, value, raw_text, explicit?)
       when is_binary(key) and is_binary(label) and is_binary(value) do
    SideEffects.persist_user_fact(key, label, value, raw_text)

    if explicit? do
      "I’ll remember that your #{label} is #{value}."
    else
      "I’ve noted that your #{label} is #{value}."
    end
  end

  defp normalize_name(name) when is_binary(name) do
    n =
      name
      |> String.trim()
      |> String.replace(~r/\s+/u, " ")

    if n == "", do: nil, else: n
  end

  defp normalize_name(_), do: nil

  defp question_shaped?(text) when is_binary(text) do
    Regex.match?(
      ~r/^\s*(?:who|what|when|where|why|how|do|does|did|can|could|will|would|should|is|are|am|have|has|had|may|might|was|were)\b/iu,
      text
    ) or String.contains?(text, "?")
  end

  defp question_shaped?(_), do: false

  defp extract_location_fact(text) when is_binary(text) do
    case Regex.run(
           ~r/^\s*i\s+live\s+in\s+(.+?)[\.\!]*\s*$/iu,
           text
         ) do
      [_, value] ->
        value =
          value
          |> strip_remember_suffix()
          |> normalize_fact_value()

        if value == "", do: nil, else: {"location", "location", value}

      _ ->
        nil
    end
  end

  defp strip_remember_suffix(value) when is_binary(value) do
    value
    |> String.replace(
      ~r/(?:,\s*)?(?:please\s+)?rem?em?ber\s+that\s*$/iu,
      ""
    )
    |> String.trim()
  end

  defp split_fact_body(body) when is_binary(body) do
    case Regex.run(~r/^\s*(?:my\s+)?(.+?)\s+(?:is|=)\s+(.+?)\s*$/iu, body) do
      [_, label, value] ->
        label = normalize_fact_label(label)
        value = normalize_fact_value(value)

        if label != "" and value != "" and not reserved_fact_label?(label) do
          {label, value}
        end

      _ ->
        nil
    end
  end

  defp fact_key(label) when is_binary(label) do
    label
    |> normalize_fact_label()
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/u, "_")
    |> String.trim("_")
    |> case do
      "" -> nil
      "name" -> nil
      key -> key
    end
  end

  defp normalize_fact_label(label) when is_binary(label) do
    label
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/^(?:my|the)\s+/iu, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize_fact_value(value) when is_binary(value) do
    value
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim_trailing(".")
  end

  defp reserved_fact_label?("name"), do: true
  defp reserved_fact_label?(_), do: false

  defp session_id(si) when is_map(si) do
    Map.get(si, :session_id) ||
      Map.get(si, :session) ||
      Map.get(si, :conversation_id) ||
      :global
  end

  defp si_text(si) when is_map(si) do
    si
    |> Map.get(:text, Map.get(si, :keyword, ""))
    |> to_string()
  end

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp01(_), do: 0.0
end
