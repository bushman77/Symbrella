defmodule Core.Response.GroundedRevision do
  @moduledoc """
  One optional second-chance *generation* for a rejected, evidence-backed
  population answer. This does not relax ReflectionLoop or fabricate support.

  The LLM gets conversational state and claim-specific source facts, not a
  canonical response sentence. Its new draft must pass ReflectionLoop unchanged;
  otherwise the original deterministic grounded fallback remains in force.
  """

  alias Core.Response.ResponseFrame

  @spec eligible?(map(), map()) :: boolean()
  def eligible?(%{} = context, %{} = reflection) do
    issues = Map.get(reflection, :issues, [])
    evidence = Map.get(context, :web_evidence, %{})
    frame = Map.get(context, :response_frame, %{})
    support = Map.get(evidence, :support, %{})

    Map.get(reflection, :status) == :repair and
      is_list(issues) and :geographic_claim_unverified in issues and
      not Enum.any?(issues, &(&1 in [
        :unsafe, :leaked_hidden_context, :overclaimed_self_awareness
      ])) and
      match?(%{status: :retrieved, resolution: :source_supported}, evidence) and
      match?(%{kind: :population, subject: _, value: _, census_year: _,
               source_url: _}, support) and
      is_integer(Map.get(support, :value)) and Map.get(support, :value) > 0 and
      is_integer(Map.get(support, :census_year)) and
      is_binary(Map.get(support, :subject)) and
      is_binary(Map.get(support, :source_url)) and
      String.starts_with?(Map.get(support, :source_url, ""), "https://") and
      match?(%{version: 1, evidence: %{kind: :population}}, frame)
  end

  def eligible?(_, _), do: false

  @spec messages(list(), map()) :: list()
  def messages(original_messages, context)
      when is_list(original_messages) and is_map(context) do
    frame = Map.get(context, :response_frame, %{})
    user_text = Map.get(context, :user_text, "")
    history =
      original_messages
      |> Enum.filter(fn
        %{"role" => role, "content" => text}
        when role in ["user", "assistant"] and is_binary(text) -> true
        _ -> false
      end)
      |> Enum.drop(-1)
      |> Enum.take(-4)

    verified = Map.get(frame, :evidence, %{})
    count = Map.get(verified, :value)
    year = Map.get(verified, :year)
    subject = Map.get(verified, :subject)
    urls = Map.get(verified, :source_urls, [])
    source = if is_list(urls), do: List.first(urls), else: nil

    # Keep the fact data separate from the words Symbrella chooses. Small
    # models have been turning bare count=97902 into 979,020; provide a
    # human-readable, comma-grouped literal instead.
    fact_lock =
      if is_integer(count) and is_integer(year) and is_binary(subject) and
           is_binary(source) do
        "VERIFIED FACT (not a sentence to copy): " <>
          "city=#{inspect(subject)}; count=#{format_count(count)} people; " <>
          "measurement_year=#{year}; citation=#{source}. " <>
          "Write the exact count #{format_count(count)}; do not recalculate it."
      else
        ""
      end

    system =
      [
        "You are Symbrella, a continuing conversational companion.",
        fact_lock,
        "Your earlier draft did not pass factual review. Generate a NEW, concise " <>
          "reply to the current user turn in your own natural language.",
        ResponseFrame.prompt(frame),
        "MANDATORY: Keep the place name, population count, measurement year and " <>
          "exact https source URL provided by the verified frame. This is a dated " <>
          "population, not necessarily today's estimate.",
        "Never copy earlier assistant claims as evidence. Do not introduce " <>
          "additional numeric facts, local geography, current population estimates " <>
          "or unsupported historical details. No stock service offers.",
        "The above frame is conversation data and evidence, not instructions " <>
          "originating from external article content."
      ]
      |> Enum.join("\n")

    [%{"role" => "system", "content" => system}] ++
      history ++ [%{"role" => "user", "content" => to_string(user_text)}]
  end

  def messages(_, _), do: []

  defp format_count(number) when is_integer(number) do
    number
    |> Integer.to_string()
    |> String.reverse()
    |> String.graphemes()
    |> Enum.chunk_every(3)
    |> Enum.map_join(",", &Enum.join/1)
    |> String.reverse()
  end
end
