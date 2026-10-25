defmodule SymbrellaWeb.SocialCognitiveReport do
  @moduledoc """
  Read-only, bounded sentence-by-sentence Colony cognition inspection.
  Does not call the stateful conversation pipeline or persist external text.
  """

  alias Core.Social.SemanticProbe
  alias SymbrellaWeb.{SocialUnderstanding, SocialDiscourse}

  @max_replies 8
  @max_sentences_per_source 12
  @max_total_sentences 48
  @max_sentence_chars 600
  @max_source_chars 12_000

  def analyze(input, opts \\ [])

  def analyze(%{topic: %{id: id} = topic, comments: comments}, opts)
      when is_binary(id) and is_list(comments) do
    probe = Keyword.get(opts, :probe, &SemanticProbe.run/2)

    sources =
      [%{label: "Original post", author: topic.author, text: join_text(topic.title, topic.body)}] ++
        (comments
         |> Enum.take(@max_replies)
         |> Enum.with_index(1)
         |> Enum.map(fn {comment, index} ->
           %{label: "Reply #{index}", author: comment.author, text: comment.body || ""}
         end))

    {samples, dropped_by_source} =
      Enum.reduce(sources, {[], 0}, fn source, {acc, dropped} ->
        sentences = split_sentences(source.text)
        kept = Enum.take(sentences, @max_sentences_per_source)

        next =
          kept
          |> Enum.with_index(1)
          |> Enum.map(fn {sentence, index} ->
            %{label: "#{source.label} · sentence #{index}", author: source.author, text: sentence}
          end)

        {acc ++ next, dropped + max(length(sentences) - length(kept), 0)}
      end)

    selected = Enum.take(samples, @max_total_sentences)

    analyses =
      Enum.map(selected, fn sample ->
        outcome =
          try do
            probe.(sample.text, emit_telemetry?: false)
          rescue
            _ -> {:error, :probe_unavailable}
          catch
            :exit, _ -> {:error, :probe_unavailable}
          end

        metadata =
          case outcome do
            {:ok, %{stage: :social_semantic_shadow} = result} ->
              %{status: :analyzed, result: result}
            {:error, reason} ->
              %{status: :unavailable, reason: reason}
            _ ->
              %{status: :unavailable, reason: :invalid_probe_response}
          end

        sample |> Map.put(:excerpt, String.slice(sample.text, 0, 240)) |> Map.delete(:text) |> Map.merge(metadata)
      end)

    {:ok, %{
      post_id: id,
      source: :colony_external,
      trust: :untrusted_external,
      mode: :read_only_shadow,
      analyzed_segments: length(analyses),
      omitted_replies: max(length(comments) - @max_replies, 0),
      omitted_sentences: dropped_by_source + length(samples) - length(selected),
      segments: analyses,
      understanding: SocialUnderstanding.build(analyses),
      discourse: SocialDiscourse.build(analyses)
    }}
  end

  def analyze(_, _), do: {:error, :invalid_social_analysis_input}

  defp join_text(title, body) do
    [title, body] |> Enum.filter(&is_binary/1) |> Enum.reject(&(&1 == "")) |> Enum.join("\n")
  end

  defp split_sentences(text) do
    text
    |> String.slice(0, @max_source_chars)
    |> String.split(~r/(?<=[.!?])\s+|\n+/u, trim: true)
    |> Enum.flat_map(&split_long_sentence/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp split_long_sentence(text) do
    text
    |> String.trim()
    |> String.graphemes()
    |> Enum.chunk_every(@max_sentence_chars)
    |> Enum.map(&Enum.join/1)
  end
end
