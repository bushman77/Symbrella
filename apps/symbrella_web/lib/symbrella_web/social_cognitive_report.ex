defmodule SymbrellaWeb.SocialCognitiveReport do
  @moduledoc """
  Read-only, bounded sentence-by-sentence Colony cognition inspection.
  Does not call the stateful conversation pipeline or persist external text.
  An operator-initiated invocation may request bounded transient Brain.Cell
  activation for lexically attested senses, without memory or synaptic writes.
  """

  alias Core.Social.SemanticProbe
  alias SymbrellaWeb.{SocialUnderstanding, SocialDiscourse, SocialConversation}

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
      [%{label: "Original post", source_type: :original_post, source_index: 0,
         author: topic.author, text: join_text(topic.title, topic.body)}] ++
        (comments
         |> Enum.take(@max_replies)
         |> Enum.with_index(1)
         |> Enum.map(fn {comment, index} ->
           %{label: "Reply #{index}", source_type: :reply, source_index: index,
             author: comment.author, text: comment.body || ""}
         end))

    {samples, dropped_by_source} =
      Enum.reduce(sources, {[], 0}, fn source, {acc, dropped} ->
        sentences = split_sentences(source.text)
        kept = Enum.take(sentences, @max_sentences_per_source)

        next =
          kept
          |> Enum.with_index(1)
          |> Enum.map(fn {sentence, index} ->
            %{label: "#{source.label} · sentence #{index}",
              source_label: source.label, source_type: source.source_type,
              source_index: source.source_index, author: source.author, text: sentence}
          end)

        {acc ++ next, dropped + max(length(sentences) - length(kept), 0)}
      end)

    selected = Enum.take(samples, @max_total_sentences)

    analyses =
      Enum.map(selected, fn sample ->
        outcome =
          try do
            probe.(sample.text,
              emit_telemetry?: false,
              brain_observation?: Keyword.get(opts, :brain_observation?, false)
            )
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

    # Preserve the original source boundary throughout cognition. A reply
    # never inherits the post's meaning merely because it follows the post.
    analyses =
      analyses
      |> Enum.with_index()
      |> Enum.map(fn {sample, index} -> Map.put(sample, :sample_index, index) end)

    original_segments = Enum.filter(analyses, &(&1.source_type == :original_post))

    reply_sources =
      sources
      |> Enum.filter(&(&1.source_type == :reply))
      |> Enum.map(fn source ->
        own = Enum.filter(analyses, fn segment ->
          segment.source_type == :reply and segment.source_index == source.source_index
        end)

        %{
          kind: :reply, index: source.source_index, label: source.label,
          author: source.author, segments: own,
          understanding: SocialUnderstanding.build(own)
        }
      end)

    sections = %{
      original_post: %{
        kind: :original_post, label: "Original post",
        author: topic.author, segments: original_segments,
        understanding: SocialUnderstanding.build(original_segments)
      },
      replies: reply_sources
    }

    discourse = SocialDiscourse.build(analyses)

    {:ok, %{
      post_id: id,
      source: :colony_external,
      trust: :untrusted_external,
      mode: :read_only_shadow,
      analyzed_segments: length(analyses),
      omitted_replies: max(length(comments) - @max_replies, 0),
      omitted_sentences: dropped_by_source + length(samples) - length(selected),
      segments: analyses,
      sections: sections,
      understanding: SocialUnderstanding.build(analyses),
      discourse: discourse,
      conversation: SocialConversation.build(topic, comments, analyses, discourse)
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
