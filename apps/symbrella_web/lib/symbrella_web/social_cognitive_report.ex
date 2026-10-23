defmodule SymbrellaWeb.SocialCognitiveReport do
  @moduledoc """
  Bounded, on-demand, read-only inspection of external Colony text.

  Reuses the existing Core.Social.SemanticProbe, not the stateful live
  conversation pipeline. No persistence, LLM generation or public posting.
  Each sampled segment receives its own report and retains attribution.
  """
  alias Core.Social.SemanticProbe

  @max_replies 3

  def analyze(%{topic: %{id: id} = topic, comments: comments}, opts \\ [])
      when is_binary(id) and is_list(comments) do
    probe = Keyword.get(opts, :probe, &SemanticProbe.run/2)

    samples =
      [%{label: "Original post", author: topic.author,
         text: sample_text(topic.title, topic.body)}] ++
        (comments
         |> Enum.take(@max_replies)
         |> Enum.with_index(1)
         |> Enum.map(fn {comment, index} ->
           %{label: "Reply #{index}", author: comment.author,
             text: String.slice(comment.body || "", 0, 420)}
         end))

    analyses =
      Enum.map(samples, fn sample ->
        outcome =
          try do
            probe.(sample.text, emit_telemetry?: false)
          rescue
            _ -> {:error, :probe_unavailable}
          catch
            :exit, _ -> {:error, :probe_unavailable}
          end

        Map.merge(sample, case outcome do
          {:ok, %{stage: :social_semantic_shadow} = result} ->
            %{status: :analyzed, result: result}
          {:error, reason} ->
            %{status: :unavailable, reason: reason}
          _ ->
            %{status: :unavailable, reason: :invalid_probe_response}
        end)
        |> Map.delete(:text)
      end)

    {:ok, %{
      post_id: id,
      source: :colony_external,
      trust: :untrusted_external,
      mode: :read_only_shadow,
      analyzed_segments: length(analyses),
      omitted_replies: max(length(comments) - @max_replies, 0),
      segments: analyses
    }}
  end

  def analyze(_, _), do: {:error, :invalid_social_analysis_input}

  defp sample_text(title, body) do
    text = [title, body] |> Enum.filter(&is_binary/1) |> Enum.join(". ")
    String.slice(text, 0, 420)
  end
end
