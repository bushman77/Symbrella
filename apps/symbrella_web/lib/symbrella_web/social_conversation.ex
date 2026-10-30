defmodule SymbrellaWeb.SocialConversation do
  @moduledoc """
  Evidence-constrained view of what a Colony thread discusses, what each
  sampled speaker expressed, and which relationships are actually supported.

  External wording is attribution, not accepted knowledge. Grammatical
  interpretations are shown only when the semantic probe supported them.
  Reply-parent links are structural; shared terms are not agreement or
  coreference. This module has no persistence or model calls.
  """

  @max_replies 8
  @max_interpretations_per_source 4
  @max_relations 20

  def build(topic, comments, segments, discourse)
      when is_map(topic) and is_list(comments) and is_list(segments) and is_map(discourse) do
    selected_comments = Enum.take(comments, @max_replies)
    interpretations = Map.get(discourse, :interpretations, [])

    sources =
      [%{label: "Original post", speaker: Map.get(topic, :author, "Unknown author")}] ++
        (selected_comments
         |> Enum.with_index(1)
         |> Enum.map(fn {comment, index} ->
           %{label: "Reply #{index}", speaker: Map.get(comment, :author, "Unknown author")}
         end))

    participants =
      Enum.map(sources, fn source ->
        supported =
          interpretations
          |> Enum.filter(fn interpretation ->
            source_label(Map.get(interpretation, :source_label)) == source.label and
              Map.get(interpretation, :speaker) == source.speaker
          end)
          |> Enum.take(@max_interpretations_per_source)
          |> Enum.map(fn interpretation ->
            %{
              speech_act: Map.get(interpretation, :speech_act),
              interpretation: Map.get(interpretation, :interpretation),
              roles: Map.get(interpretation, :roles, %{}),
              evidence: :semantic_probe,
              verification: :external_unverified
            }
          end)

        %{
          source_label: source.label,
          speaker: source.speaker,
          status: if(supported == [], do: :meaning_unresolved, else: :partial_interpretation),
          interpretations: supported
        }
      end)

    labels =
      selected_comments
      |> Enum.with_index(1)
      |> Enum.reduce(%{Map.get(topic, :id) => "Original post"}, fn {comment, index}, acc ->
        case Map.get(comment, :id) do
          id when is_binary(id) and id != "" -> Map.put(acc, id, "Reply #{index}")
          _ -> acc
        end
      end)

    reply_links =
      selected_comments
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {comment, index} ->
        parent_id = Map.get(comment, :parent_id)

        case Map.get(labels, parent_id) do
          nil -> []
          parent ->
            [%{
              from: "Reply #{index}",
              to: parent,
              kind: :explicit_reply_parent,
              status: :structural_only,
              detail: "The Colony supplied a parent comment identifier."
            }]
        end
      end)

    lexical_links =
      discourse
      |> Map.get(:lexical_links, [])
      |> Enum.flat_map(fn link ->
        from_index = Map.get(link, :from)
        to_index = Map.get(link, :to)
        from = if is_integer(from_index) and from_index >= 0, do: Enum.at(segments, from_index), else: nil
        to = if is_integer(to_index) and to_index >= 0, do: Enum.at(segments, to_index), else: nil

        if is_map(from) and is_map(to) do
          [%{
            from: Map.get(from, :label),
            to: Map.get(to, :label),
            kind: :shared_lexical_terms,
            status: :unverified_link,
            detail: "Shared words: " <> Enum.join(Map.get(link, :shared_terms, []), ", ")
          }]
        else
          []
        end
      end)

    sentence_order_links =
      segments
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.flat_map(fn [earlier, later] ->
        same_source? =
          source_label(Map.get(earlier, :label)) == source_label(Map.get(later, :label)) and
            Map.get(earlier, :author) == Map.get(later, :author)

        if same_source? do
          [%{
            from: Map.get(earlier, :label),
            to: Map.get(later, :label),
            kind: :same_source_sentence_order,
            status: :ordering_only,
            detail: "These sentences appear consecutively in the same author's source; no semantic relation is inferred."
          }]
        else
          []
        end
      end)

    topics =
      segments
      |> Enum.filter(fn segment ->
        source_label(Map.get(segment, :label)) == "Original post"
      end)
      |> Enum.flat_map(fn segment ->
        get_in(segment, [:result, :comprehension, :recognized_topics])
        |> List.wrap()
      end)
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.slice(&1, 0, 100))
      |> Enum.uniq()
      |> Enum.take(8)

    %{
      subject: %{
        author_title: Map.get(topic, :title, ""),
        status: if(hd(participants).status == :partial_interpretation,
          do: :partially_interpreted, else: :author_title_only),
        lexical_topic_candidates: topics,
        trust: :external_unverified
      },
      participants: participants,
      relations: Enum.take(reply_links ++ lexical_links ++ sentence_order_links, @max_relations),
      gaps: [
        "A title and recognized words do not prove the discussion's intended meaning.",
        "Reply linkage does not establish agreement, disagreement, correction, or causality.",
        "Unsampled replies and unsupported sentence interpretations remain unresolved."
      ],
      mode: :read_only_shadow
    }
  end

  defp source_label(value) when is_binary(value),
    do: value |> String.split(" · sentence", parts: 2) |> hd()

  defp source_label(_), do: ""
end
