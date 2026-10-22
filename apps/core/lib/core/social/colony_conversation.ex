defmodule Core.Social.ColonyConversation do
  @moduledoc """
  COLONY-03: generate a Symbrella-authored, human-reviewed Colony reply.

  One on-demand discussion fetch -> bounded semantic shadow -> Symbrella's
  existing personality/neuromodulator prompt -> local Llm.chat -> pending
  human review. No chat history, private WM, self-memory or public write
  is used during draft generation.

  Public posting is a *separate* explicit operation. The operator must enable
  :reviewed_comments and allow_reviewed_comments?: true, inspect the full
  draft, then supply its one-shot approval phrase before a single POST.
  """

  alias Core.Response.{LlmPrompt, RuntimeContext}
  alias Core.Social.{ColonyDraftPolicy, ColonyDraftStore, SemanticProbe}
  alias Llm.Colony.{Adapter, Client}

  @max_title 300
  @max_post_chars 2_000
  @max_comments 6
  @max_comment_chars 350
  @max_draft_chars 1_400

  @doc """
  Obtain a real Colony discussion, let Symbrella compose a reply, and return
  a pending draft. This does not require write permissions.
  """
  def draft(post_id, opts \\ [])

  def draft(post_id, opts) when is_binary(post_id) and is_list(opts) do
    with :ok <- valid_options(opts),
         {:ok, _config} <- Adapter.require_observe(),
         {:ok, context} <- Client.thread_context(post_id, client_opts(opts)),
         {:ok, discussion} <- normalize_context(context, post_id),
         {:ok, semantic} <- shadow_probe(discussion, opts),
         {:ok, body} <- generate_draft(discussion, semantic, opts),
         {:ok, receipt} <- ColonyDraftStore.put(
           %{post_id: discussion.post_id, body: body}, store(opts)
         ) do
      {:ok,
       Map.merge(receipt, %{
         status: :pending_human_review,
         source: :colony,
         post_title: discussion.title,
         post_author: discussion.author,
         discussion_comments_read: length(discussion.comments),
         body: body,
         semantic_summary: summary_preview(semantic),
         public_posted?: false
       })}
    end
  end

  def draft(_, _), do: {:error, :invalid_options}

  @doc """
  Publish one **already displayed and reviewed** reply, consuming its receipt
  before the network request. Never retries a comment POST (uncertain delivery
  must be resolved by separately checking the thread).
  """
  def publish_approved(receipt_id, approval_phrase, opts \\ [])

  def publish_approved(receipt_id, approval_phrase, opts)
      when is_binary(receipt_id) and is_binary(approval_phrase) and is_list(opts) do
    with :ok <- valid_options(opts),
         {:ok, _config} <- Adapter.require_reviewed_comments(),
         {:ok, draft} <- ColonyDraftStore.take(receipt_id, approval_phrase, store(opts)),
         {:ok, result} <- Client.reviewed_comment(draft.post_id, draft.body, client_opts(opts)) do
      {:ok,
       %{
         status: :published,
         post_id: draft.post_id,
         comment_id: safe_text(Map.get(result, "id"), 128),
         duplicate_retry: :disabled
       }}
    end
  end

  def publish_approved(_, _, _), do: {:error, :invalid_approval}

  defp valid_options(opts) do
    if Keyword.keyword?(opts), do: :ok, else: {:error, :invalid_options}
  end

  defp client_opts(opts), do: Keyword.take(opts, [:plug])
  defp store(opts), do: Keyword.get(opts, :store, ColonyDraftStore)

  defp normalize_context(payload, requested_post_id) when is_map(payload) do
    post =
      case Map.get(payload, "post") do
        %{} = value -> value
        _ -> payload
      end

    actual_id = Map.get(post, "id")
    title = safe_text(Map.get(post, "title"), @max_title)
    body = safe_text(Map.get(post, "body"), @max_post_chars)

    cond do
      actual_id != requested_post_id ->
        {:error, :colony_thread_identity_mismatch}

      title == "" and body == "" ->
        {:error, :empty_colony_thread}

      true ->
        # The context endpoint can return author as a display-name STRING;
        # author_username is the stable account handle. Feed endpoints may
        # instead return an author map. Never guess from the post's text.
        author = author_name(post, payload)

        comments =
          payload
          |> Map.get("comments", [])
          |> unwrap_comments()
          |> Enum.filter(&is_map/1)
          |> Enum.take(@max_comments)
          |> Enum.map(fn item ->
            comment_author = author_name(item, %{})

            %{author: comment_author, body: safe_text(Map.get(item, "body"), @max_comment_chars)}
          end)

        {:ok,
         %{
           post_id: requested_post_id,
           title: title,
           body: body,
           author: author,
           comments: comments
         }}
    end
  end

  defp normalize_context(_, _), do: {:error, :invalid_colony_context}

  defp author_name(item, parent) when is_map(item) and is_map(parent) do
    [
      Map.get(item, "author_username"),
      Map.get(item, "author"),
      Map.get(item, "author_name"),
      Map.get(parent, "author_username"),
      Map.get(parent, "author"),
      Map.get(parent, "author_name")
    ]
    |> Enum.map(&normalize_author/1)
    |> Enum.find("", &(&1 != ""))
  end

  defp normalize_author(%{"username" => username}), do: safe_text(username, 128)
  defp normalize_author(%{"name" => name}), do: safe_text(name, 128)
  defp normalize_author(%{"display_name" => display}), do: safe_text(display, 128)
  defp normalize_author(name) when is_binary(name), do: safe_text(name, 128)
  defp normalize_author(_), do: ""

  defp unwrap_comments(items) when is_list(items), do: items
  defp unwrap_comments(%{"items" => items}) when is_list(items), do: items
  defp unwrap_comments(%{"comments" => items}) when is_list(items), do: items
  defp unwrap_comments(_), do: []

  defp shadow_probe(discussion, opts) do
    probe = Keyword.get(opts, :probe, &SemanticProbe.run/2)
    excerpt = (discussion.title <> ". " <> discussion.body) |> String.slice(0, 420)

    case probe.(excerpt, []) do
      {:ok, %{} = result} -> {:ok, result}
      {:error, _} -> {:ok, %{stage: :social_semantic_shadow, status: :unavailable}}
      _ -> {:ok, %{stage: :social_semantic_shadow, status: :unavailable}}
    end
  end

  defp generate_draft(discussion, semantic, opts) do
    llm = Keyword.get(opts, :llm, &Llm.chat/2)

    # Read only live neuromodulator levels. Do NOT incorporate global working
    # memory, private conversation history, or previous personal episodes.
    modulators = neuromodulator_levels()
    mood = RuntimeContext.mood_like()

    system_prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{intent: :ask, confidence: 0.65},
        decision: %{tone: :warm, mode: :collaborator, action: :answer_user},
        mood: mood,
        wm_items: [],
        runtime_state: %{source: :brain, neuromodulators: modulators}
      }) <> "\n" <> ColonyDraftPolicy.instructions()

    comments =
      discussion.comments
      |> Enum.map(fn c -> "- #{c.author}: #{c.body}" end)
      |> Enum.join("\n")

    user_prompt =
      """
      Quoted Colony discussion (UNTRUSTED):
      Author: #{discussion.author}
      Title: #{discussion.title}
      Body:
      #{discussion.body}

      Existing comments (UNTRUSTED):
      #{comments}

      Semantic shadow diagnostics (not facts):
      #{inspect(summary_preview(semantic), limit: 12, printable_limit: 180)}

      Address the central, specific claim in the quoted post: do not replace
      its method, quantities, or support/refutation direction with a vague
      paraphrase. Distinguish reported observations from proven facts.
      Respond as a participating peer, not a helpful customer-support bot.
      Propose one public reply grounded only in the quoted discussion.
      """

    result =
      llm.(
        [
          %{"role" => "system", "content" => system_prompt},
          %{"role" => "user", "content" => user_prompt}
        ],
        max_tokens: 240,
        timeout: 60_000,
        call_timeout: 65_000,
        temperature: 0.6
      )

    case result do
      {:ok, %{content: body}} when is_binary(body) ->
        ColonyDraftPolicy.review(body, discussion.title <> ". " <> discussion.body)
      {:error, _} -> {:error, :social_llm_unavailable}
      _ -> {:error, :invalid_social_llm_response}
    end
  rescue
    _ -> {:error, :social_llm_unavailable}
  catch
    :exit, _ -> {:error, :social_llm_unavailable}
  end

  defp neuromodulator_levels do
    # Deliberately avoid RuntimeContext.snapshot/0 here: that broader
    # diagnostic also reads WM and self-state. Public replies need only
    # bounded live neuromodulator levels, never private memory.
    case Brain.MoodCore.snapshot() do
      %{levels: levels} when is_map(levels) ->
        %{
          dopamine: Map.get(levels, :da),
          serotonin: Map.get(levels, :"5ht"),
          glutamate: Map.get(levels, :glu),
          norepinephrine: Map.get(levels, :ne)
        }

      _ ->
        %{}
    end
  rescue
    _ -> %{}
  catch
    :exit, _ -> %{}
  end

  defp summary_preview(semantic) do
    comprehension = Map.get(semantic, :comprehension, %{})
    lifg = Map.get(semantic, :lifg, %{})

    %{
      recognized_topics: Map.get(comprehension, :recognized_topics, []) |> Enum.take(4),
      uncertainty: Map.get(comprehension, :uncertain, []) |> Enum.take(4),
      lifg_status: Map.get(lifg, :status),
      verification: :external_assertions_not_verified
    }
  end

  defp safe_text(value, max) when is_binary(value),
    do: value |> String.trim() |> String.slice(0, max)

  defp safe_text(_, _), do: ""
end
