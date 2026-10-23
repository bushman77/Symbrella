defmodule SymbrellaWeb.SocialData do
  @moduledoc """
  Read-only projection of the existing Colony API for the protected web UI.

  Pages contain at most 25 *posts* and use the API's newest/offset ordering.
  One selected post is read through /posts/:id/context. No LLM, draft,
  memory, database, or Colony write operation is reachable from this module.
  All external text remains untrusted display data.
  """

  alias Llm.Colony.{Client, Context}

  @page_size 25
  @max_page 4_001
  @max_comments 200
  @post_id ~r/\A[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\z/

  def page_size, do: @page_size

  @doc "Read one page of up to 25 conversations, newest first."
  def list_page(page, opts \\ [])

  def list_page(page, opts)
      when is_integer(page) and page in 1..@max_page and is_list(opts) do
    client = Keyword.get(opts, :client, configured_client())

    with {:ok, payload} <- client.posts(limit: @page_size, offset: (page - 1) * @page_size, sort: :newest),
         {:ok, %{posts: posts}} <- Context.from_feed(payload, max_posts: @page_size) do
      topics =
        posts
        |> Enum.filter(fn post -> valid_id?(Map.get(post, :id)) end)
        |> Enum.map(fn post ->
          %{post |
            title: display_title(post),
            author: display_author(post.author),
            body: display_text(post.body, 2_000)}
        end)

      {:ok,
       %{
         topics: topics,
         page: page,
         has_next?: next_page?(payload, page, length(raw_items(payload))),
         source: :colony,
         trust: :untrusted_external
       }}
    end
  end

  def list_page(_, _), do: {:error, :invalid_social_page}

  @doc "Read a selected post and the comments returned by its context endpoint."
  def thread(post_id, opts \\ [])

  def thread(post_id, opts) when is_binary(post_id) and is_list(opts) do
    if valid_id?(post_id) do
      client = Keyword.get(opts, :client, configured_client())

      with {:ok, payload} <- client.thread_context(post_id),
           {:ok, topic} <- extract_post(payload, post_id) do
        comments = payload |> Map.get("comments", []) |> unwrap_comments()
        visible = Enum.take(comments, @max_comments)

        {:ok,
         %{
           topic: topic,
           comments: Enum.map(visible, &normalize_comment/1),
           comments_truncated?: length(comments) > @max_comments,
           source: :colony,
           trust: :untrusted_external
         }}
      end
    else
      {:error, :invalid_post_id}
    end
  end

  def thread(_, _), do: {:error, :invalid_post_id}

  def valid_id?(id) when is_binary(id), do: Regex.match?(@post_id, id)
  def valid_id?(_), do: false

  defp configured_client,
    do: Application.get_env(:symbrella_web, :social_client, Client)

  defp raw_items(%{"items" => items}) when is_list(items), do: items
  defp raw_items(%{"posts" => posts}) when is_list(posts), do: posts
  defp raw_items(_), do: []

  defp next_page?(payload, page, count) when is_map(payload) do
    cond do
      page >= @max_page -> false
      is_boolean(payload["has_more"]) -> payload["has_more"]
      is_integer(payload["total"]) -> page * @page_size < payload["total"]
      true -> count >= @page_size
    end
  end

  defp extract_post(payload, post_id) when is_map(payload) do
    post =
      case Map.get(payload, "post") do
        %{} = nested -> nested
        _ -> payload
      end

    if Map.get(post, "id") == post_id do
      {:ok,
       %{
         id: post_id,
         title: display_text(Map.get(post, "title"), 300) |> fallback_title(post),
         body: display_text(Map.get(post, "body"), 20_000),
         author: author_from(post),
         colony: colony_from(post),
         created_at: display_text(Map.get(post, "created_at"), 100),
         reply_count: Map.get(post, "comment_count") || Map.get(post, "comments_count")
       }}
    else
      {:error, :colony_thread_identity_mismatch}
    end
  end

  defp extract_post(_, _), do: {:error, :invalid_colony_context}

  defp normalize_comment(comment) when is_map(comment) do
    %{
      id: display_text(Map.get(comment, "id"), 128),
      author: author_from(comment),
      body: display_text(Map.get(comment, "body"), 8_000),
      created_at: display_text(Map.get(comment, "created_at"), 100),
      parent_id: display_text(Map.get(comment, "parent_id") || Map.get(comment, "parent_comment_id"), 128)
    }
  end

  defp normalize_comment(_), do: %{id: nil, author: "Unknown author", body: "", created_at: "", parent_id: ""}

  defp unwrap_comments(comments) when is_list(comments), do: Enum.filter(comments, &is_map/1)
  defp unwrap_comments(%{"items" => items}) when is_list(items), do: unwrap_comments(items)
  defp unwrap_comments(%{"comments" => items}) when is_list(items), do: unwrap_comments(items)
  defp unwrap_comments(_), do: []

  defp display_title(%{title: title, body: body}) do
    case display_text(title, 300) do
      "" -> fallback_title("", %{"body" => body})
      text -> text
    end
  end

  defp fallback_title("", post) do
    case display_text(Map.get(post, "body"), 110) do
      "" -> "(untitled topic)"
      excerpt -> excerpt
    end
  end

  defp fallback_title(title, _post), do: title

  defp display_author(author) when is_binary(author) and author != "", do: author
  defp display_author(_), do: "Unknown author"

  defp author_from(post) do
    candidate =
      Map.get(post, "author_username") ||
        Map.get(post, "author") ||
        Map.get(post, "author_name")

    name =
      case candidate do
        %{"username" => username} -> username
        %{"name" => name} -> name
        %{"display_name" => name} -> name
        name when is_binary(name) -> name
        _ -> nil
      end

    name |> display_text(128) |> display_author()
  end

  defp colony_from(%{"colony" => %{"name" => name}}), do: display_text(name, 128)
  defp colony_from(%{"colony" => name}) when is_binary(name), do: display_text(name, 128)
  defp colony_from(post), do: display_text(Map.get(post, "colony_name"), 128)

  defp display_text(value, max) when is_binary(value) do
    value |> String.trim() |> String.slice(0, max)
  end

  defp display_text(_, _), do: ""
end
