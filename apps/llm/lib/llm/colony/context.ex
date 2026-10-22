defmodule Llm.Colony.Context do
  @moduledoc """
  Bounded normalization for untrusted Colony posts.

  The resulting maps are observations, not instructions or verified facts.
  In particular, no retrieved text is placed in a system prompt or stored
  as authoritative knowledge at this stage.
  """

  @title_chars 300
  @body_chars 2_000
  @identity_chars 128
  @reason_chars 240

  @doc "Normalize an /posts or /feed/for-you response."
  def from_feed(payload, opts \\ [])

  def from_feed(%{"items" => items} = payload, opts) when is_list(items) do
    normalize(items, payload, opts)
  end

  def from_feed(%{"posts" => posts} = payload, opts) when is_list(posts) do
    normalize(posts, payload, opts)
  end

  def from_feed(_payload, _opts), do: {:error, :invalid_colony_feed}

  defp normalize(items, payload, opts) do
    max_posts = Keyword.get(opts, :max_posts, 10)

    if is_integer(max_posts) and max_posts in 1..50 do
      posts =
        items
        |> Enum.filter(&post_item?/1)
        |> Enum.take(max_posts)
        |> Enum.map(&normalize_item/1)

      {:ok,
       %{
         source: :colony,
         trust: :untrusted_external,
         posts: posts,
         next_cursor: short_text(Map.get(payload, "next_cursor"), 1024)
       }}
    else
      {:error, :invalid_limit}
    end
  end

  defp post_item?(%{"kind" => "comment"}), do: false
  defp post_item?(%{"kind" => "post", "post" => post}), do: is_map(post)
  defp post_item?(%{"post" => post}), do: is_map(post)
  defp post_item?(%{"id" => _id}), do: true
  defp post_item?(_), do: false

  defp normalize_item(item) do
    # For-you feed items may wrap a post; /posts returns posts directly.
    post =
      case Map.get(item, "post") do
        nested when is_map(nested) -> nested
        _ -> item
      end

    %{
      source: :colony,
      trust: :untrusted_external,
      id: short_text(Map.get(post, "id"), @identity_chars),
      title: short_text(Map.get(post, "title"), @title_chars),
      body: short_text(Map.get(post, "body"), @body_chars),
      post_type: short_text(Map.get(post, "post_type"), @identity_chars),
      author: author(Map.get(post, "author")),
      colony: colony(post),
      created_at: short_text(Map.get(post, "created_at"), @identity_chars),
      relevance_reason: short_text(Map.get(item, "reason"), @reason_chars)
    }
  end

  defp author(%{"username" => username}), do: short_text(username, @identity_chars)
  defp author(%{"name" => name}), do: short_text(name, @identity_chars)
  defp author(_), do: nil

  defp colony(%{"colony" => %{"name" => name}}), do: short_text(name, @identity_chars)
  defp colony(%{"colony" => name}) when is_binary(name), do: short_text(name, @identity_chars)
  defp colony(%{"colony_name" => name}), do: short_text(name, @identity_chars)
  defp colony(%{"colony_id" => id}), do: short_text(id, @identity_chars)
  defp colony(_), do: nil

  defp short_text(value, max) when is_binary(value),
    do: value |> String.trim() |> String.slice(0, max)

  defp short_text(_value, _max), do: nil
end
