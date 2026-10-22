defmodule Llm.Moltbook.Context do
  @moduledoc """
  Bounded, explicitly untrusted Moltbook input for future cognitive routing.

  This is a data normalization boundary, not a prompt generator. Social posts
  must not become system instructions, verified facts, or durable memories
  merely because they were retrieved successfully.
  """

  alias Llm.Moltbook.Adapter

  @title_chars 300
  @content_chars 2_000
  @identity_chars 128

  @doc "Normalize an API feed without granting its content authority."
  def from_feed(%{"success" => false}), do: {:error, :api_rejected}

  def from_feed(%{"posts" => posts}) when is_list(posts) do
    with {:ok, config} <- Adapter.config() do
      observations =
        posts
        |> Enum.filter(&is_map/1)
        |> Enum.take(config.max_posts)
        |> Enum.map(&normalize_post/1)

      {:ok,
       %{
         source: :moltbook,
         trust: :untrusted_external,
         posts: observations
       }}
    end
  end

  def from_feed(_other), do: {:error, :invalid_feed}

  defp normalize_post(post) do
    %{
      source: :moltbook,
      trust: :untrusted_external,
      id: text(Map.get(post, "id"), @identity_chars),
      title: text(Map.get(post, "title"), @title_chars),
      content: text(Map.get(post, "content"), @content_chars),
      author: author(Map.get(post, "author")),
      submolt: submolt(Map.get(post, "submolt"))
    }
  end

  defp author(%{"name" => name}), do: text(name, @identity_chars)
  defp author(%{"username" => name}), do: text(name, @identity_chars)
  defp author(name), do: text(name, @identity_chars)

  defp submolt(%{"name" => name}), do: text(name, @identity_chars)
  defp submolt(name), do: text(name, @identity_chars)

  defp text(value, max) when is_binary(value) do
    value |> String.trim() |> String.slice(0, max)
  end

  defp text(_value, _max), do: nil
end
