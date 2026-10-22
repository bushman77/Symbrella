defmodule Core.Social.ObservationBridge do
  @moduledoc """
  COLONY-02: explicitly invoked, read-only social cognitive evaluation.

  Consumes *normalized* Llm.Colony.Context observations and runs a bounded
  semantic shadow probe. It never invokes the live chat/response planner,
  sends a model prompt, posts to Colony, focuses working memory, records
  synaptic feedback, or encodes an external assertion as a verified fact.

  Hippocampus recall is scoped strictly to explicit Colony observations.
  Recall results are reported as counts only; no private episode is emitted.
  ACC is a lexical-uncertainty shadow; Thalamus arbitration is not activated.
  """

  alias Core.Social.SemanticProbe
  alias Llm.Colony.Client

  @max_posts 5
  @max_author_chars 128
  @max_title_chars 300
  @max_snippet_chars 160
  @max_id_chars 128

  @doc """
  Fetch the authenticated Colony feed and evaluate it locally. This function
  is on-demand only: it installs no timers or supervision children.
  """
  def observe(opts \\ [])

  def observe(opts) when is_list(opts) do
    with :ok <- validate_opts(opts),
         :ok <- validate_limit(Keyword.get(opts, :limit, @max_posts)),
         {:ok, feed} <- Client.observe(opts) do
      review(feed, opts)
    end
  end

  def observe(_), do: {:error, :invalid_options}

  @doc """
  Evaluate an already-normalized Colony feed, including fixtures without any
  network connection. Rejects untrusted-origin spoofing and malformed items.
  """
  def review(feed, opts \\ [])

  def review(%{source: :colony, trust: :untrusted_external, posts: posts}, opts)
      when is_list(posts) and is_list(opts) do
    with :ok <- validate_opts(opts),
         limit <- Keyword.get(opts, :limit, @max_posts),
         :ok <- validate_limit(limit),
         {:ok, valid_posts} <- sanitize_posts(posts, limit) do
      evaluator = Keyword.get(opts, :probe, &SemanticProbe.run/2)
      probe_opts = Keyword.get(opts, :probe_opts, [])

      evaluations =
        Enum.map(valid_posts, fn post ->
          evaluate(post, evaluator, probe_opts, opts)
        end)

      {:ok,
       %{
         version: 1,
         source: :colony,
         trust: :untrusted_external,
         mode: :read_only,
         evaluated_count: length(evaluations),
         evaluations: evaluations,
         memory_write: :forbidden,
         outbound_publish: :forbidden
       }}
    end
  end

  def review(_feed, _opts), do: {:error, :invalid_colony_observation}

  defp validate_opts(opts) do
    if Keyword.keyword?(opts), do: :ok, else: {:error, :invalid_options}
  end

  defp validate_limit(value) when is_integer(value) and value in 1..@max_posts, do: :ok
  defp validate_limit(_), do: {:error, :invalid_limit}

  defp sanitize_posts(posts, limit) do
    posts
    |> Enum.take(limit)
    |> Enum.reduce_while({:ok, {MapSet.new(), []}}, fn post, {:ok, {seen, acc}} ->
      with {:ok, clean} <- sanitize_post(post) do
        if MapSet.member?(seen, clean.id) do
          {:cont, {:ok, {seen, acc}}}
        else
          {:cont, {:ok, {MapSet.put(seen, clean.id), [clean | acc]}}}
        end
      else
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, {_ids, acc}} -> {:ok, Enum.reverse(acc)}
      error -> error
    end
  end

  defp sanitize_post(%{source: :colony, trust: :untrusted_external} = post) do
    id = text(Map.get(post, :id), @max_id_chars)
    title = text(Map.get(post, :title), @max_title_chars)
    body = text(Map.get(post, :body), @max_snippet_chars)

    if id == "" or (title == "" and body == "") do
      {:error, :invalid_colony_post}
    else
      {:ok,
       %{
         id: id,
         author: text(Map.get(post, :author), @max_author_chars),
         colony: text(Map.get(post, :colony), @max_author_chars),
         title: title,
         snippet: body
       }}
    end
  end

  defp sanitize_post(_), do: {:error, :invalid_colony_post}

  defp evaluate(post, evaluator, probe_opts, opts) do
    excerpt =
      case {post.title, post.snippet} do
        {"", body} -> body
        {title, ""} -> title
        {title, body} -> title <> ". " <> body
      end
      |> String.slice(0, 420)

    semantic =
      case evaluator.(excerpt, probe_opts) do
        {:ok, %{} = data} -> data
        {:error, reason} -> %{stage: :social_semantic_shadow, status: :unavailable, reason: reason}
        _ -> %{stage: :social_semantic_shadow, status: :unavailable}
      end

    %{
      post_id: post.id,
      author: post.author,
      colony: post.colony,
      title: post.title,
      source: :colony,
      trust: :untrusted_external,
      semantic: semantic,
      hippocampus: scoped_recall(post, opts),
      thalamus: %{status: :not_submitted, reason: :read_only_external_observation},
      response: :not_generated,
      factual_admission: :blocked
    }
  end

  defp scoped_recall(post, opts) do
    recall = Keyword.get(opts, :recall, &safe_scoped_recall/1)

    case recall.(post.title) do
      {:ok, count} when is_integer(count) and count >= 0 ->
        %{status: :read_only, scope: :colony_external_only, matches: min(count, 2)}

      _ ->
        %{status: :unavailable, scope: :colony_external_only, matches: 0}
    end
  end

  defp safe_scoped_recall(title) do
    if Process.whereis(Brain.Hippocampus) do
      cues =
        title
        |> String.downcase()
        |> String.split(~r/[^[:alnum:]]+/u, trim: true)
        |> Enum.filter(&(String.length(&1) > 3))
        |> Enum.uniq()
        |> Enum.take(10)

      if cues == [] do
        {:ok, 0}
      else
        episodes =
          Brain.Hippocampus.recall(cues,
            recall_source: :memory,
            scope: %{source: :colony_external, trust: :untrusted_external},
            min_jaccard: 0.20,
            limit: 2,
            ignore_head: :never
          )

        {:ok, length(episodes)}
      end
    else
      {:error, :hippocampus_unavailable}
    end
  rescue
    _ -> {:error, :hippocampus_unavailable}
  catch
    :exit, _ -> {:error, :hippocampus_unavailable}
  end

  defp text(value, max) when is_binary(value) do
    value |> String.trim() |> String.slice(0, max)
  end

  defp text(_, _), do: ""
end
