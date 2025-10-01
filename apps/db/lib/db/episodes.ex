defmodule Db.Episodes do
  @moduledoc """
  Convenience helpers for creating and querying hippocampus episodes.

  Responsibilities:
  • Normalize + insert episodes (sync or async embedding)
  • Hybrid recall (token prefilter + vector ANN) with re‑rank
  • Utilities for embedding text and patching embeddings
  """

  import Ecto.Query, only: [from: 2]
  alias Db.Episode

  @type id :: term()
  @type cues :: [String.t()]
  @type embedding :: [number()]
  @type recall_item :: %{
          id: id(),
          episode: Episode.t(),
          jaccard: float(),
          vec_sim: float() | nil,
          recency: float(),
          age_seconds: integer(),
          score: float()
        }

  # Configuration
  @embedding_dim Application.compile_env(:db, :embedding_dim, 1536)

  @defaults [
    token_limit: 200,
    vec_limit: 100,
    top_k: 10,
    half_life: 86_400, # seconds
    alpha: 0.7,        # jaccard weight
    beta: 0.2,         # vector weight
    gamma: 0.1         # recency weight
  ]

  # ---------------------------------------------------------------------------
  # Public API — Writes
  # ---------------------------------------------------------------------------

  @doc """
  Insert an episode. By default, computes embedding synchronously.

  Options:
    * :user_id — UUID for owner (optional)
    * :tags    — list of strings (optional)
    * :async_embedding — if true, inserts without embedding; patch later
  """
  @spec write_episode(map(), keyword()) :: {:ok, Episode.t()} | {:error, term()}
  def write_episode(si, opts \\ [])

  # Primary clause: SI must include :tokens list (strings or token maps)
  def write_episode(%{tokens: tokens} = si, opts) when is_list(tokens) do
    tokens_norm = normalize_tokens(tokens)

    attrs = new_episode_attrs(si, tokens_norm, opts)

    if Keyword.get(opts, :async_embedding, false) do
      %Episode{}
      |> Episode.changeset(attrs)
      |> Db.insert()
    else
      text = si_text_for_embedding(si)

      with {:ok, emb} <- safe_embed(text) do
        %Episode{}
        |> Episode.changeset(Map.put(attrs, :embedding, emb))
        |> Db.insert()
      end
    end
  end

  def write_episode(_other, _opts), do: {:error, :invalid_si}

  @doc "Patch the embedding for a stored episode by id."
  @spec update_embedding(id(), embedding()) :: {:ok, Episode.t()} | {:error, Ecto.Changeset.t()}
  def update_embedding(id, emb) when is_list(emb) do
    case Db.get(Episode, id) do
      nil -> {:error, :not_found}
      ep -> ep |> Ecto.Changeset.change(%{embedding: emb}) |> Db.update()
    end
  end

  # ---------------------------------------------------------------------------
  # Public API — Recall
  # ---------------------------------------------------------------------------

  @doc """
  Hybrid recall with re‑rank.

  * `cues` — normalized token list
  * `query` — an embedding list, a query string, or nil
  * Options: token_limit, vec_limit, top_k, half_life, alpha, beta, gamma
  """
  @spec recall_hybrid(cues(), embedding() | String.t() | nil, keyword()) :: [recall_item()]
  def recall_hybrid(cues, query \\ nil, opts \\ [])

  # Clause A: list (embedding) or nil
  def recall_hybrid(cues, query_emb, opts)
      when is_list(cues) and (is_list(query_emb) or is_nil(query_emb)) do
    opts = Keyword.merge(@defaults, opts)

    token_limit = opts[:token_limit]
    vec_limit   = opts[:vec_limit]
    top_k       = opts[:top_k]
    half_life   = opts[:half_life]
    alpha       = opts[:alpha]
    beta        = opts[:beta]
    gamma       = opts[:gamma]

    now = NaiveDateTime.utc_now()

    # 1) Token prefilter (fast GIN &&)
    token_q =
      from(e in Episode,
        where: fragment("? && ?", e.tokens, ^cues),
        limit: ^token_limit,
        select: e
      )

    token_candidates = Db.all(token_q)

    # 2) Vector nearest neighbors (kNN) — optional branch
    vector_candidates =
      if is_list(query_emb) and query_emb != [] do
        vec_val = Pgvector.new(query_emb)

        vec_q =
          from(e in Episode,
            where: not is_nil(e.embedding),
            order_by: fragment("? <-> ?", e.embedding, ^vec_val),
            limit: ^vec_limit,
            select: %{ep: e, dist: fragment("? <-> ?", e.embedding, ^vec_val)}
          )

        Db.all(vec_q)
        |> Enum.map(fn %{ep: e, dist: dist} -> {e, dist} end)
      else
        []
      end

    # 3) Merge candidate sets
    vec_sim_map =
      vector_candidates
      |> Enum.into(%{}, fn {ep, dist} -> {ep.id, {ep, 1.0 / (1.0 + (dist || 0.0))}} end)

    token_list = Enum.map(token_candidates, &{&1.id, &1})
    vec_list   = vec_sim_map |> Map.values() |> Enum.map(fn {ep, _sim} -> {ep.id, ep} end)

    all_cands =
      (token_list ++ vec_list)
      |> Enum.uniq_by(fn {id, _} -> id end)
      |> Enum.into(%{}, fn {id, ep} -> {id, ep} end)

    # 4) Score + re‑rank
    all_cands
    |> Enum.map(fn {_id, ep} ->
      ep_tokens_set = MapSet.new((ep.tokens || []) |> Enum.map(&String.downcase/1))
      cue_set       = MapSet.new(cues)

      inter   = MapSet.intersection(ep_tokens_set, cue_set) |> MapSet.size()
      union   = MapSet.union(ep_tokens_set, cue_set) |> MapSet.size()
      jaccard = if union == 0, do: 0.0, else: inter / union

      vec_sim =
        case Map.get(vec_sim_map, ep.id) do
          nil -> compute_emb_sim_fallback(ep.embedding, query_emb)
          {_ep, sim} -> sim
        end

      age_s = case ep.inserted_at do
        %NaiveDateTime{} = t -> NaiveDateTime.diff(now, t, :second)
        _ -> 0
      end

      recency = :math.pow(2.0, -age_s / max(1, half_life))
      score   = alpha * jaccard + beta * (vec_sim || 0.0) + gamma * recency

      %{
        id: ep.id,
        episode: ep,
        jaccard: jaccard,
        vec_sim: vec_sim,
        recency: recency,
        age_seconds: age_s,
        score: score
      }
    end)
    |> Enum.sort_by(& &1.score, :desc)
    |> Enum.take(top_k)
  end

  # Clause B: string — embed internally, then delegate to Clause A
  def recall_hybrid(cues, query_text, opts)
      when is_list(cues) and is_binary(query_text) do
    case embedder_module().embed(query_text) do
      {:ok, emb} -> recall_hybrid(cues, emb, opts)
      _ -> recall_hybrid(cues, nil, opts)
    end
  end

  # ---------------------------------------------------------------------------
  # Utilities (private)
  # ---------------------------------------------------------------------------

  # Read embedder at runtime so tests can swap it via Application.put_env/3
  defp embedder_module, do: Application.get_env(:db, :embedder, MyEmbeddings)

  @spec safe_embed(String.t()) :: {:ok, embedding()} | {:error, term()}
  defp safe_embed(text) when is_binary(text) do
    try do
      case embedder_module().embed(text) do
        {:ok, emb} when is_list(emb) and length(emb) == @embedding_dim -> {:ok, emb}
        {:ok, emb} -> {:error, {:wrong_embedding_size, length(emb)}}
        other -> {:error, {:embedder_error, other}}
      end
    rescue
      e -> {:error, {:embedder_exception, Exception.message(e)}}
    end
  end

  defp safe_embed(_), do: {:error, :invalid_text}

  @doc "Build the text we feed to the embedder from an SI map."
  @spec si_text_for_embedding(map()) :: String.t()
  def si_text_for_embedding(si) when is_map(si) do
    token_text =
      si
      |> Map.get(:tokens, [])
      |> Enum.map(&token_to_text/1)
      |> Enum.join(" ")

    [
      Map.get(si, :intent, ""),
      Map.get(si, :keyword, ""),
      Map.get(si, :sentence, ""),
      token_text
    ]
    |> Enum.reject(&(&1 == nil or &1 == ""))
    |> Enum.join("\n")
  end

  # Accept string tokens or token maps with :phrase / :norm keys
  @spec token_to_text(term()) :: String.t()
  defp token_to_text(t) when is_binary(t), do: t
  defp token_to_text(%{} = t) do
    cond do
      Map.has_key?(t, :phrase) -> to_string(Map.get(t, :phrase))
      Map.has_key?(t, "phrase") -> to_string(Map.get(t, "phrase"))
      Map.has_key?(t, :norm) -> to_string(Map.get(t, :norm))
      Map.has_key?(t, "norm") -> to_string(Map.get(t, "norm"))
      true -> to_string(t)
    end
  end
  defp token_to_text(other), do: to_string(other)

  @spec normalize_tokens([term()]) :: [String.t()]
  defp normalize_tokens(tokens) do
    tokens |> Enum.map(&token_to_text/1) |> Enum.map(&String.downcase/1)
  end

  @spec new_episode_attrs(map(), [String.t()], keyword()) :: map()
  defp new_episode_attrs(si, tokens_norm, opts) do
    %{
      user_id: Keyword.get(opts, :user_id),
      tokens: tokens_norm,
      token_count: length(tokens_norm),
      si: si,
      tags: Keyword.get(opts, :tags, [])
    }
  end

  # Fallback local cosine similarity when DB distance not available
  @spec compute_emb_sim_fallback(term(), embedding() | nil) :: float() | nil
  defp compute_emb_sim_fallback(_, query) when is_binary(query), do: nil
  defp compute_emb_sim_fallback(nil, _), do: nil
  defp compute_emb_sim_fallback(_, nil), do: nil
  defp compute_emb_sim_fallback(%Pgvector{} = p, query_emb) when is_list(query_emb) do
    compute_emb_sim_fallback(Pgvector.to_list(p), query_emb)
  end
  defp compute_emb_sim_fallback(ep_emb, query_emb) when is_list(ep_emb) and is_list(query_emb) do
    dot = Enum.zip(ep_emb, query_emb) |> Enum.reduce(0.0, fn {a, b}, acc -> acc + a * b end)
    mag_a = :math.sqrt(Enum.reduce(ep_emb, 0.0, fn x, s -> s + x * x end))
    mag_b = :math.sqrt(Enum.reduce(query_emb, 0.0, fn x, s -> s + x * x end))
    if mag_a == 0 or mag_b == 0, do: nil, else: dot / (mag_a * mag_b)
  end
end

