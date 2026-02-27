# apps/brain/lib/brain/hippocampus/writer.ex
defmodule Brain.Hippocampus.Writer do
  @moduledoc """
  Persistence boundary for Hippocampus episodes.

  Accepts:
    • Hippocampus episode shape: %{slate: map(), meta: map(), norms: MapSet.t()}
    • SI-like shape: %{
        sentence: ..., tokens: ..., atl_slate: ..., evidence: ...
      }

  Writes into Db via insert_all into "episodes".
  """

  require Logger

  alias Brain.Hippocampus.Normalize

  @type opts :: keyword() | map()

@spec maybe_persist(map(), opts) :: map()
def maybe_persist(%{} = payload, opts \\ []) do
  opts = normalize_opts(opts)

  if enabled?(opts) and repo_ready?() do
    payload
    |> build_row(opts)
    |> ensure_timestamps()
    |> insert_row!()
  end

  maybe_prime(payload, opts)
  payload
end

defp insert_row!(row) when is_map(row) do
  # Let it crash on failure (no try/rescue). This makes DB problems visible immediately.
  _ = Db.insert_all("episodes", [row], on_conflict: :nothing)
  :ok
end

defp ensure_timestamps(%{} = row) do
  now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

  row
  |> Map.put_new(:inserted_at, now)
  |> Map.put_new(:updated_at, now)
end

  # ────────────────────────────────────────────────────────────────────────────
  # Row builder (episode-aware)
  # ────────────────────────────────────────────────────────────────────────────

  defp build_row(payload, opts) do
    {tokens, tags, si_blob} =
      cond do
        hippo_episode?(payload) ->
          {
            tokens_from_episode(payload),
            tags_from_episode(payload),
            compact_episode(payload)
          }

        true ->
          {
            tokens_from_si(payload),
            tags_from_si(payload),
            compact_si(payload)
          }
      end

    %{
      user_id: user_id_from(payload, opts),
      tokens: tokens,
      token_count: length(tokens),
      si: si_blob,
      tags: tags,
      embedding: embedding_from(opts)
    }
  end

  defp hippo_episode?(%{slate: %{}, meta: %{}, norms: %MapSet{}}), do: true
  defp hippo_episode?(_), do: false

  defp compact_episode(%{slate: slate, meta: meta, norms: norms}) do
    %{
      episode: %{
        slate: slate,
        meta: meta,
        norms: MapSet.to_list(norms || MapSet.new())
      }
    }
  end

  defp compact_si(%{} = si) do
    # Keep it small; avoid huge nested blobs when possible.
    %{
      sentence: si[:sentence] || si["sentence"],
      tokens: si[:tokens] || si["tokens"] || [],
      atl_slate: si[:atl_slate] || si["atl_slate"],
      evidence: si[:evidence] || si["evidence"]
    }
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Episode tokens/tags
  # ────────────────────────────────────────────────────────────────────────────

  defp tokens_from_episode(%{norms: %MapSet{} = norms}) do
    norms
    |> MapSet.to_list()
    |> Enum.map(&norm/1)
    |> Enum.reject(&Normalize.empty?/1)
    |> Enum.uniq()
  end

  defp tokens_from_episode(_), do: []

  defp tags_from_episode(%{meta: meta, slate: slate}) do
    meta_tags = meta[:tags] || meta["tags"]
    slate_tags = slate[:tags] || slate["tags"]

    base =
      cond do
        is_list(meta_tags) -> meta_tags
        is_list(slate_tags) -> slate_tags
        true -> []
      end

    (["hippo"] ++ base)
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.uniq()
  end

  defp tags_from_episode(_), do: ["hippo"]

  # ────────────────────────────────────────────────────────────────────────────
  # SI tokens/tags (legacy auto/lifg path)
  # ────────────────────────────────────────────────────────────────────────────

  defp tokens_from_si(si) do
    toks =
      (si[:tokens] || si["tokens"] || [])
      |> List.wrap()
      |> Enum.flat_map(&token_extract/1)

    winners =
      (get_in(si, [:atl_slate, :winners]) ||
         get_in(si, ["atl_slate", "winners"]) ||
         [])
      |> List.wrap()
      |> Enum.flat_map(&winner_extract/1)

    (toks ++ winners)
    |> Enum.map(&norm/1)
    |> Enum.reject(&Normalize.empty?/1)
    |> Enum.uniq()
  end

  defp tags_from_si(si) do
    # Prefer explicit tags if present (caller can pass them)
    tags =
      si[:tags] || si["tags"] ||
        get_in(si, [:atl_slate, :tags]) ||
        get_in(si, ["atl_slate", "tags"]) ||
        []

    tags =
      cond do
        is_list(tags) -> tags
        true -> []
      end

    base =
      if tags == [] do
        ["hippo", "auto", "lifg"]
      else
        ["hippo" | tags]
      end

    base
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.uniq()
  end

  defp token_extract(%{} = t) do
    cond do
      is_binary(t[:phrase]) -> [t[:phrase]]
      is_binary(t["phrase"]) -> [t["phrase"]]
      is_binary(t[:lemma]) -> [t[:lemma]]
      is_binary(t["lemma"]) -> [t["lemma"]]
      is_binary(t[:norm]) -> [t[:norm]]
      is_binary(t["norm"]) -> [t["norm"]]
      is_binary(t[:word]) -> [t[:word]]
      is_binary(t["word"]) -> [t["word"]]
      true -> []
    end
  end

  defp token_extract(s) when is_binary(s), do: [s]
  defp token_extract(_), do: []

  defp winner_extract(%{} = w) do
    cond do
      is_binary(w[:lemma]) -> [w[:lemma]]
      is_binary(w["lemma"]) -> [w["lemma"]]
      is_binary(w[:norm]) -> [w[:norm]]
      is_binary(w["norm"]) -> [w["norm"]]
      is_binary(w[:id]) -> [w[:id]]
      is_binary(w["id"]) -> [w["id"]]
      true -> []
    end
  end

  defp winner_extract(s) when is_binary(s), do: [s]
  defp winner_extract(_), do: []

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp norm(v), do: v |> to_string() |> norm()

  # ────────────────────────────────────────────────────────────────────────────
  # Gating / opts / embedding
  # ────────────────────────────────────────────────────────────────────────────

  defp enabled?(opts) do
    # default true; caller can disable by persist: false
    Keyword.get(opts, :persist, true)
  end

  defp repo_ready? do
    Code.ensure_loaded?(Db) and function_exported?(Db, :insert_all, 3)
  end

  defp embedding_from(opts) do
    case Keyword.get(opts, :embedding) do
      nil -> nil
      emb -> emb
    end
  end

defp user_id_from(_payload, opts), do: Keyword.get(opts, :user_id)

  defp maybe_prime(_payload, _opts), do: :ok

  defp normalize_opts(opts) when is_list(opts), do: opts
  defp normalize_opts(%{} = opts), do: Enum.into(opts, [])
  defp normalize_opts(nil), do: []
  defp normalize_opts(_), do: []
end
