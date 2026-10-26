defmodule Brain.Hippocampus.SemanticAssociations do
  @moduledoc """
  Opaque hippocampal pointers to durable semantic conclusions.

  No Core dependency, no reasoning, no factual claims. An association is a
  retrieval HINT only: the caller must fetch and independently validate the
  referenced semantic record before using any of its content.

  Semantic pointers reuse the existing episodic store and warm-start window.
  No autonomous replay or chat messages are produced.
  """

  alias Brain.Hippocampus

  @tag "semantic_association"
  @kind "population_comparison"
  @max_cues 12
  defp regex_fingerprint, do: ~r/\A[0-9a-f]{64}\z/

  @spec remember(String.t(), [String.t()]) :: :ok | {:error, atom()}
  def remember(fingerprint, [a, b] = subjects)
      when is_binary(fingerprint) and is_binary(a) and is_binary(b) do
    if valid_fingerprint?(fingerprint) and valid_subjects?(subjects) do
      if is_pid(Process.whereis(Hippocampus)) do
        words = cue_tokens(subjects)

        # Deliberately store no claim, numeric value, assistant answer or
        # source text in the episode. The fingerprint only indexes DB facts.
        slate = %{winners: words, tags: [@tag]}
        meta = %{
          kind: @tag,
          key: @kind,
          value: fingerprint,
          subject: "public_semantic_memory",
          tags: [@tag]
        }

        # Repeated chat answers should not flood the episodic window with
        # the same proof pointer. Cold restarts still recover recent pointers
        # from the ordinary hippocampal episode store.
        unless fingerprint in cues(subjects, @max_cues) do
          _ = Hippocampus.encode(slate, meta)
        end

        :ok
      else
        {:error, :hippocampus_unavailable}
      end
    else
      {:error, :invalid_association}
    end
  catch
    :exit, _ -> {:error, :hippocampus_unavailable}
  end

  def remember(_, _), do: {:error, :invalid_association}

  @doc """
  Bounded indexed recall. Only specially tagged episodes can contribute
  opaque fingerprints, never untrusted free-form episode text.
  """
  @spec cues([String.t()], pos_integer()) :: [String.t()]
  def cues(subjects, limit \\ 8)

  def cues([a, b] = subjects, limit)
      when is_binary(a) and is_binary(b) and is_integer(limit) and limit > 0 do
    if valid_subjects?(subjects) and is_pid(Process.whereis(Hippocampus)) do
      subjects
      |> cue_tokens()
      |> Hippocampus.recall(
        source: :memory,
        scope: %{kind: @tag},
        limit: min(limit, @max_cues),
        min_jaccard: 0.01,
        ignore_head: :never
      )
      |> extract_fingerprints()
    else
      []
    end
  catch
    :exit, _ -> []
  end

  def cues(_, _), do: []

  @spec extract_fingerprints(list()) :: [String.t()]
  def extract_fingerprints(results) when is_list(results) do
    results
    |> Enum.flat_map(fn
      %{episode: %{meta: meta}} when is_map(meta) ->
        kind = Map.get(meta, :kind, Map.get(meta, "kind"))
        key = Map.get(meta, :key, Map.get(meta, "key"))
        value = Map.get(meta, :value, Map.get(meta, "value"))
        tags = Map.get(meta, :tags, Map.get(meta, "tags", []))

        if kind == @tag and key == @kind and @tag in List.wrap(tags) and
             valid_fingerprint?(value) do
          [value]
        else
          []
        end

      _ -> []
    end)
    |> Enum.uniq()
  end

  def extract_fingerprints(_), do: []

  defp valid_fingerprint?(value) when is_binary(value),
    do: Regex.match?(regex_fingerprint(), value)

  defp valid_fingerprint?(_), do: false

  defp valid_subjects?([a, b]) do
    Enum.all?([a, b], fn s ->
      is_binary(s) and byte_size(String.trim(s)) in 3..100
    end) and String.downcase(String.trim(a)) != String.downcase(String.trim(b))
  end

  defp valid_subjects?(_), do: false

  defp cue_tokens(subjects) do
    subjects
    |> Enum.flat_map(fn s ->
      s
      |> String.downcase()
      |> String.split(~r/[^\p{L}\p{N}]+/u, trim: true)
    end)
    |> Enum.uniq()
    |> Enum.take(16)
  end
end
