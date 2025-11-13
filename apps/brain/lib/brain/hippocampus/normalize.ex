defmodule Brain.Hippocampus.Normalize do
  @moduledoc """
  Normalization helpers for Hippocampus encode/recall.

  Goals:
  • **No Core deps** — accepts generic maps/structs.
  • **Flexible inputs** — tokens, winners, strings, lists, etc.
  • **Consistent norms** — downcased, trimmed strings.
  • **Caller’s choice** — list via `extract_norms_from_any/1` (order-preserving
    & de-duplicated) or set via `cue_set/1`.
  """

  @type norm :: String.t()
  @type cue_set :: MapSet.t(norm)

  # ── Public API ────────────────────────────────────────────────────────────────

  @doc """
  Extract comparable norms from many possible cue/episode shapes.

  Accepts:
    * binary/atom/integer → coerced to downcased strings
    * list                 → flattens recursively and collects each element
    * `%{winners: [...]}`  (episode slate)
    * `%{tokens:  [...]}`  (SI-like)
    * generic map          → tries common keys (`:norm/:lemma/:word/:phrase/:id/:chosen_id`)

  Returns a **de-duplicated list of strings**, preserving first-seen order.
  """
  @spec extract_norms_from_any(term()) :: [norm]
  def extract_norms_from_any(nil), do: []

  def extract_norms_from_any(s) when is_binary(s) do
    s |> norm_string() |> wrap_non_empty()
  end

  def extract_norms_from_any(a) when is_atom(a) do
    a |> Atom.to_string() |> norm_string() |> wrap_non_empty()
  end

  def extract_norms_from_any(i) when is_integer(i) do
    i |> Integer.to_string() |> norm_string() |> wrap_non_empty()
  end

  def extract_norms_from_any(list) when is_list(list) do
    list
    |> Enum.flat_map(&extract_norms_from_any/1)
    |> dedup_preserving_order()
  end

  def extract_norms_from_any(%{winners: winners}) when is_list(winners) do
    winners
    |> Enum.map(&winner_norm/1)
    |> Enum.reject(&empty?/1)
    |> dedup_preserving_order()
  end

  def extract_norms_from_any(%{tokens: tokens}) when is_list(tokens) do
    tokens
    |> Enum.map(&token_norm/1)
    |> Enum.reject(&empty?/1)
    |> dedup_preserving_order()
  end

  def extract_norms_from_any(%{} = map) do
    map
    |> candidate_from_map()
    |> case do
      nil -> []
      "" -> []
      v -> [norm_string(v)]
    end
  end

  def extract_norms_from_any(other) do
    other
    |> to_string()
    |> norm_string()
    |> wrap_non_empty()
  end

  @doc """
  Build a **set** of cues from any input accepted by `extract_norms_from_any/1`.
  """
  @spec cue_set(term()) :: cue_set
  def cue_set(any), do: any |> extract_norms_from_any() |> MapSet.new()

  @doc """
  Build a **set** of normalized tokens for an episode-like structure.

  Accepts:
    * `%{norms: MapSet.t()}` → returned as-is
    * `%{slate: %{tokens: [...]}}` → set from token fields
    * `%{slate: %{winners: [...]}}` → set from winner fields
    * any of the shapes supported by `extract_norms_from_any/1`

  Always returns a `MapSet` of downcased strings.
  """
  @spec episode_token_set(term()) :: cue_set
  def episode_token_set(%{norms: %MapSet{} = s}), do: s

  def episode_token_set(%{slate: %{} = slate}) do
    cond do
      is_list(slate[:tokens]) -> slate[:tokens] |> Enum.map(&token_norm/1) |> set_from()
      is_list(slate[:winners]) -> slate[:winners] |> Enum.map(&winner_norm/1) |> set_from()
      true -> cue_set(slate)
    end
  end

  def episode_token_set(list) when is_list(list), do: list |> cue_set()
  def episode_token_set(other), do: cue_set(other)

  # ── Internals ────────────────────────────────────────────────────────────────

  # Winner elements may be binaries or maps with common keys.
  defp winner_norm(s) when is_binary(s), do: norm_string(s)

  defp winner_norm(%{} = map) do
    map
    |> candidate_from_map()
    |> case do
      nil -> ""
      v -> norm_string(v)
    end
  end

  # Token elements may be binaries or maps with :norm/lemma/word/phrase/id.
  defp token_norm(s) when is_binary(s), do: norm_string(s)

  defp token_norm(%{} = map) do
    map
    |> first_present([:norm, :lemma, :word, :phrase, :id, :chosen_id])
    |> case do
      nil -> ""
      v -> norm_string(v)
    end
  end

  defp token_norm(_), do: ""

  # Try common keys for generic maps.
  defp candidate_from_map(map) when is_map(map) do
    first_present(map, [:norm, :lemma, :word, :phrase, :id, :chosen_id])
  end

  # Returns the first non-empty candidate among the given keys in the map.
  defp first_present(map, keys) do
    Enum.find_value(keys, fn k ->
      case Map.fetch(map, k) do
        :error ->
          nil

        {:ok, nil} ->
          nil

        {:ok, ""} ->
          nil

        {:ok, v} when is_binary(v) ->
          non_blank(v)

        {:ok, v} when is_atom(v) ->
          non_blank(Atom.to_string(v))

        {:ok, v} when is_integer(v) ->
          Integer.to_string(v)

        {:ok, v} ->
          # Allow ids like "alpha|noun|1" in `id`/`chosen_id`
          case k do
            :id -> parse_id_word(v)
            :chosen_id -> parse_id_word(v)
            _ -> nil
          end
      end
    end)
  end

  defp non_blank(v) when is_binary(v) do
    trimmed = String.trim(v)
    if trimmed == "", do: nil, else: trimmed
  end

  # Accept binaries like "alpha|noun|1", atoms/ints too.
  defp parse_id_word(nil), do: nil
  defp parse_id_word(v) when is_atom(v), do: v |> Atom.to_string() |> parse_id_word()
  defp parse_id_word(v) when is_integer(v), do: Integer.to_string(v)

  defp parse_id_word(v) when is_binary(v) do
    case String.split(v, "|", parts: 2) do
      [w | _] -> String.trim(w)
      _ -> nil
    end
  end

  defp parse_id_word(_), do: nil

  # Normalize to downcased, trimmed binary.
  defp norm_string(nil), do: ""
  defp norm_string(s) when is_binary(s), do: s |> String.trim() |> String.downcase()
  defp norm_string(other), do: other |> to_string() |> String.trim() |> String.downcase()

  # True if value is empty/blank.
  def empty?(nil), do: true
  def empty?(""), do: true
  def empty?(s) when is_binary(s), do: String.trim(s) == ""
  def empty?(list) when is_list(list), do: Enum.all?(list, &empty?/1)
  def empty?(_), do: false

  defp wrap_non_empty(""), do: []
  defp wrap_non_empty(v) when is_binary(v), do: [v]

  # Stable de-dupe that preserves first-seen order.
  defp dedup_preserving_order(list) do
    {rev, _seen} =
      Enum.reduce(list, {[], MapSet.new()}, fn v, {acc, seen} ->
        if v == "" or MapSet.member?(seen, v) do
          {acc, seen}
        else
          {[v | acc], MapSet.put(seen, v)}
        end
      end)

    Enum.reverse(rev)
  end

  defp set_from(list) when is_list(list) do
    list
    |> Enum.reject(&empty?/1)
    |> Enum.map(&norm_string/1)
    |> MapSet.new()
  end
end
