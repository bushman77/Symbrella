defmodule Brain.LIFG.BoundaryGuard do
  @moduledoc ~S"""
  BoundaryGuard: drop tokens that do not start and end on word boundaries
  in the original sentence, unless `mw: true`. Intended to run **before** LIFG.

  Behaviors:
  - Keep tokens with `mw: true` (multiword expressions).
  - Keep single-word tokens whose `phrase` is aligned to word boundaries.
  - Drop char-grams and substrings that cut through words (e.g., "ick" in "kick").
  - When `sentence` is unavailable, use conservative heuristics (wordish single tokens only).
  - De-duplicate by `{phrase, span}`; prefer `mw: true` when duplicates exist; stable sort by start, then MWE flag (singles first), then length.

  Implementation note: keep logic out of function guards. We only use basic shape checks in code paths.
  """

  @type token :: map()

  @spec sanitize([token], String.t() | nil) :: [token]
  def sanitize(tokens, sentence \\ nil) do
    tokens =
      tokens
      |> List.wrap()
      # ensure mw:true wins de-dup ties on {phrase, span}
      |> Enum.sort_by(fn t -> if Map.get(t, :mw, false), do: 0, else: 1 end)

    tokens
    |> Enum.uniq_by(fn t -> {Map.get(t, :phrase), Map.get(t, :span)} end)
    |> then(fn uniq ->
      if is_binary(sentence) do
        Enum.filter(uniq, fn t -> keep_with_sentence?(t, sentence) end)
      else
        Enum.filter(uniq, &keep_without_sentence?/1)
      end
    end)
    |> Enum.sort_by(&sort_key/1)
  end

  # -------- internals --------

  # With a sentence available, prefer strict boundary alignment via spans when present.
  defp keep_with_sentence?(%{mw: true}, _s), do: true

  defp keep_with_sentence?(t, s) do
    phrase =
      t
      |> Map.get(:phrase, "")
      |> to_string()
      |> String.trim()

    cond do
      phrase == "" ->
        false

      # Only allow multiword phrases if mw: true
      Regex.match?(~r/\s/u, phrase) and Map.get(t, :mw) != true ->
        false

      # Drop single-character tokens in LIFG path
      String.length(phrase) == 1 ->
Regex.match?(~r/^[A-Z]$/u, phrase) or phrase == "I"
      true ->
        case Map.get(t, :span) do
          {i, j} ->
            if is_integer(i) and is_integer(j) and i >= 0 and j > i and j <= String.length(s) do
              sub = String.slice(s, i, j - i)
              aligned = edge_word_boundary?(s, i) and edge_word_boundary?(s, j)
              aligned and String.trim(sub) != ""
            else
              boundary_match?(s, phrase)
            end

          _ ->
            boundary_match?(s, phrase)
        end
    end
  end

  # Without a sentence, keep only explicit MWEs or "wordish" single tokens (no whitespace).
  defp keep_without_sentence?(%{mw: true}), do: true

  defp keep_without_sentence?(t) do
    phrase =
      t
      |> Map.get(:phrase, "")
      |> to_string()
      |> String.trim()

    cond do
      phrase == "" ->
        false

      Regex.match?(~r/\s/u, phrase) ->
        false

      String.length(phrase) == 1 ->
        false

      true ->
        wordish?(phrase)
    end
  end

  # Sorting: by start index (if present), then singles before MWEs, then shorter before longer.
  defp sort_key(t) do
    {start, len} =
      case Map.get(t, :span) do
        {i, j} when is_integer(i) and is_integer(j) and j >= i -> {i, j - i}
        _ -> {9_000_000, 9_000_000}
      end

    mw_flag = if Map.get(t, :mw, false), do: 1, else: 0
    {start, mw_flag, len}
  end

  # Regex boundary-anchored search (Unicode-aware).
  defp boundary_match?(sentence, phrase) do
    escaped = Regex.escape(phrase)
    # word char class: letters, marks, numbers, underscore, apostrophes, en-dash, hyphen
    Regex.match?(~r/(?<![\p{L}\p{M}\p{N}_'’-])#{escaped}(?![\p{L}\p{M}\p{N}_'’-])/u, sentence)
  end

  # Word boundary check at absolute index `idx` in `sentence`
  defp edge_word_boundary?(sentence, idx) do
    cond do
      not is_integer(idx) ->
        false

      idx <= 0 ->
        true

      idx >= String.length(sentence) ->
        true

      true ->
        prev = String.at(sentence, idx - 1)
        next = String.at(sentence, idx)
        non_word?(prev) or non_word?(next)
    end
  end

  defp non_word?(nil), do: true
  defp non_word?(ch), do: not String.match?(ch, ~r/[\p{L}\p{M}\p{N}_'’-]/u)

  defp wordish?(p), do: String.match?(p, ~r/^\p{L}[\p{L}\p{M}\p{N}_'’-]*$/u)
end
