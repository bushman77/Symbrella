# apps/core/lib/core/invariants.ex
defmodule Core.Invariants do
  @moduledoc ~S"""
  Runtime invariants for the Core/LIFG pipeline.

  This module is intentionally strict: it will raise fast and with
  actionable messages when tokens violate basic expectations.

  Invariants enforced:

  • `assert_no_chargrams!/1`
      A token that contains whitespace must be a multi-word expression
      (`mw: true`). Any whitespace in `phrase` without `mw: true` is
      treated as a leaked char-gram.

  • `assert_boundary_only_or_mwe!/2`
      Every single-word token must align to word boundaries of the
      original sentence (case-insensitive). MWEs are allowed regardless
      of boundary membership, provided `mw: true`.

  • `assert_sorted_spans!/1`
      Ordering constraints for spans:
        1) All single-word tokens come before any MWEs.
        2) Single-word token starts are non-decreasing.
        3) MWE spans are non-decreasing by `{start, end}`.

      Tokens without a valid `span: {start, end}` are ignored for these checks.

  Notes on span shape:

  - The pipeline may produce `{start, end_exclusive}` (preferred).
  - Some legacy/synthetic inputs may carry `{start, len}`.
  - We accept either by disambiguating using the sentence slice match.
  """

  @doc "Raises if any token is a char-gram (whitespace in `phrase` without `mw: true`)."
  @spec assert_no_chargrams!([map()]) :: :ok
  def assert_no_chargrams!(tokens) when is_list(tokens) do
    leak =
      Enum.find(tokens, fn
        %{phrase: phrase} = t when is_binary(phrase) ->
          String.contains?(phrase, " ") and Map.get(t, :mw, false) !== true

        _ ->
          false
      end)

    if leak, do: raise(ArgumentError, "char-gram token leaked: #{inspect(leak)}")
    :ok
  end

  @doc "Raises unless all single-word tokens align to word boundaries in `sentence`, or are `mw: true`."
  @spec assert_boundary_only_or_mwe!([map()], String.t()) :: :ok
  def assert_boundary_only_or_mwe!(tokens, sentence)
      when is_list(tokens) and is_binary(sentence) do
    sent_norm = normalize_sentence(sentence)

    # Build a set of boundary words from the sentence (case-insensitive).
    words =
      sent_norm
      |> split_words()
      |> MapSet.new()

    bad =
      Enum.find(tokens, fn
        # MWEs are permitted if explicitly flagged.
        %{phrase: phrase, mw: true} when is_binary(phrase) ->
          false

        # Single-word tokens must (a) be a boundary word and, if they carry a
        # span, (b) align to boundaries in the sentence slice.
        %{phrase: phrase} = t when is_binary(phrase) ->
          p = norm(phrase)

          cond do
            p == "" ->
              true

            String.contains?(p, " ") ->
              # Embedded whitespace without `mw: true` is invalid.
              true

            not MapSet.member?(words, p) ->
              true

            align_span_violation?(t, sent_norm, p) ->
              true

            true ->
              false
          end

        _ ->
          false
      end)

    if bad, do: raise(ArgumentError, "non-boundary token present: #{inspect(bad)}")
    :ok
  end

  @doc """
  Asserts ordering matches our injector policy:

  1) All non-MWE (single-word) tokens appear before any MWE tokens.
  2) Non-MWE spans are non-decreasing by start.
  3) MWE spans are non-decreasing by {start, end}.

  Tokens without a valid `span: {start, end}` are ignored for ordering checks.
  """
  @spec assert_sorted_spans!([map()]) :: :ok
  def assert_sorted_spans!(tokens) when is_list(tokens) do
    # 1) words before MWEs in the overall list
    first_mwe_idx = Enum.find_index(tokens, &(Map.get(&1, :mw, false) == true))

    if first_mwe_idx do
      has_word_after? =
        tokens
        |> Enum.drop(first_mwe_idx)
        |> Enum.any?(fn t -> Map.get(t, :mw, false) != true end)

      if has_word_after? do
        raise ArgumentError, "ordering violation: found word token after MWE section"
      end
    end

    # Consider only tokens that have a well-formed span.
    with_spans =
      Enum.filter(tokens, fn
        %{span: {s, e}} when is_integer(s) and is_integer(e) and s >= 0 and e > s -> true
        _ -> false
      end)

    {words, mwes} =
      Enum.split_with(with_spans, fn t -> Map.get(t, :mw, false) != true end)

    # 2) words: starts non-decreasing
    word_starts = Enum.map(words, fn %{span: {s, _e}} -> s end)

    if word_starts != Enum.sort(word_starts) do
      raise ArgumentError,
            "word spans not sorted by start: #{inspect(Enum.map(words, & &1.span))}"
    end

    # 3) mwes: sort key {start, end} must be non-decreasing
    mwe_pairs = Enum.map(mwes, fn %{span: {s, e}} -> {s, e} end)

    if mwe_pairs != Enum.sort_by(mwe_pairs, fn {s, e} -> {s, e} end) do
      raise ArgumentError,
            "MWE spans not sorted by {start,end}: #{inspect(Enum.map(mwes, & &1.span))}"
    end

    :ok
  end

  # ── helpers ──────────────────────────────────────────────────────────────

  # If a token carries a span, ensure the slice matches the phrase
  # and that the slice begins/ends on word boundaries.
  defp align_span_violation?(%{span: {start, second}}, sent_norm, phrase_norm)
       when is_integer(start) and is_integer(second) and is_binary(sent_norm) and
              is_binary(phrase_norm) do
    case disambiguate_span(sent_norm, phrase_norm, start, second) do
      {:ok, {s, e}} ->
        # boundary check: previous and next bytes must NOT be word bytes (or OOB)
        prev = if s == 0, do: nil, else: :binary.at(sent_norm, s - 1)
        nxt = if e == byte_size(sent_norm), do: nil, else: :binary.at(sent_norm, e)

        boundary_ok? = boundary_byte?(prev) and boundary_byte?(nxt)
        not boundary_ok?

      :mismatch ->
        true

      :invalid ->
        true
    end
  end

  defp align_span_violation?(_t, _sent_norm, _phrase_norm), do: false

  # Accept either:
  #   • {start, end_exclusive}   (preferred)
  #   • {start, len}            (legacy)
  #
  # We disambiguate by choosing the in-bounds candidate whose slice matches `phrase_norm`.
  defp disambiguate_span(sent_norm, phrase_norm, start, second) do
    bytes = byte_size(sent_norm)
    ph_len = byte_size(phrase_norm)

    candidates =
      [
        # treat second as end_exclusive
        {start, second},
        # treat second as len
        {start, start + second},
        # treat as phrase length
        {start, start + ph_len}
      ]
      |> Enum.uniq()
      |> Enum.filter(fn {s, e} -> s >= 0 and e > s and e <= bytes end)

    case candidates do
      [] ->
        :invalid

      list ->
        match =
          Enum.find(list, fn {s, e} ->
            norm(slice_bytes(sent_norm, s, e)) == phrase_norm
          end)

        if match, do: {:ok, match}, else: :mismatch
    end
  end

  defp slice_bytes(sent, s, e)
       when is_binary(sent) and is_integer(s) and is_integer(e) and e > s do
    try do
      :binary.part(sent, s, e - s)
    rescue
      _ -> ""
    end
  end

  defp slice_bytes(_, _, _), do: ""

  defp boundary_byte?(nil), do: true
  defp boundary_byte?(c) when is_integer(c), do: not word_byte?(c)
  defp boundary_byte?(_), do: true

  defp word_byte?(b) when is_integer(b) do
    (b >= ?0 and b <= ?9) or
      (b >= ?A and b <= ?Z) or
      (b >= ?a and b <= ?z) or
      b == ?_
  end

  defp word_byte?(_), do: false

  defp normalize_sentence(s) when is_binary(s) do
    s
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
    |> String.downcase()
  end

  defp split_words(s) when is_binary(s) do
    # Split on non-word characters (anything that is not letter, number, or underscore)
    String.split(s, ~r/[^\p{L}\p{N}_]+/u, trim: true)
  end

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp norm(v), do: norm(Kernel.to_string(v))
end
