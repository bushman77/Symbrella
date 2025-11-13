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
      Tokens without a valid `span: {start, len}` are ignored for these checks.
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
        # char-span, (b) align to boundaries in the sentence slice.
        %{phrase: phrase} = t when is_binary(phrase) ->
          p = norm(phrase)

          cond do
            p == "" ->
              true

            String.contains?(p, " ") ->
              # Single-word check only; embedded whitespace without `mw: true` is invalid.
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

  Tokens without a valid `span: {start, len}` are ignored for ordering checks.
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

    # Consider only tokens that have a well-formed char span.
    with_spans =
      Enum.filter(tokens, fn
        %{span: {s, e}} when is_integer(s) and is_integer(e) and s >= 0 and e >= 0 -> true
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

  # If a token carries a char span, ensure the slice matches the phrase
  # and that the slice begins/ends on word boundaries.
  defp align_span_violation?(%{span: {start, len}, phrase: phrase}, sent_norm, phrase_norm)
       when is_integer(start) and is_integer(len) and is_binary(sent_norm) and is_binary(phrase) do
    # Slice guard
    in_bounds? = start >= 0 and len > 0 and start + len <= String.length(sent_norm)

    if not in_bounds? do
      # If the span is malformed, treat as violation.
      true
    else
      slice = String.slice(sent_norm, start, len) |> norm()
      slice_mismatch? = slice != phrase_norm

      # Boundary check: previous and next chars must be boundary or OOB.
      prev = if start - 1 < 0, do: ?\s, else: String.at(sent_norm, start - 1)

      next =
        if start + len >= String.length(sent_norm),
          do: ?\s,
          else: String.at(sent_norm, start + len)

      boundary? = boundary_char?(prev) and boundary_char?(next)

      slice_mismatch? or not boundary?
    end
  end

  defp align_span_violation?(_t, _sent_norm, _phrase_norm), do: false

  defp boundary_char?(nil), do: true

  defp boundary_char?(<<c::utf8>>) do
    # Treat letters, digits, and underscore as non-boundary; everything else as boundary.
    not match?(true, letter_or_digit_or_uscore?(c))
  end

  defp letter_or_digit_or_uscore?(c) do
    # ASCII fast path; for non-ASCII, rely on Unicode properties via Regex
    cond do
      c in ?0..?9 ->
        true

      c in ?A..?Z ->
        true

      c in ?a..?z ->
        true

      c == ?_ ->
        true

      true ->
        # Non-ASCII: consider as letter/digit if it matches \p{L} or \p{N}
        Regex.match?(~r/^\p{L}$|^\p{N}$/u, <<c::utf8>>)
    end
  end

  defp normalize_sentence(s) when is_binary(s) do
    s
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
    |> String.downcase()
  end

  defp split_words(s) when is_binary(s) do
    # Split on non-word characters (anything that is not letter, number, or underscore)
    s
    |> String.split(~r/[^\p{L}\p{N}_]+/u, trim: true)
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
