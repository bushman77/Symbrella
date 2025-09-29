defmodule Core.Invariants do
  @moduledoc ~S"""
  Runtime invariants for the LIFG pipeline.
  """

  @doc "Raises if any token is a char-gram (whitespace in phrase without `mw: true`)."
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

  @doc "Raises unless all tokens align to word boundaries in `sentence`, or are `mw: true`."
  @spec assert_boundary_only_or_mwe!([map()], String.t()) :: :ok
  def assert_boundary_only_or_mwe!(tokens, sentence)
      when is_list(tokens) and is_binary(sentence) do
    words =
      Regex.scan(~r/\p{L}+/u, sentence)
      |> List.flatten()
      |> MapSet.new()

    bad =
      Enum.find(tokens, fn
        %{phrase: phrase, mw: true} when is_binary(phrase) ->
          false

        %{phrase: phrase} when is_binary(phrase) ->
          if String.contains?(phrase, " "), do: true, else: not MapSet.member?(words, phrase)

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
  3) MWE spans are non-decreasing by start; ties broken by increasing end.

  Tokens without a valid `span: {s,e}` are ignored for ordering checks.
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

    # split into words vs mwes (only those with valid spans)
    {words, mwes} =
      tokens
      |> Enum.filter(fn
        %{span: {s, e}} when is_integer(s) and is_integer(e) -> true
        _ -> false
      end)
      |> Enum.split_with(fn t -> Map.get(t, :mw, false) != true end)

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
end
