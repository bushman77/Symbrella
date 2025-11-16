defmodule Brain.LIFG.Guard do
  @moduledoc """
  LIFG input guard / token normalizer.

  Responsibilities (pure, no process calls):

    * Accepts mixed token inputs (maps, structs, atoms, binaries, etc.).
    * Ensures every token is a **map** with at least:
        - `:phrase` (string)
        - `:index` (integer, 0-based if not provided)
    * Normalizes spans:
        - Keeps `{start, stop}` as-is when `stop >= start` and positive length.
        - For any other `{start, stop}` pair of integers, treats `stop` as a
          length and recovers `stop = start + byte_size(phrase)`.
        - Drops `:span` when invalid / missing.
    * Sorts tokens by span start **only** when all spans are valid;
      otherwise preserves original order.
  """

  @type token :: %{optional(atom()) => any()}

  @spec sanitize([term()]) :: [token()]
  def sanitize(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.map(fn {raw, idx} ->
      raw
      |> normalize_raw()
      |> ensure_index(idx)
      |> normalize_span()
    end)
    |> maybe_sort_by_span()
  end

  def sanitize(other) do
    other
    |> List.wrap()
    |> sanitize()
  end

  # -- helpers ---------------------------------------------------------------

  # Step 1: normalize the raw input into a map with a :phrase key.

  # Actual structs: %DemoTok{}, %Core.Token{}, etc.
  defp normalize_raw(%_struct{} = raw) do
    raw
    |> Map.from_struct()
    |> normalize_phrase_key()
  end

  # Plain maps (including maps with string keys).
  defp normalize_raw(%{} = raw) do
    raw
    |> normalize_phrase_key()
  end

  # Bare binaries.
  defp normalize_raw(raw) when is_binary(raw) do
    %{phrase: raw}
  end

  # Atoms (including :gamma in the test).
  defp normalize_raw(raw) when is_atom(raw) do
    %{phrase: Atom.to_string(raw)}
  end

  # Fallback: stringify whatever we got.
  defp normalize_raw(raw) do
    %{phrase: to_string(raw)}
  end

  defp normalize_phrase_key(map) do
    cond do
      # Already has a proper phrase
      Map.has_key?(map, :phrase) ->
        map

      Map.has_key?(map, "phrase") ->
        map
        |> Map.put(:phrase, map["phrase"])
        |> Map.delete("phrase")

      true ->
        phrase =
          map[:lemma] ||
            map["lemma"] ||
            map[:word] ||
            map["word"] ||
            map[:text] ||
            map["text"] ||
            inspect(map)

        Map.put(map, :phrase, phrase)
    end
  end

  # Step 2: ensure :index is present and integer.

  defp ensure_index(%{index: idx} = tok, _when_present) when is_integer(idx) do
    tok
  end

  defp ensure_index(%{"index" => idx} = tok, _when_present) when is_integer(idx) do
    tok
    |> Map.put(:index, idx)
    |> Map.delete("index")
  end

  defp ensure_index(tok, idx) do
    Map.put(tok, :index, idx)
  end

  # Step 3: normalize span semantics.
  #
  # Rules:
  #   * If span missing or invalid → drop it.
  #   * If span = {start, stop} and stop >= start and (stop - start) > 0
  #       → keep as {start, stop}.
  #   * For any other {start, stop} with integer start/stop
  #       → treat stop as "length" and recover via phrase bytes:
  #           {start, start + byte_size(phrase)}.
  defp normalize_span(%{phrase: phrase} = tok) do
    span =
      cond do
        Map.has_key?(tok, :span) -> tok.span
        Map.has_key?(tok, "span") -> tok["span"]
        true -> nil
      end

    normalized =
      case span do
        {start, stop} when is_integer(start) and is_integer(stop) ->
          cond do
            stop >= start and stop - start > 0 ->
              # Looks like a proper {start, stop} range; keep it.
              {start, stop}

            true ->
              # Treat second component as length and recover via phrase length.
              {start, start + byte_size(phrase)}
          end

        _ ->
          :none
      end

    case normalized do
      :none ->
        tok
        |> Map.delete(:span)
        |> Map.delete("span")

      {s, e} ->
        tok
        |> Map.put(:span, {s, e})
        |> Map.delete("span")
    end
  end

  defp normalize_span(tok), do: tok

  # Step 4: sort by span start only if all spans are valid.

  defp maybe_sort_by_span(tokens) do
    if Enum.all?(tokens, &valid_span?/1) do
      Enum.sort_by(tokens, fn %{span: {start, _}} -> start end)
    else
      tokens
    end
  end

  defp valid_span?(%{span: {start, stop}})
       when is_integer(start) and is_integer(stop) and stop >= start,
       do: true

  defp valid_span?(_), do: false
end

