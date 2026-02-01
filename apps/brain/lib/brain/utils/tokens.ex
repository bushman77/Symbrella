# lib/brain/utils/tokens.ex
defmodule Brain.Utils.Tokens do
  @moduledoc """
  Token normalization and span/phrase utilities.
  """

  @doc """
  Extract tokens from an SI-like map or derive from candidates, then normalize.
  """
  @spec extract_tokens(map() | list(), list()) :: [map()]
  def extract_tokens(si_or, candidates) do
    sentence = (is_map(si_or) && (Map.get(si_or, :sentence) || Map.get(si_or, "sentence"))) || nil

    raw_tokens =
      cond do
        is_map(si_or) and is_list(Map.get(si_or, :tokens)) -> Map.get(si_or, :tokens)
        is_map(si_or) and is_list(Map.get(si_or, "tokens")) -> Map.get(si_or, "tokens")
        true -> build_tokens_from(candidates)
      end

    normalize_tokens(raw_tokens, sentence)
  end

@spec normalize_tokens(list(), binary() | nil) :: [map()]
def normalize_tokens(tokens, sentence) do
  tokens1 =
    tokens
    |> Enum.with_index()
    |> Enum.map(fn {t, fallback_idx} ->
      tm = if is_struct(t), do: Map.from_struct(t), else: t
      idx = Map.get(tm, :index) || Map.get(tm, "index") || fallback_idx

      # span_from_token/1 should now return {:ok, start, end}
      coerced_span =
        case span_from_token(tm) do
          {:ok, s, e} when is_integer(s) and s >= 0 and is_integer(e) and e > s -> {s, e}
          _ -> nil
        end

      phrase0 = Map.get(tm, :phrase) || Map.get(tm, "phrase") || ""

      phrase1 =
        if is_binary(sentence) and coerced_span do
          {s, e} = coerced_span
          safe_slice_bytes(sentence, s, e) |> String.trim()
        else
          phrase0
        end

      tm
      |> Map.put(:index, idx)
      |> Map.put(:phrase, phrase1)
      |> then(fn m -> if coerced_span, do: Map.put(m, :span, coerced_span), else: m end)
    end)

  tokens2 =
    if is_binary(sentence) and Enum.any?(tokens1, fn t -> not has_valid_span?(t) end) do
      fill_spans_left_to_right(tokens1, sentence)
    else
      tokens1
    end

  if Enum.all?(tokens2, &has_valid_span?/1) do
    Enum.sort_by(tokens2, fn t -> {elem(Map.fetch!(t, :span), 0)} end)
  else
    tokens2
  end
end

@spec has_valid_span?(map()) :: boolean()
def has_valid_span?(t) do
  case Map.get(t, :span) do
    {s, e} when is_integer(s) and s >= 0 and is_integer(e) and e > s -> true
    _ -> false
  end
end

@spec fill_spans_left_to_right([map()], binary()) :: [map()]
def fill_spans_left_to_right(tokens, sentence) when is_binary(sentence) do
  {rev, _cursor} =
    Enum.reduce(tokens, {[], 0}, fn t, {acc, cursor} ->
      case Map.get(t, :span) do
        {s, e} when is_integer(s) and is_integer(e) and e > s ->
          {[t | acc], max(cursor, e)}

        _ ->
          phrase = Map.get(t, :phrase) || Map.get(t, "phrase") || ""
          phrase = if is_binary(phrase), do: phrase, else: to_string(phrase)

          if phrase == "" do
            {[t | acc], cursor}
          else
            case find_from_bytes(sentence, phrase, cursor) do
              {pos, len} when is_integer(pos) and is_integer(len) and len > 0 ->
                span = {pos, pos + len}
                {[Map.put(t, :span, span) | acc], pos + len}

              _ ->
                {[t | acc], cursor}
            end
          end
      end
    end)

  Enum.reverse(rev)
end

defp find_from_bytes(sentence, phrase, cursor)
     when is_binary(sentence) and is_binary(phrase) and is_integer(cursor) do
  slen = byte_size(sentence)

  cond do
    phrase == "" -> :nomatch
    cursor < 0 -> :nomatch
    cursor >= slen -> :nomatch
    true ->
      # Search for phrase starting at byte offset cursor
      case :binary.match(sentence, phrase, [{:scope, {cursor, slen - cursor}}]) do
        {pos, len} -> {pos, len}
        :nomatch -> :nomatch
      end
  end
end

  defp find_from_grapheme(gsent, phrase, cursor) do
    plen = String.length(phrase)
    max_start = max(length(gsent) - plen, 0)
    target = String.downcase(phrase)
    do_find_from(gsent, target, cursor, max_start, plen)
  end

  defp do_find_from(_gsent, _target, pos, max_start, _plen) when pos > max_start, do: nil

  defp do_find_from(gsent, target, pos, max_start, plen) do
    slice = gsent |> Enum.slice(pos, plen) |> Enum.join() |> String.downcase()
    if slice == target, do: pos, else: do_find_from(gsent, target, pos + 1, max_start, plen)
  end

@spec span_from_token(map()) :: {:ok, integer(), integer()} | :error
def span_from_token(t) when is_map(t) do
  span = Map.get(t, :span) || Map.get(t, "span")
  phrase = Map.get(t, :phrase) || Map.get(t, "phrase")

  plen =
    if is_binary(phrase) and phrase != "" do
      byte_size(phrase)
    else
      nil
    end

  case span do
    # Tuple form: could be {start,end} (new) OR {start,len} (legacy)
    {s, x} when is_integer(s) and is_integer(x) ->
      tuple_span(s, x, plen)

    # List form: [start, end] (sometimes shows up)
    [s, x] when is_integer(s) and is_integer(x) ->
      tuple_span(s, x, plen)

    # Map span form
    %{} = m ->
      s = Map.get(m, :start) || Map.get(m, "start")
      e = Map.get(m, :end) || Map.get(m, "end")
      l = Map.get(m, :len) || Map.get(m, "len")

      cond do
        is_integer(s) and s >= 0 and is_integer(e) and e > s ->
          {:ok, s, e}

        is_integer(s) and s >= 0 and is_integer(l) and l > 0 ->
          {:ok, s, s + l}

        true ->
          :error
      end

    # No :span key; try flat keys on token
    _ ->
      s = Map.get(t, :start) || Map.get(t, "start")
      e = Map.get(t, :end) || Map.get(t, "end")
      l = Map.get(t, :len) || Map.get(t, "len")

      cond do
        is_integer(s) and s >= 0 and is_integer(e) and e > s ->
          {:ok, s, e}

        is_integer(s) and s >= 0 and is_integer(l) and l > 0 ->
          {:ok, s, s + l}

        true ->
          :error
      end
  end
end

defp tuple_span(s, x, plen) do
  cond do
    not is_integer(s) or not is_integer(x) or s < 0 ->
      :error

    # If phrase length matches x exactly, treat x as a LENGTH.
    is_integer(plen) and plen > 0 and x == plen ->
      {:ok, s, s + x}

    # If phrase length matches (x - s), treat x as an END.
    is_integer(plen) and plen > 0 and x >= s and (x - s) == plen ->
      {:ok, s, x}

    # Default: new style is {start,end}
    x >= s ->
      if x > s, do: {:ok, s, x}, else: :error

    # Fallback: legacy {start,len} where len < start (common in your earlier dump)
    x > 0 ->
      {:ok, s, s + x}

    true ->
      :error
  end
end

  @spec build_tokens_from(list()) :: [map()]
  def build_tokens_from(cands) do
    cands
    |> Enum.group_by(fn c -> Map.get(c, :token_index) || Map.get(c, "token_index") || 0 end)
    |> Enum.map(fn {idx, group} ->
      phrase =
        Enum.find_value(group, fn c ->
          v =
            Map.get(c, :phrase) || Map.get(c, "phrase") ||
              Map.get(c, :word) || Map.get(c, "word") ||
              Map.get(c, :lemma) || Map.get(c, "lemma")

          cond do
            is_binary(v) ->
              vt = String.trim(v)
              if vt != "", do: vt, else: nil

            true ->
              nil
          end
        end) || "t#{idx}"

      %{index: idx, phrase: phrase}
    end)
    |> Enum.sort_by(& &1.index)
  end

defp safe_slice_bytes(s, st, en)
     when is_binary(s) and is_integer(st) and is_integer(en) and en >= st do
  len = byte_size(s)

  cond do
    st < 0 or en < 0 or st > len or en > len ->
      ""

    true ->
      try do
String.slice(s, st, en - st) || ""
      rescue
        _ -> ""
      end
  end
end

defp safe_slice_bytes(_s, _st, _en), do: ""



end
