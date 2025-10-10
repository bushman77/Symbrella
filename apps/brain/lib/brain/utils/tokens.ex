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

        coerced_span =
          case span_from_token(tm) do
            {:ok, s, l} when is_integer(s) and s >= 0 and is_integer(l) and l > 0 -> {s, l}
            _ -> nil
          end

        phrase0 = Map.get(tm, :phrase) || Map.get(tm, "phrase") || ""

        phrase1 =
          if is_binary(sentence) and coerced_span do
            {s, l} = coerced_span
            (String.slice(sentence, s, l) || "") |> String.trim()
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
      {s, l} when is_integer(s) and s >= 0 and is_integer(l) and l > 0 -> true
      _ -> false
    end
  end

  @spec fill_spans_left_to_right([map()], binary()) :: [map()]
  def fill_spans_left_to_right(tokens, sentence) do
    gsent = String.graphemes(sentence)

    {rev, _cursor} =
      Enum.reduce(tokens, {[], 0}, fn t, {acc, cursor} ->
        case Map.get(t, :span) do
          {s, l} when is_integer(s) and is_integer(l) and l > 0 ->
            {[t | acc], max(cursor, s + l)}

          _ ->
            phrase = to_string(Map.get(t, :phrase) || "")

            if phrase == "" do
              {[t | acc], cursor}
            else
              plen = String.length(phrase)
              start_opt = find_from_grapheme(gsent, phrase, cursor)

              if is_integer(start_opt) do
                span = {start_opt, plen}
                {[Map.put(t, :span, span) | acc], start_opt + plen}
              else
                {[t | acc], cursor}
              end
            end
        end
      end)

    Enum.reverse(rev)
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
  def span_from_token(t) do
    span = Map.get(t, :span) || Map.get(t, "span")

    cond do
      is_tuple(span) and tuple_size(span) == 2 ->
        {s, l} = span
        {:ok, s, l}

      is_map(span) ->
        s = Map.get(span, :start) || Map.get(span, "start")
        l = Map.get(span, :len) || Map.get(span, "len")
        e = Map.get(span, :end) || Map.get(span, "end")

        cond do
          is_integer(s) and is_integer(l) -> {:ok, s, l}
          is_integer(s) and is_integer(e) and e >= s -> {:ok, s, e - s}
          true -> :error
        end

      true ->
        s = Map.get(t, :start) || Map.get(t, "start")
        l = Map.get(t, :len) || Map.get(t, "len")
        e = Map.get(t, :end) || Map.get(t, "end")

        cond do
          is_integer(s) and is_integer(l) -> {:ok, s, l}
          is_integer(s) and is_integer(e) and e >= s -> {:ok, s, e - s}
          true -> :error
        end
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
end
