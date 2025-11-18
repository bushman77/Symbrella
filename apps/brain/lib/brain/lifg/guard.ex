defmodule Brain.LIFG.Guard do
  @moduledoc """
  LIFG input guard / token normalizer + char-gram boundary gate.

  Responsibilities (pure, no process calls):

    * Accepts mixed token inputs (maps, structs, atoms, binaries, etc.).
    * Supports both:
        - bare token lists: `sanitize([term()]) :: [token()]`
        - LIFG slates:      `sanitize(%{tokens: [...], sentence: binary()})`
    * Ensures every token is a **map** with at least:
        - `:phrase` (string)
        - `:index` (integer, 0-based if not provided)
    * Normalizes spans:
        - Keeps `{start, stop}` as-is when `stop >= start` and positive length.
        - For any other `{start, stop}` pair of integers, treats `stop` as a
          length and recovers `stop = start + byte_size(phrase)`.
        - Drops `:span` when invalid / missing.
    * Char-gram / boundary guard:
        - Drops explicit char-grams (e.g. `kind: :chargram` or `source: :chargram`) with telemetry.
        - When `sentence` is present and `span` is valid:
            · Drops non-boundary substrings unless `mw: true`.
            · Emits `[:brain, :lifg, :chargram_violation]` for each drop.
        - When **no** `sentence` is present:
            · Does NOT run boundary heuristics.
            · Only drops explicit char-grams.
    * Sorts tokens by span start **only** when all spans are valid;
      otherwise preserves original order.
  """

  @type token :: %{optional(atom()) => any()}

  @spec sanitize([term()] | map()) :: [token()] | map()
  # Slate form: %{tokens: [...], sentence: "..."} (used by LIFG Stage-1)
  def sanitize(%{tokens: tokens} = slate) do
    sentence = Map.get(slate, :sentence)

    tokens =
      tokens
      |> do_sanitize(sentence)

    %{slate | tokens: tokens}
  end

  # Legacy / simple form: just a list of tokens (no sentence).
  def sanitize(tokens) when is_list(tokens) do
    do_sanitize(tokens, nil)
  end

  # Fallback: wrap into a list and sanitize.
  def sanitize(other) do
    other
    |> List.wrap()
    |> sanitize()
  end

  # -- main pipeline ---------------------------------------------------------

  defp do_sanitize(tokens, sentence) do
    tokens
    |> Enum.with_index()
    |> Enum.map(fn {raw, idx} ->
      raw
      |> normalize_raw()
      |> ensure_index(idx)
      |> normalize_span()
    end)
    |> maybe_filter_chargrams(sentence)
    |> maybe_sort_by_span()
  end

  # -- helpers: raw normalization -------------------------------------------

  # Actual structs: %DemoTok{}, %Core.Token{}, etc.
  # NOTE: pattern matches any struct, but never references Core directly.
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

  # Atoms (including :gamma in tests).
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

  # -- helpers: ensure index -------------------------------------------------

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

  # -- helpers: span normalization -------------------------------------------

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
              {start, stop}

            true ->
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

  # -- char-gram / boundary guard -------------------------------------------

  # When there is NO sentence:
  #   • Only drop explicit char-grams (kind/source/chargram?).
  #   • Don't run boundary heuristics.
  defp maybe_filter_chargrams(tokens, nil) do
    {kept, _dropped} =
      Enum.reduce(tokens, {[], []}, fn tok, {keep, drop} ->
        if explicit_chargram?(tok) do
          emit_chargram_violation(nil, tok, Map.get(tok, :index, 0), :chargram)
          {keep, [tok | drop]}
        else
          {[tok | keep], drop}
        end
      end)

    Enum.reverse(kept)
  end

  # When sentence is present:
  #   • Drop explicit char-grams.
  #   • Drop non-boundary substrings unless mw: true.
  defp maybe_filter_chargrams(tokens, sentence) when is_binary(sentence) do
    {kept, _dropped} =
      Enum.reduce(tokens, {[], []}, fn tok, {keep, drop} ->
        idx = Map.get(tok, :index, 0)

        cond do
          explicit_chargram?(tok) ->
            emit_chargram_violation(sentence, tok, idx, :chargram)
            {keep, [tok | drop]}

          boundary_violation?(sentence, tok) and not Map.get(tok, :mw, false) ->
            emit_chargram_violation(sentence, tok, idx, :boundary)
            {keep, [tok | drop]}

          true ->
            {[tok | keep], drop}
        end
      end)

    Enum.reverse(kept)
  end

  # 🔴 KEY CHANGE: now also looks at :source / "source"
  defp explicit_chargram?(tok) do
    kind   = Map.get(tok, :kind)   || Map.get(tok, "kind")
    source = Map.get(tok, :source) || Map.get(tok, "source")

    kind in [:chargram, :char_ngram, "chargram", "char_ngram"] or
      source in [:chargram, :char_ngram, "chargram", "char_ngram"] or
      Map.get(tok, :chargram?, false) or
      Map.get(tok, "chargram?", false)
  end

  defp boundary_violation?(sentence, %{span: {start, stop}})
       when is_binary(sentence) and is_integer(start) and is_integer(stop) do
    phrase = safe_slice(sentence, start, stop)

    cond do
      phrase == "" ->
        true

      not word_boundary?(sentence, start, stop) ->
        true

      true ->
        false
    end
  end

  defp boundary_violation?(_sentence, _tok), do: false

  defp safe_slice(sentence, start, stop) do
    len = max(stop - start, 0)

    cond do
      len <= 0 ->
        ""

      start < 0 ->
        ""

      start >= byte_size(sentence) ->
        ""

      true ->
        String.slice(sentence, start, len) || ""
    end
  end

  defp word_boundary?(sentence, start, stop) do
    len = String.length(sentence)

    left_char =
      cond do
        start <= 0 -> nil
        true -> String.at(sentence, start - 1)
      end

    right_char =
      cond do
        stop >= len -> nil
        true -> String.at(sentence, stop)
      end

    left_ok? = is_nil(left_char) or left_char in [" ", "\n", "\t"]
    right_ok? = is_nil(right_char) or right_char in [" ", "\n", "\t", ".", ",", "!", "?", ";", ":"]

    left_ok? and right_ok?
  end

  defp emit_chargram_violation(sentence, tok, token_index, reason) do
    phrase =
      case {sentence, Map.get(tok, :span)} do
        {bin, {start, stop}}
        when is_binary(bin) and is_integer(start) and is_integer(stop) and stop > start ->
          safe_slice(bin, start, stop)

        _ ->
          Map.get(tok, :phrase, "")
      end

    :telemetry.execute(
      [:brain, :lifg, :chargram_violation],
      %{},
      %{
        count: 1,
        reason: reason,
        v: 2,
        phrase: phrase,
        token_index: token_index,
        mw: Map.get(tok, :mw, false)
      }
    )
  end

  # -- sorting ---------------------------------------------------------------

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

