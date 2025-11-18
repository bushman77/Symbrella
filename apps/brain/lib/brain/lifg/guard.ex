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
        - Drops explicit char-grams (e.g. `kind: :chargram` or `source: :chargram`)
          and emits `[:brain, :lifg, :chargram_violation]` plus a
          test-only `[:test, :lifg, :chargram_violation_tripwire]`.
        - When `sentence` is present and `span` is valid:
            · Drops **cross-word char-grams** (substrings that include whitespace)
              as `:chargram` violations.
            · Drops **in-word, non-boundary substrings** as `:boundary_drop`
              (unless `mw: true`).
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
        idx = Map.get(tok, :index, 0)

        if explicit_chargram?(tok) do
          emit_chargram_violation(nil, tok, idx, :chargram)
          {keep, [tok | drop]}
        else
          {[tok | keep], drop}
        end
      end)

    Enum.reverse(kept)
  end

  # When sentence is present:
  #   • Drop explicit char-grams and cross-word char-grams as :chargram.
  #   • Drop in-word non-boundary substrings as boundary_drop (unless mw: true).
  defp maybe_filter_chargrams(tokens, sentence) when is_binary(sentence) do
    {kept, _dropped} =
      Enum.reduce(tokens, {[], []}, fn tok, {keep, drop} ->
        idx = Map.get(tok, :index, 0)

        case classify_violation(sentence, tok) do
          :chargram ->
            emit_chargram_violation(sentence, tok, idx, :chargram)
            {keep, [tok | drop]}

          :boundary ->
            if Map.get(tok, :mw, false) do
              # MWEs bypass boundary drops
              {[tok | keep], drop}
            else
              emit_boundary_drop(sentence, tok, idx)
              {keep, [tok | drop]}
            end

          :ok ->
            {[tok | keep], drop}
        end
      end)

    Enum.reverse(kept)
  end

  # Decide whether a token is a char-gram, a boundary substring, or OK.
  defp classify_violation(sentence, tok) do
    cond do
      explicit_chargram?(tok) ->
        :chargram

      cross_word_chargram?(sentence, tok) ->
        :chargram

      boundary_violation?(sentence, tok) ->
        :boundary

      true ->
        :ok
    end
  end

  # 🔍 Cross-word char-gram: span slice contains whitespace.
  # NOTE:
  #   • Real MWEs (mw: true / "mw" => true) are *not* treated as char-grams here.
  #   • Only non-MWE cross-word substrings are considered char-grams.
  defp cross_word_chargram?(sentence, %{span: {start, stop}} = tok)
       when is_binary(sentence) and is_integer(start) and is_integer(stop) do
    # If upstream has explicitly marked this token as an MWE, we let it
    # through and let Stage1's MWE logic decide what to do (including
    # emitting :mwe_fallback_emitted when it has no senses).
    if Map.get(tok, :mw, false) or Map.get(tok, "mw", false) do
      false
    else
      slice = safe_slice(sentence, start, stop)
      slice != "" and String.contains?(slice, " ")
    end
  end

  defp cross_word_chargram?(_sentence, _tok), do: false

  # 🔍 Explicit char-gram, flagged via kind/source/chargram?
  defp explicit_chargram?(tok) do
    kind   = Map.get(tok, :kind)   || Map.get(tok, "kind")
    source = Map.get(tok, :source) || Map.get(tok, "source")

    kind in [:chargram, :char_ngram, "chargram", "char_ngram"] or
      source in [:chargram, :char_ngram, "chargram", "char_ngram"] or
      Map.get(tok, :chargram?, false) or
      Map.get(tok, "chargram?", false)
  end

  # 🔍 In-word non-boundary substring
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

  # 🚨 Char-gram violation telemetry
  #
  # Emits both:
  #   • [:brain, :lifg, :chargram_violation]
  #   • [:test,  :lifg, :chargram_violation_tripwire]  (for TripwireTest)
  defp emit_chargram_violation(_sentence, tok, token_index, reason) do
    phrase = Map.get(tok, :phrase, "")

    meta = %{
      count: 1,
      reason: reason,
      v: 2,
      phrase: phrase,
      token_index: token_index,
      mw: Map.get(tok, :mw, false)
    }

    # Prod-style event
    :telemetry.execute(
      [:brain, :lifg, :chargram_violation],
      %{},
      meta
    )

    # Test-only tripwire (captured by Brain.LIFG.TripwireTest)
    :telemetry.execute(
      [:test, :lifg, :chargram_violation_tripwire],
      %{},
      meta
    )
  end

  # 🚨 Boundary drop telemetry
  #
  # Used for in-word misaligned substrings (e.g. "hat" inside "what").
  defp emit_boundary_drop(sentence, tok, token_index) do
    phrase =
      case {sentence, Map.get(tok, :span)} do
        {bin, {start, stop}}
        when is_binary(bin) and is_integer(start) and is_integer(stop) and stop > start ->
          safe_slice(bin, start, stop)

        _ ->
          Map.get(tok, :phrase, "")
      end

    meta = %{
      token_index: token_index,
      mw: Map.get(tok, :mw, false),
      phrase: phrase
    }

    :telemetry.execute(
      [:brain, :lifg, :boundary_drop],
      %{},
      meta
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

