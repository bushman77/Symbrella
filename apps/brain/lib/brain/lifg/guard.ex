defmodule Brain.LIFG.Guard do
  @moduledoc """
  Slate-level token guard for the LIFG path.

  Contract (per tests):
  - Accepts SI-like maps containing `:tokens` or `"tokens"` and returns same shape.
  - Accepts bare token lists and returns a list.
  - When sentence is present:
      * normalize/recover spans
      * drop tokens that do not start/end on word boundaries (unless `mw: true`)
      * emit `[:brain, :lifg, :boundary_drop]` with meta including `mw: false/true`
  - Always:
      * drop classic char-gram symptoms (non-mw tokens whose phrase contains whitespace)
      * emit `[:brain, :lifg, :chargram_violation]` where meta.reason == :chargram
  - When sentence is NOT present:
      * be permissive — do NOT drop tokens just because `:span` is missing
        (property tests build synthetic inputs without spans/sentences)
  """

  @chargram_event [:brain, :lifg, :chargram_violation]
  @boundary_drop_event [:brain, :lifg, :boundary_drop]

  # Test-only tripwire event (emitted in test env in addition to prod event)
  @chargram_tripwire_event [:test, :lifg, :chargram_violation_tripwire]

  # ── Public entry points ───────────────────────────────────────────────

  def sanitize(%{tokens: toks} = si) when is_list(toks) do
    sent = normalize_sentence(Map.get(si, :sentence) || Map.get(si, "sentence"))
    Map.put(si, :tokens, sanitize(toks, sent))
  end

  def sanitize(%{"tokens" => toks} = si) when is_list(toks) do
    sent = normalize_sentence(Map.get(si, :sentence) || Map.get(si, "sentence"))
    Map.put(si, "tokens", sanitize(toks, sent))
  end

  def sanitize(tokens) when is_list(tokens), do: sanitize(tokens, nil)
  def sanitize(other), do: other

  # ── Core sanitizer ────────────────────────────────────────────────────

  def sanitize(tokens, sentence) when is_list(tokens) do
    sent = normalize_sentence(sentence)

    {toks2, _cursor} =
      tokens
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {tok, fallback_idx}, cursor ->
        t0 = mapify(tok)

        idx =
          case {Map.get(t0, :index), Map.get(t0, "index")} do
            {i, _} when is_integer(i) -> i
            {_, i} when is_integer(i) -> i
            _ -> fallback_idx
          end

        t1 = Map.put(t0, :index, idx)

        phrase =
          tok_phrase(t1) ||
            phrase_from_id(Map.get(t1, :id) || Map.get(t1, "id")) ||
            ""

        t2 = ensure_phrase_and_norm(t1, phrase)

        span0 = Map.get(t2, :span) || Map.get(t2, "span")
        {span1, cursor2} = normalize_or_recover_span(span0, phrase, sent, cursor)

        t3 =
          t2
          |> maybe_put_span(span1)
          |> maybe_put_mw_from_id()

        {t3, cursor2}
      end)

    toks2
    |> Enum.reject(&drop_chargram?/1)
    |> maybe_drop_by_boundary(sent)
    |> sort_by_span_if_present()
  end

  # ── Telemetry helpers ─────────────────────────────────────────────────

  defp emit_chargram_violation(tok, phrase) do
    meta = %{
      reason: :chargram,
      token_index: tok_index(tok),
      phrase: phrase,
      span: Map.get(tok, :span) || Map.get(tok, "span"),
      mw: tok_mw?(tok),
      count: 1,
      v: 2
    }

    safe_telemetry(@chargram_event, %{}, meta)

    # In test env, ALSO emit the tripwire event so tests can assert it deterministically.
    if test_env?() do
      safe_telemetry(@chargram_tripwire_event, %{}, meta)
    end
  end

  defp emit_boundary_drop(tok, reason, phrase, span) do
    safe_telemetry(
      @boundary_drop_event,
      %{},
      %{
        reason: reason,
        token_index: tok_index(tok),
        phrase: phrase,
        span: span,
        mw: tok_mw?(tok),
        slice: nil,
        count: 1,
        v: 2
      }
    )
  end

  defp safe_telemetry(event, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, meas, meta)
    else
      :ok
    end
  end

  defp test_env? do
    Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env() == :test
  end

  # ── Char-gram dropping (always active) ────────────────────────────────

  defp drop_chargram?(tok) when is_map(tok) do
    mw? = tok_mw?(tok)
    phrase = tok_phrase(tok) || ""

    cond do
      # explicit chargram flags are hard drops unless mw
      explicit_chargram?(tok) and not mw? ->
        emit_chargram_violation(tok, phrase)
        true

      # cross-word non-mw is a classic char-gram symptom
      (not mw?) and is_binary(phrase) and String.contains?(phrase, " ") ->
        emit_chargram_violation(tok, phrase)
        true

      true ->
        false
    end
  end

  defp drop_chargram?(_), do: false

  defp explicit_chargram?(t) when is_map(t) do
    Map.get(t, :chargram) in [true, "true"] or
      Map.get(t, "chargram") in [true, "true"] or
      Map.get(t, :chargram?) in [true, "true"] or
      Map.get(t, "chargram?") in [true, "true"] or
      Map.get(t, :kind) in [:chargram, "chargram", :char_ngram, "char_ngram"] or
      Map.get(t, "kind") in [:chargram, "chargram", :char_ngram, "char_ngram"] or
      Map.get(t, :source) in [:chargram, "chargram", :char_ngram, "char_ngram"] or
      Map.get(t, "source") in [:chargram, "chargram", :char_ngram, "char_ngram"]
  end

  defp explicit_chargram?(_), do: false

  # ── Boundary dropping (only when sentence exists) ─────────────────────

  defp maybe_drop_by_boundary(tokens, nil), do: tokens

  defp maybe_drop_by_boundary(tokens, sent) when is_binary(sent) do
    Enum.reject(tokens, fn tok ->
      mw? = tok_mw?(tok)
      phrase = tok_phrase(tok) || ""
      span = Map.get(tok, :span) || Map.get(tok, "span")

      cond do
        not is_tuple(span) ->
          emit_boundary_drop(tok, :missing_span, phrase, span)
          true

        mw? ->
          false

        boundary_ok?(sent, span) ->
          false

        true ->
          emit_boundary_drop(tok, :boundary, phrase, span)
          true
      end
    end)
  end

  defp boundary_ok?(sent, {s, e})
       when is_binary(sent) and is_integer(s) and is_integer(e) do
    len = byte_size(sent)

    cond do
      s < 0 or e <= s or e > len ->
        false

      true ->
        prev_ok = s == 0 or not word_byte?(:binary.at(sent, s - 1))
        next_ok = e == len or not word_byte?(:binary.at(sent, e))
        prev_ok and next_ok
    end
  end

  defp boundary_ok?(_, _), do: false

  defp word_byte?(b) when is_integer(b) do
    (b >= ?a and b <= ?z) or (b >= ?A and b <= ?Z) or (b >= ?0 and b <= ?9) or b == ?_
  end

  defp word_byte?(_), do: false

  # ── Token helpers ─────────────────────────────────────────────────────

  defp tok_phrase(%{} = t) do
    Map.get(t, :phrase) ||
      Map.get(t, "phrase") ||
      Map.get(t, :norm) ||
      Map.get(t, "norm") ||
      Map.get(t, :lemma) ||
      Map.get(t, "lemma") ||
      Map.get(t, :word) ||
      Map.get(t, "word")
  end

  defp tok_phrase(_), do: nil

  defp tok_index(%{} = t) do
    Map.get(t, :index) ||
      Map.get(t, "index") ||
      Map.get(t, :token_index) ||
      Map.get(t, "token_index")
  end

  defp tok_index(_), do: nil

  defp tok_mw?(%{} = t) do
    mw_flag = Map.get(t, :mw) == true or Map.get(t, "mw") == true

    n =
      case Map.get(t, :n) || Map.get(t, "n") do
        i when is_integer(i) -> i
        _ -> 1
      end

    id = to_string(Map.get(t, :id) || Map.get(t, "id") || "")
    pos = to_string(Map.get(t, :pos) || Map.get(t, "pos") || "")

    mw_flag or n > 1 or
      String.contains?(id, "|phrase|") or
      String.downcase(pos) == "phrase"
  end

  defp tok_mw?(_), do: false

  defp ensure_phrase_and_norm(%{} = t, phrase) do
    phrase1 =
      cond do
        is_binary(Map.get(t, :phrase)) and String.trim(Map.get(t, :phrase)) != "" ->
          Map.get(t, :phrase)

        is_binary(Map.get(t, "phrase")) and String.trim(Map.get(t, "phrase")) != "" ->
          Map.get(t, "phrase")

        is_binary(phrase) and String.trim(phrase) != "" ->
          phrase

        true ->
          ""
      end

    t1 = Map.put(t, :phrase, phrase1)

    norm1 =
      cond do
        is_binary(Map.get(t, :norm)) and String.trim(Map.get(t, :norm)) != "" ->
          Map.get(t, :norm)

        is_binary(Map.get(t, "norm")) and String.trim(Map.get(t, "norm")) != "" ->
          Map.get(t, "norm")

        is_binary(phrase1) and String.trim(phrase1) != "" ->
          down(phrase1)

        true ->
          ""
      end

    Map.put(t1, :norm, norm1)
  end

  defp ensure_phrase_and_norm(t, _), do: t

  defp maybe_put_span(t, {s, e}) when is_map(t) and is_integer(s) and is_integer(e),
    do: Map.put(t, :span, {s, e})

  defp maybe_put_span(t, _), do: t

  defp phrase_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [ph, _rest] -> ph
      _ -> nil
    end
  end

  defp phrase_from_id(_), do: nil

  defp maybe_put_mw_from_id(%{} = t) do
    id = to_string(Map.get(t, :id) || Map.get(t, "id") || "")

    if String.contains?(id, "|phrase|") do
      Map.put_new(t, :mw, true)
    else
      t
    end
  end

  defp maybe_put_mw_from_id(other), do: other

  defp normalize_sentence(sent) when is_binary(sent) do
    if String.trim(sent) == "", do: nil, else: sent
  end

  defp normalize_sentence(_), do: nil

  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  defp down(_), do: ""

  # ── Span normalization + recovery ─────────────────────────────────────

# Replace your current clause:
# defp normalize_or_recover_span({s, x}, phrase, sent, cursor) ...

defp normalize_or_recover_span({s, x}, phrase, sent, cursor)
     when is_integer(s) and is_integer(x) and s >= 0 do
  cond do
    is_binary(sent) ->
      pick_span_for_sentence(sent, phrase, s, x, cursor)

    # treat as {start,end}
    x > s ->
      {{s, x}, max(cursor, x)}

    # treat as {start,len} when end isn't plausible
    x > 0 ->
      e = s + x
      {{s, e}, max(cursor, e)}

    # {start,0} (or {start,start}) -> recover from phrase length
    true ->
      ph_len = byte_size(to_string(phrase || ""))
      e = s + max(ph_len, 1)
      {{s, e}, max(cursor, e)}
  end
end

  defp normalize_or_recover_span(_bad, phrase, sent, cursor) do
    if is_binary(sent) and is_binary(phrase) and phrase != "" do
      case recover_span(sent, phrase, cursor) do
        {span, cursor2} -> {span, cursor2}
        nil -> {nil, cursor}
      end
    else
      {nil, cursor}
    end
  end

  defp pick_span_for_sentence(sent, phrase, s, x, cursor) do
    len_sent = byte_size(sent)
    phrase2 = down(to_string(phrase || ""))
    phrase_present? = phrase2 != ""
    ph_len = if phrase_present?, do: byte_size(phrase2), else: 0

    candidates =
      [
        {s, x},
        {s, s + x},
        if(phrase_present?, do: {s, s + ph_len}, else: nil)
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    match =
      if phrase_present? do
        Enum.find(candidates, fn {a, b} ->
          a >= 0 and b > a and b <= len_sent and match_span_phrase?(sent, phrase2, a, b)
        end)
      else
        nil
      end

    span =
      match ||
        Enum.find(candidates, fn {a, b} -> a >= 0 and b > a and b <= len_sent end) ||
        word_span_at(sent, s) ||
        {s, min(len_sent, s + max(x, 1))}

    span =
      case span do
        {a, b} when is_integer(a) and is_integer(b) and b <= a ->
          word_span_at(sent, a) || {a, min(len_sent, a + 1)}

        other ->
          other
      end

    {span, max(cursor, elem(span, 1))}
  end

  defp match_span_phrase?(sent, phrase, s, e) do
    down(safe_slice(sent, s, e)) == down(phrase)
  end

  defp recover_span(sent, phrase, cursor)
       when is_binary(sent) and is_binary(phrase) and is_integer(cursor) and cursor >= 0 do
    sent2 = down(sent)
    ph2 = down(phrase)

    cur = min(cursor, byte_size(sent2))
    rest = binary_part(sent2, cur, byte_size(sent2) - cur)

    case :binary.match(rest, ph2) do
      :nomatch ->
        nil

      {pos, _len} ->
        s = cur + pos
        e = s + byte_size(ph2)
        {{s, e}, e}
    end
  end

  defp recover_span(_, _, _), do: nil

  defp safe_slice(sent, s, e) do
    binary_part(sent, s, e - s)
  rescue
    _ -> ""
  end

  # ── Misc internals ────────────────────────────────────────────────────

  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m), do: m
  defp mapify(other), do: %{phrase: to_string(other)}

  defp sort_by_span_if_present(list) when is_list(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, fn %{span: {s, _e}} -> s end)
    else
      list
    end
  end

  defp valid_span?(%{span: {s, e}})
       when is_integer(s) and is_integer(e) and s >= 0 and e > s,
       do: true

  defp valid_span?(_), do: false

  defp word_span_at(sent, s) when is_binary(sent) and is_integer(s) do
    len = byte_size(sent)

    cond do
      s < 0 or s >= len ->
        nil

      not word_byte?(:binary.at(sent, s)) ->
        nil

      true ->
        e = scan_word_end(sent, s, len)
        if e > s, do: {s, e}, else: nil
    end
  end

  defp word_span_at(_, _), do: nil

  defp scan_word_end(sent, i, len) do
    cond do
      i >= len -> len
      word_byte?(:binary.at(sent, i)) -> scan_word_end(sent, i + 1, len)
      true -> i
    end
  end
end

