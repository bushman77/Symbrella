# apps/brain/lib/brain/lifg/guard.ex
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
      * drop explicit char-grams (source/kind flags)
      * emit `[:brain, :lifg, :chargram_violation]` where meta.reason == :chargram
        and meta.v == 2
      * in `:test`, also emit `[:test, :lifg, :chargram_violation_tripwire]`
  - When sentence is NOT present:
      * be permissive — do NOT drop tokens just because `:span` is missing
        (property tests build synthetic inputs without spans/sentences)

  Span convention (per tests): `{start, end_exclusive}` in bytes.
  """

  @chargram_event [:brain, :lifg, :chargram_violation]
  @boundary_drop_event [:brain, :lifg, :boundary_drop]
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

        # Preserve upstream token indexing when present; only fallback to enumeration.
        idx = tok_index(t0, fallback_idx)

        t1 =
          t0
          |> Map.put(:index, idx)
          |> Map.put(:token_index, idx)

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

    {kept, dropped} = split_chargrams(toks2)
    maybe_emit_chargram_violation_group(dropped)

    kept
    |> maybe_drop_by_boundary(sent)
    |> sort_by_span_if_present()
  end

  # ── Telemetry helpers ─────────────────────────────────────────────────

  defp maybe_emit_chargram_violation_group([]), do: :ok

  defp maybe_emit_chargram_violation_group(dropped) when is_list(dropped) do
    # Deterministic "primary" offender selection:
    # prefer cross-word chargrams (these are what tests assert), else explicit, else first.
    {tok0, sr0, phrase0} =
      Enum.find(dropped, fn {_t, sr, _ph} -> sr == :cross_word end) ||
        Enum.find(dropped, fn {_t, sr, _ph} -> sr == :explicit end) ||
        hd(dropped)

    idx0 = tok_index(tok0, 0)
    span0 = Map.get(tok0, :span) || Map.get(tok0, "span")
    mw0 = tok_mw?(tok0)

    meta = %{
      reason: :chargram,
      subreason: sr0,
      token_index: idx0,
      phrase: phrase0,
      span: span0,
      mw: mw0,
      count: length(dropped),
      token_indexes: Enum.map(dropped, fn {t, _sr, _ph} -> tok_index(t, 0) end),
      phrases: Enum.map(dropped, fn {_t, _sr, ph} -> ph end),
      subreasons: Enum.map(dropped, fn {_t, sr, _ph} -> sr end),
      pid: self(),
      v: 2
    }

    safe_telemetry(@chargram_event, %{}, meta)

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
        token_index: tok_index(tok, 0),
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

  defp split_chargrams(tokens) when is_list(tokens) do
    Enum.reduce(tokens, {[], []}, fn tok, {kept, dropped} ->
      phrase = tok_phrase(tok) || ""
      mw? = tok_mw?(tok)

      cond do
        mw? ->
          {[tok | kept], dropped}

        true ->
          case chargram_subreason(tok, phrase) do
            nil -> {[tok | kept], dropped}
            sr -> {kept, [{tok, sr, phrase} | dropped]}
          end
      end
    end)
    |> then(fn {kept, dropped} -> {Enum.reverse(kept), Enum.reverse(dropped)} end)
  end

  defp chargram_subreason(tok, phrase) do
    cond do
      explicit_chargram?(tok) ->
        :explicit

      is_binary(phrase) and String.match?(phrase, ~r/\s/u) ->
        :cross_word

      true ->
        nil
    end
  end

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
        mw? ->
          false

        not is_tuple(span) ->
          emit_boundary_drop(tok, :missing_span, phrase, span)
          true

        boundary_ok?(sent, span) ->
          false

        true ->
          emit_boundary_drop(tok, :boundary, phrase, span)
          true
      end
    end)
  end

  # span is {start, end_exclusive}
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

  defp tok_index(%{} = t, fallback) do
    raw =
      Map.get(t, :token_index) ||
        Map.get(t, "token_index") ||
        Map.get(t, :index) ||
        Map.get(t, "index")

    coerce_nonneg_int(raw, fallback)
  end

  defp tok_index(_t, fallback), do: fallback

  defp coerce_nonneg_int(v, _fallback) when is_integer(v) and v >= 0, do: v
  defp coerce_nonneg_int(v, fallback) when is_integer(v), do: fallback

  defp coerce_nonneg_int(v, fallback) when is_float(v) do
    n = trunc(v)
    if n >= 0, do: n, else: fallback
  end

  defp coerce_nonneg_int(v, fallback) when is_binary(v) do
    case Integer.parse(String.trim(v)) do
      {n, _} when is_integer(n) and n >= 0 -> n
      _ -> fallback
    end
  end

  defp coerce_nonneg_int(_v, fallback), do: fallback

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
          norm_phrase(phrase1)

        true ->
          ""
      end

    Map.put(t1, :norm, norm1)
  end

  defp ensure_phrase_and_norm(t, _), do: t

  defp maybe_put_span(t, {s, e})
       when is_map(t) and is_integer(s) and is_integer(e) and s >= 0 and e > s,
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

  defp norm_phrase(nil), do: ""

  defp norm_phrase(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm_phrase(v), do: v |> to_string() |> norm_phrase()

  # ── Span normalization + recovery ─────────────────────────────────────

  defp normalize_or_recover_span({s, x}, phrase, sent, cursor)
       when is_integer(s) and is_integer(x) and s >= 0 do
    cond do
      is_binary(sent) ->
        pick_span_for_sentence(sent, phrase, s, x, cursor)

      x > s ->
        {{s, x}, max(cursor, x)}

      x > 0 ->
        e = s + x
        {{s, e}, max(cursor, e)}

      true ->
        ph_len = byte_size(to_string(phrase || ""))
        e = s + max(ph_len, 1)
        {{s, e}, max(cursor, e)}
    end
  end

  defp normalize_or_recover_span(_bad, phrase, sent, cursor) do
    if is_binary(sent) and is_binary(phrase) and String.trim(phrase) != "" do
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
    phrase_norm = norm_phrase(to_string(phrase || ""))
    phrase_present? = phrase_norm != ""
    ph_len = byte_size(to_string(phrase || ""))

    candidates =
      [
        if(x > s, do: {s, x}, else: nil),
        if(x > 0, do: {s, s + x}, else: nil),
        if(x >= s, do: {s, x + 1}, else: nil),
        if(ph_len > 0, do: {s, s + ph_len}, else: nil),
        word_span_at(sent, s)
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.filter(fn {a, b} ->
        is_integer(a) and is_integer(b) and a >= 0 and b > a and b <= len_sent
      end)
      |> Enum.uniq()

    match =
      if phrase_present? do
        Enum.find(candidates, fn {a, b} ->
          norm_phrase(safe_slice(sent, a, b - a)) == phrase_norm
        end)
      else
        nil
      end

    span = match || List.first(candidates)

    case span do
      {a, b} when is_integer(a) and is_integer(b) and b > a and b <= len_sent ->
        {span, max(cursor, b)}

      _ ->
        {nil, cursor}
    end
  end

  defp recover_span(sent, phrase, cursor)
       when is_binary(sent) and is_binary(phrase) and is_integer(cursor) and cursor >= 0 do
    ph = String.trim(phrase)

    if ph == "" do
      nil
    else
      start_at = min(max(cursor, 0), byte_size(sent))

      part =
        try do
          binary_part(sent, start_at, byte_size(sent) - start_at)
        rescue
          _ -> sent
        end

      re = Regex.compile!(Regex.escape(ph), "i")

      case Regex.run(re, part, return: :index) do
        nil ->
          nil

        [{pos, len} | _] ->
          s = start_at + pos
          e = s + len
          {{s, e}, e}
      end
    end
  end

  defp recover_span(_, _, _), do: nil

  defp safe_slice(sent, s, len) do
    binary_part(sent, s, len)
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
