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

  Span convention (per tests): `{start, end_exclusive}` in bytes.
  """

  @chargram_event [:brain, :lifg, :chargram_violation]
  @boundary_drop_event [:brain, :lifg, :boundary_drop]
  @chargram_tripwire_event [:test, :lifg, :chargram_violation_tripwire]

  @type token :: map()
  @type si_like :: map()

  # ── Public ────────────────────────────────────────────────────────────

  @spec sanitize(si_like | [token]) :: si_like | [token]
  def sanitize(%{tokens: toks} = si) when is_list(toks) do
    sent = Map.get(si, :sentence) || Map.get(si, "sentence")
    toks2 = sanitize(toks, sent)

    si
    |> Map.put(:tokens, toks2)
    |> maybe_put("tokens", toks2)
  end

  def sanitize(%{"tokens" => toks} = si) when is_list(toks) do
    sent = Map.get(si, :sentence) || Map.get(si, "sentence")
    toks2 = sanitize(toks, sent)

    si
    |> Map.put("tokens", toks2)
    |> maybe_put(:tokens, toks2)
  end

  def sanitize(tokens) when is_list(tokens), do: sanitize(tokens, nil)
  def sanitize(other), do: other

  @spec sanitize([token], String.t() | nil) :: [token]
  def sanitize(tokens, sentence) when is_list(tokens) do
    sent = normalize_sentence(sentence)

    {normed, _cursor} =
      tokens
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {tok, fallback_idx}, cursor ->
        t0 = mapify(tok)
        idx = tok_index(t0, fallback_idx)

        # Ensure Stage1 can read either atom OR string keys.
        t1 =
          t0
          |> put_both(:index, idx)
          |> put_both(:token_index, idx)
          |> put_both(:n, tok_n(t0))
          |> put_both(:mw, tok_mw?(t0))

        phrase =
          tok_phrase(t1) ||
            phrase_from_id(Map.get(t1, :id) || Map.get(t1, "id")) ||
            ""

        t2 = ensure_phrase_and_norm(t1, phrase)

        span0 = Map.get(t2, :span) || Map.get(t2, "span")
        {span1, cursor2} = normalize_or_recover_span(span0, tok_phrase(t2) || "", sent, cursor)

        t3 =
          t2
          |> maybe_put_span(span1)
          |> maybe_put_mw_from_id()

        {t3, cursor2}
      end)

    {kept, dropped} = split_chargrams(normed)

    maybe_emit_chargram_violation_group(dropped)

    kept
    |> maybe_drop_by_boundary(sent)
    |> sort_by_span_then_index()
  end

  # ── Chargrams ─────────────────────────────────────────────────────────

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

  # IMPORTANT: tests want meta.reason == :chargram and meta.v == 2
  defp maybe_emit_chargram_violation_group([]), do: :ok

  defp maybe_emit_chargram_violation_group(dropped) when is_list(dropped) do
    {tok0, sr0, phrase0} =
      Enum.find(dropped, fn {_t, sr, _ph} -> sr == :cross_word end) ||
        Enum.find(dropped, fn {_t, sr, _ph} -> sr == :explicit end) ||
        hd(dropped)

    idx0 = tok_index(tok0, 0)
    span0 = Map.get(tok0, :span) || Map.get(tok0, "span")

    meta = %{
      reason: :chargram,
      subreason: sr0,
      token_index: idx0,
      phrase: phrase0,
      span: span0,
      mw: tok_mw?(tok0),
      count: length(dropped),
      token_indexes: Enum.map(dropped, fn {t, _sr, _ph} -> tok_index(t, 0) end),
      v: 2
    }

    safe_telemetry(@chargram_event, %{count: length(dropped)}, meta)

    if test_env?() do
      safe_telemetry(@chargram_tripwire_event, %{count: length(dropped)}, meta)
    end
  end

  # ── Boundary (only when sentence exists) ───────────────────────────────

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
          # With sentence present we *try* to recover; if we still have no span, treat as chargram.
          emit_chargram_violation(tok, :bad_span, phrase, span)
          true

        boundary_ok?(sent, span) ->
          false

        true ->
          emit_boundary_drop(tok, :boundary, phrase, span, sent)
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

  defp emit_boundary_drop(tok, reason, phrase, span, sent) do
    meta = %{
      reason: reason,
      token_index: tok_index(tok, 0),
      phrase: phrase,
      span: span,
      mw: tok_mw?(tok),
      slice:
        case {sent, span} do
          {bin, {s, e}} when is_binary(bin) and is_integer(s) and is_integer(e) and e > s ->
            safe_slice(bin, s, e - s)

          _ ->
            nil
        end,
      count: 1,
      v: 2
    }

    safe_telemetry(@boundary_drop_event, %{count: 1}, meta)
  end

  defp emit_chargram_violation(tok, detail, phrase, span) do
    meta = %{
      reason: :chargram,
      subreason: detail,
      token_index: tok_index(tok, 0),
      phrase: phrase,
      span: span,
      mw: tok_mw?(tok),
      count: 1,
      v: 2
    }

    safe_telemetry(@chargram_event, %{count: 1}, meta)

    if test_env?() do
      safe_telemetry(@chargram_tripwire_event, %{count: 1}, meta)
    end
  end

  # ── Span normalization + recovery (BYTE OFFSETS) ───────────────────────

  # Input may be {start,end} OR {start,len}. With sentence present, choose the one
  # whose slice matches the phrase (exact, then normalized).
  defp normalize_or_recover_span({s, x}, phrase, sent, cursor)
       when is_integer(s) and is_integer(x) and s >= 0 do
    phrase_bin = to_string(phrase || "")
    ph_len = byte_size(phrase_bin)

    cond do
      is_binary(sent) ->
        size = byte_size(sent)

        cand_end = if x > s and x <= size, do: {s, x}, else: nil
        cand_len = if x > 0 and s + x <= size, do: {s, s + x}, else: nil
        cand_ph = if ph_len > 0 and s + ph_len <= size, do: {s, s + ph_len}, else: nil

        candidates =
          [cand_end, cand_len, cand_ph]
          |> Enum.reject(&is_nil/1)
          |> Enum.uniq()

        winner =
          Enum.find(candidates, fn {a, b} ->
            safe_slice(sent, a, b - a) == phrase_bin
          end) ||
            Enum.find(candidates, fn {a, b} ->
              norm_phrase(safe_slice(sent, a, b - a)) == norm_phrase(phrase_bin)
            end) ||
            # Heuristic when sentence exists but phrase is empty/odd:
            (if x == ph_len, do: cand_len, else: nil) ||
            (if x > s and (x - s) == ph_len, do: cand_end, else: nil) ||
            cand_ph ||
            cand_len ||
            cand_end

        {winner, next_cursor(winner, cursor)}

      true ->
        # No sentence: prefer len-form when it matches phrase bytes; else keep permissive.
        span =
          cond do
            ph_len > 0 and x == ph_len -> {s, s + x}
            x < s and x > 0 -> {s, s + x}
            x > s -> {s, x}
            x > 0 -> {s, s + x}
            true -> nil
          end

        {span, next_cursor(span, cursor)}
    end
  end

  defp normalize_or_recover_span(_bad, phrase, sent, cursor) do
    phrase_bin = to_string(phrase || "")

    case recover_span(sent, phrase_bin, cursor) do
      {span, cur2} -> {span, cur2}
      nil -> {nil, cursor}
    end
  end

  defp next_cursor(nil, cursor), do: cursor
  defp next_cursor({_, e}, cursor) when is_integer(e), do: max(cursor, e)
  defp next_cursor(_, cursor), do: cursor

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

      re = Regex.compile!(Regex.escape(ph), "iu")

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
    cond do
      not is_binary(sent) or not is_integer(s) or not is_integer(len) or len <= 0 -> ""
      s < 0 or s + len > byte_size(sent) -> ""
      true -> binary_part(sent, s, len)
    end
  rescue
    _ -> ""
  end

  # ── Sorting (deterministic ties) ───────────────────────────────────────

  defp sort_by_span_then_index(list) when is_list(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, fn tok ->
        span = Map.get(tok, :span) || Map.get(tok, "span")
        {span_start(span), tok_index(tok, 9_999_999)}
      end)
    else
      list
    end
  end

  defp valid_span?(tok) when is_map(tok) do
    case Map.get(tok, :span) || Map.get(tok, "span") do
      {s, e} when is_integer(s) and is_integer(e) and s >= 0 and e > s -> true
      _ -> false
    end
  end

  defp valid_span?(_), do: false

  defp span_start({s, _e}) when is_integer(s), do: s
  defp span_start(_), do: 9_999_999

  # ── Token field helpers ────────────────────────────────────────────────

  defp tok_phrase(%{} = t) do
    Map.get(t, :phrase) ||
      Map.get(t, "phrase") ||
      Map.get(t, :norm) ||
      Map.get(t, "norm") ||
      Map.get(t, :lemma) ||
      Map.get(t, "lemma") ||
      Map.get(t, :text) ||
      Map.get(t, "text") ||
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

  defp tok_n(%{} = t) do
    case Map.get(t, :n) || Map.get(t, "n") do
      i when is_integer(i) and i > 0 -> i
      _ -> 1
    end
  end

  defp tok_n(_), do: 1

  defp tok_mw?(%{} = t) do
    mw_flag = Map.get(t, :mw) == true or Map.get(t, "mw") == true
    n = tok_n(t)

    id = to_string(Map.get(t, :id) || Map.get(t, "id") || "")
    pos = to_string(Map.get(t, :pos) || Map.get(t, "pos") || "")

    mw_flag or n > 1 or String.contains?(id, "|phrase|") or String.downcase(pos) == "phrase"
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

    norm1 =
      cond do
        is_binary(Map.get(t, :norm)) and String.trim(Map.get(t, :norm)) != "" ->
          Map.get(t, :norm)

        is_binary(Map.get(t, "norm")) and String.trim(Map.get(t, "norm")) != "" ->
          Map.get(t, "norm")

        phrase1 != "" ->
          norm_phrase(phrase1)

        true ->
          ""
      end

    t
    |> put_both(:phrase, phrase1)
    |> put_both(:norm, norm1)
  end

  defp ensure_phrase_and_norm(t, _), do: t

  defp maybe_put_span(t, {s, e})
       when is_map(t) and is_integer(s) and is_integer(e) and s >= 0 and e > s do
    t
    |> put_both(:span, {s, e})
  end

  defp maybe_put_span(t, _), do: t

  defp phrase_from_id(id) when is_binary(id) do
    id1 = String.trim(id)

    cond do
      id1 == "" ->
        nil

      String.contains?(id1, "|") ->
        case String.split(id1, "|", parts: 2) do
          [ph, _rest] -> String.trim(ph)
          _ -> nil
        end

      true ->
        nil
    end
  end

  defp phrase_from_id(_), do: nil

  defp maybe_put_mw_from_id(%{} = t) do
    id = to_string(Map.get(t, :id) || Map.get(t, "id") || "")

    if String.contains?(id, "|phrase|") do
      put_both(t, :mw, true)
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

  # ── Misc ───────────────────────────────────────────────────────────────

  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m), do: m
  defp mapify(other), do: %{phrase: to_string(other)}

  defp maybe_put(m, k, v) when is_map(m) do
    if Map.has_key?(m, k), do: Map.put(m, k, v), else: m
  end

  defp maybe_put(m, _k, _v), do: m

  defp put_both(%{} = m, k, v) when is_atom(k) do
    m
    |> Map.put(k, v)
    |> Map.put(Atom.to_string(k), v)
  end

  defp put_both(m, _k, _v), do: m

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
end
