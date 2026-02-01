defmodule Brain.LIFG.BoundaryGuard do
  @moduledoc """
  Slate-level token guard for the LIFG path.

  Goals:
  - Accepts both list input and SI-like maps (`%{tokens: [...]}` or %{"tokens" => [...]})
  - Normalizes spans to `{start, end}` (end is exclusive) in **byte offsets**
  - Handles mixed span units (byte vs grapheme) by matching against the sentence
  - Recovers missing/broken spans by searching the original sentence left-to-right
  - Drops classic char-gram symptoms:
      * non-mw tokens whose phrase contains whitespace
      * tokens with unrecoverable spans (rare; we try hard to synthesize)
  - Enforces word-boundary alignment on `[start, end)` when sentence is present
    (unless mw: true)
  - Emits `[:brain, :lifg, :stage1, :chargram_violation]` with `reason: :chargram`
  - Ensures output tokens are sorted by span start
  """

  @chargram_event [:brain, :lifg, :stage1, :chargram_violation]
  @boundary_drop_event [:brain, :lifg, :stage1, :boundary_drop]

  @type token :: map()
  @type si_like :: map()

  @spec sanitize(si_like | [token]) :: si_like | [token]
  def sanitize(%{tokens: toks} = si) when is_list(toks) do
    sent = Map.get(si, :sentence) || Map.get(si, "sentence")
    toks2 = sanitize(toks, sent)

    si
    |> Map.put(:tokens, toks2)
    |> maybe_put_string_tokens(toks2)
  end

  def sanitize(%{"tokens" => toks} = si) when is_list(toks) do
    sent = Map.get(si, :sentence) || Map.get(si, "sentence")
    toks2 = sanitize(toks, sent)

    si
    |> Map.put("tokens", toks2)
    |> maybe_put_atom_tokens(toks2)
  end

  def sanitize(tokens) when is_list(tokens), do: sanitize(tokens, nil)
  def sanitize(other), do: other

  defp maybe_put_string_tokens(si, toks2) do
    if Map.has_key?(si, "tokens"), do: Map.put(si, "tokens", toks2), else: si
  end

  defp maybe_put_atom_tokens(si, toks2) do
    if Map.has_key?(si, :tokens), do: Map.put(si, :tokens, toks2), else: si
  end

  @spec sanitize([token], String.t() | nil) :: [token]
  def sanitize(tokens, sentence) when is_list(tokens) do
    sent = if is_binary(sentence), do: sentence, else: nil

    {toks2, _cursor_bytes} =
      tokens
      |> Enum.map(&mapify/1)
      |> Enum.map_reduce(0, fn tok, cursor ->
        phrase = tok_phrase(tok) || ""
        span0 = Map.get(tok, :span) || Map.get(tok, "span")

        {span1, cursor2} = normalize_or_recover_span(span0, phrase, sent, cursor)

        tok =
          tok
          |> put_phrase_norm(phrase)
          |> put_span(span1)

        {tok, cursor2}
      end)

    toks2
    |> Enum.reduce([], fn tok, acc ->
      mw? = tok_mw?(tok)
      phrase = tok_phrase(tok) || ""
      span = Map.get(tok, :span) || Map.get(tok, "span")

      cond do
        # cross-word non-mw => char-gram symptom
        not mw? and String.match?(phrase, ~r/\s/u) ->
          emit_chargram_violation(tok, :cross_word)
          acc

        not is_tuple(span) ->
          emit_chargram_violation(tok, :bad_span)
          acc

        boundary_ok?(sent, span, mw?) ->
          [tok | acc]

        true ->
          emit_boundary_drop(tok, sent)
          acc
      end
    end)
    |> Enum.reverse()
    |> Enum.sort_by(&span_start/1)
  end

  # ──────────────────────────────────────────────────────────
  # Token field helpers
  # ──────────────────────────────────────────────────────────

  defp tok_phrase(%{} = t) do
    Map.get(t, :phrase) ||
      Map.get(t, "phrase") ||
      Map.get(t, :norm) ||
      Map.get(t, "norm") ||
      Map.get(t, :lemma) ||
      Map.get(t, "lemma") ||
      Map.get(t, :text) ||
      Map.get(t, "text")
  end

  defp tok_phrase(_), do: nil

  defp tok_mw?(%{} = t) do
    Map.get(t, :mw) == true or Map.get(t, "mw") == true or tok_n(t) > 1
  end

  defp tok_mw?(_), do: false

  defp tok_n(%{} = t) do
    case Map.get(t, :n) || Map.get(t, "n") do
      i when is_integer(i) -> i
      _ -> 1
    end
  end

  defp tok_n(_), do: 1

  defp put_phrase_norm(%{} = t, phrase) do
    p =
      cond do
        is_binary(phrase) and phrase != "" -> phrase
        is_binary(Map.get(t, :phrase)) -> Map.get(t, :phrase)
        is_binary(Map.get(t, "phrase")) -> Map.get(t, "phrase")
        true -> ""
      end

    n0 = Map.get(t, :norm) || Map.get(t, "norm")
    n = if is_binary(n0) and n0 != "", do: n0, else: down(p)

    t
    |> Map.put(:phrase, p)
    |> Map.put("phrase", p)
    |> Map.put(:norm, n)
    |> Map.put("norm", n)
  end

  defp put_phrase_norm(t, _), do: t

  defp put_span(%{} = t, {s, e}) when is_integer(s) and is_integer(e) do
    t
    |> Map.put(:span, {s, e})
    |> Map.put("span", {s, e})
  end

  defp put_span(%{} = t, nil) do
    t |> Map.delete(:span) |> Map.delete("span")
  end

  defp put_span(t, _), do: t

  # ──────────────────────────────────────────────────────────
  # Span normalization + recovery (output is BYTE OFFSETS)
  # ──────────────────────────────────────────────────────────

  defp normalize_or_recover_span({s, x}, phrase, sent, cursor_bytes)
       when is_integer(s) and is_integer(x) and s >= 0 do
    phrase = to_string(phrase || "")

    cond do
      not is_binary(sent) ->
        # Without sentence, assume values are bytes and prefer len-form.
        span =
          cond do
            x > 0 and x < s -> {s, s + x}
            x > 0 and x >= s -> {s, s + x}
            true -> nil
          end

        {span, next_cursor(span, cursor_bytes)}

      true ->
        # With sentence, spans may be:
        #  - {start,end} in bytes
        #  - {start,len} in bytes
        #  - {start,end} in graphemes
        #  - {start,len} in graphemes
        #
        # We generate candidates in both unit systems, convert grapheme candidates to bytes,
        # and pick the first whose slice matches the phrase (case/space-insensitive).
        span =
          pick_best_span(sent, phrase, s, x) ||
            # If we can't match, still choose a sane candidate (bytes first) so we don't explode.
            first_sane_span(sent, phrase, s, x)

        {span, next_cursor(span, cursor_bytes)}
    end
  end

  defp normalize_or_recover_span(_bad, phrase, sent, cursor_bytes) do
    phrase = to_string(phrase || "")

    case recover_span(sent, phrase, cursor_bytes) do
      {span, cursor2} ->
        {span, cursor2}

      nil ->
        # Monotonic fallback span (bytes). Clamp to sentence bytes if available.
        if is_binary(phrase) and phrase != "" and is_integer(cursor_bytes) and cursor_bytes >= 0 do
          e0 = cursor_bytes + byte_size(phrase)

          span =
            if is_binary(sent) do
              len = byte_size(sent)
              e = min(e0, len)
              if e > cursor_bytes, do: {cursor_bytes, e}, else: nil
            else
              {cursor_bytes, e0}
            end

          {span, next_cursor(span, cursor_bytes)}
        else
          {nil, cursor_bytes}
        end
    end
  end

  defp pick_best_span(sent, phrase, s, x) do
    byte_len = byte_size(sent)
    g_len = String.length(sent)
    ph_blen = byte_size(phrase)
    ph_glen = if String.valid?(phrase), do: String.length(phrase), else: ph_blen

    candidates =
      [
        {:byte, {s, x}},          # maybe end-form
        {:byte, {s, s + x}},      # maybe len-form
        {:byte, {s, s + ph_blen}},# phrase length in bytes

        {:gr, {s, x}},            # maybe end-form in graphemes
        {:gr, {s, s + x}},        # maybe len-form in graphemes
        {:gr, {s, s + ph_glen}}   # phrase length in graphemes
      ]
      |> Enum.uniq()

    Enum.find_value(candidates, fn
      {:byte, {ss, ee}} ->
        if ss >= 0 and ee > ss and ee <= byte_len and slice_matches?(sent, ss, ee, phrase) do
          {ss, ee}
        else
          nil
        end

      {:gr, {gs, ge}} ->
        if gs >= 0 and ge > gs and ge <= g_len do
          {bs, be} = gspan_to_bspan(sent, {gs, ge})

          if bs >= 0 and be > bs and be <= byte_len and slice_matches?(sent, bs, be, phrase) do
            {bs, be}
          else
            nil
          end
        else
          nil
        end
    end)
  end

  defp first_sane_span(sent, phrase, s, x) do
    byte_len = byte_size(sent)
    g_len = String.length(sent)
    ph_blen = byte_size(phrase)
    ph_glen = if String.valid?(phrase), do: String.length(phrase), else: ph_blen

    byte_candidates =
      [
        {s, x},
        {s, s + x},
        {s, s + ph_blen}
      ]
      |> Enum.uniq()
      |> Enum.filter(fn {ss, ee} -> ss >= 0 and ee > ss and ee <= byte_len end)

    case byte_candidates do
      [h | _] ->
        h

      [] ->
        gr_candidates =
          [
            {s, x},
            {s, s + x},
            {s, s + ph_glen}
          ]
          |> Enum.uniq()
          |> Enum.filter(fn {gs, ge} -> gs >= 0 and ge > gs and ge <= g_len end)

        case gr_candidates do
          [h | _] ->
            {bs, be} = gspan_to_bspan(sent, h)
            if bs >= 0 and be > bs and be <= byte_len, do: {bs, be}, else: nil

          [] ->
            nil
        end
    end
  end

  defp slice_matches?(sent, s, e, phrase) do
    down(safe_slice_bytes(sent, s, e)) == down(phrase)
  end

  defp gspan_to_bspan(sent, {gs, ge}) do
    {gpos_to_bpos(sent, gs), gpos_to_bpos(sent, ge)}
  end

  defp gpos_to_bpos(_sent, gpos) when not is_integer(gpos) or gpos <= 0, do: 0

  defp gpos_to_bpos(sent, gpos) do
    # byte offset of the first gpos graphemes
    byte_size(String.slice(sent, 0, gpos))
  rescue
    _ -> 0
  end

  defp next_cursor(nil, cursor), do: cursor
  defp next_cursor({_, e}, cursor) when is_integer(e), do: max(cursor, e)
  defp next_cursor(_, cursor), do: cursor

  # Recover span in the ORIGINAL sentence. This keeps offsets correct.
  defp recover_span(sent, phrase, cursor_bytes)
       when is_binary(sent) and is_binary(phrase) and is_integer(cursor_bytes) and cursor_bytes >= 0 do
    ph = String.trim(phrase)

    if ph == "" do
      nil
    else
      start_at = min(max(cursor_bytes, 0), byte_size(sent))

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

  defp safe_slice_bytes(sent, s, e) do
    cond do
      not is_binary(sent) or not is_integer(s) or not is_integer(e) ->
        ""

      s < 0 or e <= s or e > byte_size(sent) ->
        ""

      true ->
        slice = binary_part(sent, s, e - s)
        if String.valid?(slice), do: slice, else: ""
    end
  rescue
    _ -> ""
  end

  # ──────────────────────────────────────────────────────────
  # Boundary checks (BYTE OFFSETS)
  # ──────────────────────────────────────────────────────────

  defp boundary_ok?(_sent, _span, true), do: true
  defp boundary_ok?(nil, _span, _mw?), do: true

  defp boundary_ok?(sent, {s, e}, _mw?)
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

  defp boundary_ok?(_, _, _), do: false

  defp word_byte?(b) when is_integer(b) do
    (b >= ?a and b <= ?z) or (b >= ?A and b <= ?Z) or (b >= ?0 and b <= ?9) or b == ?_
  end

  defp word_byte?(_), do: false

  defp span_start(tok) do
    case Map.get(tok, :span) || Map.get(tok, "span") do
      {s, _e} when is_integer(s) -> s
      _ -> 9_999_999
    end
  end

  defp down(s) when is_binary(s) do
    if String.valid?(s) do
      s
      |> String.downcase()
      |> String.trim()
      |> String.replace(~r/\s+/u, " ")
    else
      ascii_norm(s)
    end
  end

  defp down(_), do: ""

  defp ascii_norm(bin) when is_binary(bin) do
    {acc, _last_space?} =
      bin
      |> :binary.bin_to_list()
      |> Enum.reduce({[], true}, fn b, {acc, last_space?} ->
        cond do
          b in [?\s, ?\t, ?\n, ?\r] ->
            if last_space?, do: {acc, true}, else: {[?\s | acc], true}

          b >= ?A and b <= ?Z ->
            {[b + 32 | acc], false}

          true ->
            {[b | acc], false}
        end
      end)

    out = Enum.reverse(acc)

    out =
      case out do
        [] -> []
        _ -> if List.last(out) == ?\s, do: Enum.drop(out, -1), else: out
      end

    :erlang.list_to_binary(out)
  end

  defp emit_chargram_violation(tok, detail) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(
        @chargram_event,
        %{count: 1},
        %{
          reason: :chargram,
          subreason: detail,
          token: tok_phrase(tok),
          span: Map.get(tok, :span) || Map.get(tok, "span")
        }
      )
    else
      :ok
    end
  end

  defp emit_boundary_drop(tok, sent) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(
        @boundary_drop_event,
        %{count: 1},
        %{
          token: tok_phrase(tok),
          span: Map.get(tok, :span) || Map.get(tok, "span"),
          slice:
            case {sent, Map.get(tok, :span) || Map.get(tok, "span")} do
              {bin, {s, e}} when is_binary(bin) and is_integer(s) and is_integer(e) ->
                safe_slice_bytes(bin, s, e)

              _ ->
                nil
            end
        }
      )
    else
      :ok
    end
  end

  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m), do: m
  defp mapify(other), do: %{phrase: to_string(other)}
end

