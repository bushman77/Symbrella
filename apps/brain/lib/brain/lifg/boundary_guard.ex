defmodule Brain.LIFG.BoundaryGuard do
  @moduledoc """
  Slate-level token guard for the LIFG path.

  Goals:
  - Accepts both list input and SI-like maps (`%{tokens: [...]}` or %{"tokens" => [...]})
  - Normalizes spans to `{start, end}` (end is exclusive, byte offsets)
  - Recovers missing/broken spans by searching the sentence left-to-right (when available)
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

    {toks2, _cursor} =
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
        (not mw?) and String.match?(phrase, ~r/\s/u) ->
          emit_chargram_violation(tok, :cross_word)
          acc

        not is_tuple(span) ->
          emit_chargram_violation(tok, :bad_span)
          acc

        boundary_ok?(sent, span, mw?) ->
          [tok | acc]

        true ->
          # boundary violation (non-mw)
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
    # Keep consistent shape: if somehow nil slips through, don't crash callers.
    t |> Map.delete(:span) |> Map.delete("span")
  end

  defp put_span(t, _), do: t

  # ──────────────────────────────────────────────────────────
  # Span normalization + recovery
  # ──────────────────────────────────────────────────────────

  # Normalizes span to {start,end_exclusive}.
  # Accepts either {start,end} OR {start,len}. With sentence present, picks the
  # interpretation whose slice matches phrase (case/space-insensitive).
  defp normalize_or_recover_span({s, x}, phrase, sent, cursor)
       when is_integer(s) and is_integer(x) and s >= 0 do
    phrase = to_string(phrase || "")

    cond do
      not is_binary(sent) ->
        # No sentence: heuristic (prefer len-form when it matches phrase length)
        phrase_len = byte_size(phrase)

        span =
          cond do
            x > 0 and x == phrase_len -> {s, s + x}
            x > s and (x - s) == phrase_len -> {s, x}
            x > 0 -> {s, s + x}
            true -> nil
          end

        {span, next_cursor(span, cursor)}

      true ->
        cand_end = {s, x}
        cand_len = {s, s + x}
        cand_ph  = {s, s + byte_size(phrase)}

        candidates =
          [cand_end, cand_len, cand_ph]
          |> Enum.uniq()
          |> Enum.filter(&valid_span_bytes?(sent, &1))

        winner =
          Enum.find(candidates, fn {ss, ee} ->
            down(safe_slice_bytes(sent, ss, ee)) == down(phrase)
          end) ||
            Enum.find(candidates, fn {_ss, ee} -> ee == s + x end) ||
            List.first(candidates)

        {winner, next_cursor(winner, cursor)}
    end
  end

  defp normalize_or_recover_span(_bad, phrase, sent, cursor) do
    phrase = to_string(phrase || "")

    case recover_span(sent, phrase, cursor) do
      {span, cursor2} ->
        {span, cursor2}

      nil ->
        # Monotonic fallback span (keeps tests happy + keeps pipeline stable)
        if is_binary(phrase) and phrase != "" and is_integer(cursor) and cursor >= 0 do
          e = cursor + byte_size(phrase)
          {{cursor, e}, e}
        else
          {nil, cursor}
        end
    end
  end

  defp next_cursor(nil, cursor), do: cursor
  defp next_cursor({_, e}, cursor) when is_integer(e), do: max(cursor, e)
  defp next_cursor(_, cursor), do: cursor

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

  defp valid_span_bytes?(sent, {s, e})
       when is_binary(sent) and is_integer(s) and is_integer(e) do
    s >= 0 and e > s and e <= byte_size(sent)
  end

  defp valid_span_bytes?(_, _), do: false

  # ──────────────────────────────────────────────────────────
  # Boundary checks (byte-based; spans are byte offsets)
  # ──────────────────────────────────────────────────────────

  defp boundary_ok?(_sent, {s, e}, true)
       when is_integer(s) and is_integer(e) do
    # mw tokens are allowed to cross word boundaries; just require sane span
    e > s and s >= 0
  end

  defp boundary_ok?(nil, _span, _mw?), do: true

  defp boundary_ok?(sent, {s, e}, _mw?)
       when is_binary(sent) and is_integer(s) and is_integer(e) do
    len = byte_size(sent)

    cond do
      s < 0 or e < 0 or e > len or e <= s ->
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

defp safe_slice_bytes(sent, s, e) do
  cond do
    not valid_span_bytes?(sent, {s, e}) ->
      ""

    true ->
      slice = binary_part(sent, s, e - s)
      if String.valid?(slice), do: slice, else: ""
  end
rescue
  _ -> ""
end

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

  # IMPORTANT: tests want meta.reason == :chargram (NOT :cross_word / :span_mismatch)
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

  # ---- internals -----------------------------------------------------------

  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m), do: m
  defp mapify(other), do: %{phrase: to_string(other)}
end

