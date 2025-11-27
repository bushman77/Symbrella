defmodule Core.TokenFilters do
  @moduledoc """
  Token filtering + span/wordgram utilities for the Core pipeline.

  Hosts:
    • prune/2
    • rebuild_word_ngrams/2
    • keep_only_word_boundary_tokens/1
  """

  alias Core.{SemanticInput, Token}

  @type opts :: keyword()

  # Compile-time feature detection (keeps this module compatible with older Token structs)
  @token_keys Token.__struct__() |> Map.from_struct() |> Map.keys() |> MapSet.new()
  @token_has_index MapSet.member?(@token_keys, :index)

  @doc """
  Prunes tokens with a safety guard: if pruning would yield an empty list,
  keep the original tokens.
  """
  @spec prune(SemanticInput.t() | map(), opts()) :: SemanticInput.t() | map()
  def prune(%SemanticInput{tokens: toks} = si, opts) when is_list(toks) do
    {final, kept} = prune_tokens(toks, opts)

    si
    |> put_tokens(final)
    |> put_trace_any(:token_prune, %{
      min_n: Keyword.get(opts, :min_n, 1),
      kept: length(final),
      removed: max(length(toks) - kept, 0),
      fallback?: final == toks and toks != []
    })
  end

  def prune(%{} = si, opts) do
    toks = Map.get(si, :tokens, [])
    {final, kept} = prune_tokens(toks, opts)

    si
    |> Map.put(:tokens, final)
    |> put_trace_any(:token_prune, %{
      min_n: Keyword.get(opts, :min_n, 1),
      kept: length(final),
      removed: max(length(toks) - kept, 0),
      fallback?: final == toks and toks != []
    })
  end

  def prune(other, _opts), do: other

  defp prune_tokens(toks, opts) when is_list(toks) do
    min_n = Keyword.get(opts, :min_n, 1)

    kept =
      toks
      |> Enum.filter(fn tok ->
        span = tok_span(tok)
        n = tok_n(tok, 1)

        case span do
          {s, k} when is_integer(s) and is_integer(k) and n >= min_n ->
            # Accept either {start, stop} (k > s) or {start, len} (k > 0)
            k > s or k > 0

          _ ->
            false
        end
      end)

    final = if kept == [], do: toks, else: kept
    {final, length(kept)}
  end

  defp prune_tokens(_toks, _opts), do: {[], 0}

  @doc """
  Rebuild word n-grams from the sentence with stable, span-backed tokens.

  Generates n-grams up to `max_n`.

  NOTE on spans:
    • spans are `{start, len}` **grapheme indices** over the normalized sentence.
  """
  @spec rebuild_word_ngrams(SemanticInput.t() | map(), pos_integer()) :: SemanticInput.t() | map()
  def rebuild_word_ngrams(%SemanticInput{sentence: s} = si, max_n)
      when is_binary(s) and is_integer(max_n) and max_n > 0 do
    {s_norm, tokens} = build_ngrams(s, max_n)

    si
    |> Map.put(:sentence, s_norm)
    |> Map.put(:tokens, tokens)
    |> put_trace_any(:rebuild_word_ngrams, %{max_n: max_n, token_count: length(tokens)})
  end

  def rebuild_word_ngrams(%{} = si, max_n) when is_integer(max_n) and max_n > 0 do
    s = Map.get(si, :sentence)

    if is_binary(s) do
      {s_norm, tokens} = build_ngrams(s, max_n)

      si
      |> Map.put(:sentence, s_norm)
      |> Map.put(:tokens, tokens)
      |> put_trace_any(:rebuild_word_ngrams, %{max_n: max_n, token_count: length(tokens)})
    else
      si
    end
  end

  def rebuild_word_ngrams(si, _max_n), do: si

  defp build_ngrams(s, max_n) do
    s_norm = normalize_sentence(s)
    words = if s_norm == "", do: [], else: String.split(s_norm, " ")
    wlen = length(words)

    tokens =
      if wlen == 0 do
        []
      else
        starts = word_starts(words)

        for i <- 0..(wlen - 1),
            n <- min(max_n, wlen - i)..1//-1 do
          phrase = words |> Enum.slice(i, n) |> Enum.join(" ")
          start = Enum.at(starts, i, 0)
          len = String.length(phrase)

          new_token(nil, {start, len}, n, phrase, n > 1)
        end
        |> Enum.with_index()
        |> Enum.map(fn {t, idx} -> set_token_index(t, idx) end)
      end

    {s_norm, tokens}
  end

  @doc """
  Drops tokens whose `span` does not match the sentence slice (after normalization).

  Safety guard: if filtering yields `[]`, we keep the original tokens.
  """
  @spec keep_only_word_boundary_tokens(SemanticInput.t() | map()) :: SemanticInput.t() | map()
  def keep_only_word_boundary_tokens(%SemanticInput{sentence: s, tokens: toks} = si)
      when is_binary(s) and is_list(toks) do
    {s_norm, final, kept_count} = boundary_filter(s, toks)

    si
    |> Map.put(:sentence, s_norm)
    |> put_tokens(final)
    |> put_trace_any(:token_boundary, %{
      kept: length(final),
      removed: max(length(toks) - kept_count, 0),
      fallback?: final == toks and toks != []
    })
  end

  def keep_only_word_boundary_tokens(%{} = si) do
    s = Map.get(si, :sentence)
    toks = Map.get(si, :tokens, [])

    if is_binary(s) and is_list(toks) do
      {s_norm, final, kept_count} = boundary_filter(s, toks)

      si
      |> Map.put(:sentence, s_norm)
      |> Map.put(:tokens, final)
      |> put_trace_any(:token_boundary, %{
        kept: length(final),
        removed: max(length(toks) - kept_count, 0),
        fallback?: final == toks and toks != []
      })
    else
      si
    end
  end

  def keep_only_word_boundary_tokens(other), do: other

  defp boundary_filter(s, toks) do
    s_norm = normalize_sentence(s)

    kept =
      Enum.filter(toks, fn tok ->
        phrase = tok_phrase(tok) |> norm_text()

        case tok_span(tok) do
          {a, b} when is_integer(a) and is_integer(b) and a >= 0 ->
            # Prefer {start,len}; fall back to {start,stop}.
            cond do
              b > 0 and norm_text(String.slice(s_norm, a, b) || "") == phrase -> true
              b > a and norm_text(String.slice(s_norm, a, b - a) || "") == phrase -> true
              true -> false
            end

          _ ->
            false
        end
      end)

    final = if kept == [], do: toks, else: kept
    {s_norm, final, length(kept)}
  end

  # ───────────────────────── tiny helpers ─────────────────────────

  defp normalize_sentence(s) when is_binary(s) do
    s
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp word_starts(words) when is_list(words) do
    words
    |> Enum.reduce({[], 0}, fn w, {acc, pos} ->
      {[pos | acc], pos + String.length(w) + 1}
    end)
    |> elem(0)
    |> Enum.reverse()
  end

  # Avoid compile warnings if Core.Text.norm/1 isn't present:
  defp norm_text(v) do
    cond do
      Code.ensure_loaded?(Core.Text) and function_exported?(Core.Text, :norm, 1) ->
        apply(Core.Text, :norm, [v])

      Code.ensure_loaded?(Core.Text) and function_exported?(Core.Text, :normalize, 1) ->
        apply(Core.Text, :normalize, [v])

      true ->
        v
        |> to_string()
        |> String.downcase()
        |> String.trim()
        |> String.replace(~r/^\p{P}+/u, "")
        |> String.replace(~r/\p{P}+$/u, "")
        |> String.replace(~r/\s+/u, " ")
    end
  end

  # Build a Token struct but only with keys that exist in the current Token defstruct.
  defp new_token(index, span, n, phrase, mw) do
    base =
      %{
        index: index,
        span: span,
        n: n,
        phrase: phrase,
        mw: mw,
        instances: []
      }

    # If Token doesn't support :index, drop it cleanly.
    base2 = if @token_has_index, do: base, else: Map.delete(base, :index)

    default = Token.__struct__() |> Map.from_struct()
    struct(Token, Map.take(base2, Map.keys(default)))
  end

  # No struct-update syntax (prevents "unknown key :index" + typing warnings)
  defp set_token_index(%Token{} = t, idx) do
    if @token_has_index do
      Map.put(t, :index, idx)
    else
      t
    end
  end

  defp set_token_index(%{} = t, idx), do: Map.put(t, :index, idx)
  defp set_token_index(t, _idx), do: t

  defp tok_span(%Token{span: span}), do: span
  defp tok_span(%{span: span}), do: span
  defp tok_span(_), do: nil

  defp tok_n(%Token{n: n}, _default), do: n
  defp tok_n(%{n: n}, _default) when is_integer(n), do: n
  defp tok_n(_tok, default), do: default

  defp tok_phrase(%Token{phrase: p}), do: p
  defp tok_phrase(%{phrase: p}) when is_binary(p), do: p
  defp tok_phrase(p) when is_binary(p), do: p
  defp tok_phrase(_), do: ""

  defp put_tokens(%SemanticInput{} = si, toks), do: %SemanticInput{si | tokens: toks}
  defp put_tokens(%{} = si, toks), do: Map.put(si, :tokens, toks)
  defp put_tokens(si, _toks), do: si

  defp put_trace_any(%SemanticInput{} = si, stage, meta) do
    ts_ms = System.system_time(:millisecond)
    tr = Map.get(si, :trace) || []
    trace = [%{stage: stage, meta: meta, ts_ms: ts_ms} | tr]
    %SemanticInput{si | trace: trace}
  end

  defp put_trace_any(%{} = si, stage, meta) do
    ts_ms = System.system_time(:millisecond)
    tr = Map.get(si, :trace) || []
    trace = [%{stage: stage, meta: meta, ts_ms: ts_ms} | tr]
    Map.put(si, :trace, trace)
  end

  defp put_trace_any(other, _stage, _meta), do: other
end

