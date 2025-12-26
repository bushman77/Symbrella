# lib/core/token_filters.ex
defmodule Core.TokenFilters do
  @moduledoc """
  Token filtering and sentence-span utilities used by the Core pipeline.

  This module provides three public utilities:

  * `prune/2` — drops obviously-invalid tokens (bad spans, too-small `n`, etc.) with a safety
    fallback to the original list if pruning would remove everything.
  * `rebuild_word_ngrams/2` — rebuilds a clean token list directly from the sentence by generating
    word n-grams with stable spans.
  * `keep_only_word_boundary_tokens/1` — verifies tokens are *span-backed* by the sentence: a token
    is kept only if its `:span` slice matches its `:phrase` after normalization (with a safety
    fallback if everything would be removed).

  ## Token expectations

  This module is intentionally tolerant of token shapes. It supports:

  * `%Core.Token{}` (preferred)
  * maps with the needed keys (e.g., `%{span: {start, len}, n: 2, phrase: "..."}`)

  The functions use only these keys:

  * `:span` — expected as `{start, len}` or sometimes `{start, stop}`; the code accepts either form
    in a best-effort way.
  * `:n` — n-gram size (integer), used by `prune/2`.
  * `:phrase` — token text, used by `keep_only_word_boundary_tokens/1`.
  * `:index` — optional; `rebuild_word_ngrams/2` will populate it only if the current `Core.Token`
    struct supports it.

  ## Sentence normalization and span units

  `rebuild_word_ngrams/2` and `keep_only_word_boundary_tokens/1` normalize the sentence by:

  * trimming leading/trailing whitespace
  * collapsing internal whitespace runs to a single space

  Spans produced/verified are grapheme indices suitable for `String.slice/3` on the normalized
  sentence.

  ## Tracing

  If the input has a `:trace` field (map or `%Core.SemanticInput{}`), each public function
  prepends an entry:

  * `%{stage: <atom>, meta: <map>, ts_ms: <integer>}`

  """

  alias Core.{SemanticInput, Token}

  @type opts :: keyword()

  # Compile-time feature detection (keeps this module compatible with older Token structs)
  @token_keys Token.__struct__() |> Map.from_struct() |> Map.keys() |> MapSet.new()
  @token_has_index MapSet.member?(@token_keys, :index)

  @doc ~S"""
  Prune a token list using a conservative safety filter.

  This function removes tokens that fail basic structural checks:

  * token must have a `:span` of `{start, len_or_stop}` with integer components
  * token must have `:n >= :min_n` (default `1`)
  * token must have a non-degenerate span:
    * accepts `{start, len}` when `len > 0`
    * also accepts `{start, stop}` when `stop > start` (best-effort support)

  **Safety guard:** if pruning would yield an empty list, the original token list is retained.

  The returned value preserves the input container shape:

  * `%Core.SemanticInput{}` in → `%Core.SemanticInput{}` out
  * map in → map out
  * anything else → returned unchanged

  ## Options

  * `:min_n` (pos_integer, default: `1`) — minimum n-gram size a token must have to be kept.

  ## Trace

  Adds a `:token_prune` trace entry with:

  * `:min_n` — used threshold
  * `:kept` — count of tokens in the final list
  * `:removed` — count of tokens that *would have been removed* (based on the pre-fallback kept set)
  * `:fallback?` — whether the safety guard kept the original list

  ## Examples

      iex> toks = [
      ...>   %{span: {0, 2}, n: 1, phrase: "hi"},
      ...>   %{span: {0, 0}, n: 1, phrase: "bad"},
      ...>   %{span: {3, 5}, n: 2, phrase: "there"}
      ...> ]
      iex> si = %{tokens: toks, trace: []}
      iex> out = Core.TokenFilters.prune(si, min_n: 2)
      iex> Enum.map(out.tokens, &Map.get(&1, :phrase))
      ["there"]
      iex> match?([%{stage: :token_prune, meta: %{min_n: 2, kept: 1}} | _], out.trace)
      true

  Demonstrating the safety guard (pruning would remove everything, so original tokens are kept):

      iex> out2 = Core.TokenFilters.prune(si, min_n: 3)
      iex> Enum.map(out2.tokens, &Map.get(&1, :phrase))
      ["hi", "bad", "there"]
      iex> [%{stage: :token_prune, meta: meta} | _] = out2.trace
      iex> meta.fallback?
      true

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

  @doc ~S"""
  Rebuild word n-grams directly from the sentence with stable, span-backed tokens.

  This is the “ground truth” token constructor for word-level n-grams:

  * normalizes the sentence (trim + collapse whitespace)
  * splits into whitespace-delimited words
  * generates n-grams up to `max_n` at each start word
  * emits tokens ordered by start word, then descending n (e.g., 3-gram, 2-gram, 1-gram)
  * assigns a stable `:index` in emission order *only if* the current `Core.Token` struct supports it

  Each emitted token includes:

  * `:phrase` — the n-gram text (joined by single spaces)
  * `:span` — `{start, len}` over the normalized sentence (grapheme indices)
  * `:n` — the n-gram size
  * `:mw` — `true` for multiword n-grams (`n > 1`)
  * `:instances` — an empty list (placeholder for later enrichment stages)

  ## Trace

  Adds a `:rebuild_word_ngrams` trace entry with:

  * `:max_n`
  * `:token_count`

  ## Examples

      iex> si = %{sentence: "  hi   there  ", tokens: [], trace: []}
      iex> out = Core.TokenFilters.rebuild_word_ngrams(si, 3)
      iex> out.sentence
      "hi there"
      iex> Enum.map(out.tokens, &Map.get(&1, :phrase))
      ["hi there", "hi", "there"]
      iex> Enum.map(out.tokens, &Map.get(&1, :span))
      [{0, 8}, {0, 2}, {3, 5}]
      iex> match?([%{stage: :rebuild_word_ngrams, meta: %{max_n: 3}} | _], out.trace)
      true

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

  @doc ~S"""
  Keep only tokens whose `:span` matches their sentence slice (after normalization).

  This is a *span-backed token verification* pass. A token is kept when:

  * the sentence is normalized (trim + collapse whitespace)
  * the token has a valid integer `:span` of `{start, len_or_stop}` with `start >= 0`
  * slicing the normalized sentence at that span yields text that normalizes to the same value
    as the token’s `:phrase`

  **Safety guard:** if filtering would yield an empty list, the original token list is retained.

  Notes:

  * This function verifies “token text matches the exact sentence slice at its span.”
    It does not explicitly check “word boundary” characters before/after the span.
  * Spans are interpreted as `{start, len}` first; if that fails, `{start, stop}` is attempted.

  ## Trace

  Adds a `:token_boundary` trace entry with:

  * `:kept` — count of tokens in the final list
  * `:removed` — count of tokens that *would have been removed* (based on the pre-fallback kept set)
  * `:fallback?` — whether the safety guard kept the original list

  ## Examples

      iex> toks = [
      ...>   %{span: {0, 2}, phrase: "hi"},
      ...>   %{span: {2, 5}, phrase: "there"} # wrong start; would not match slice " there"
      ...> ]
      iex> si = %{sentence: "hi there", tokens: toks, trace: []}
      iex> out = Core.TokenFilters.keep_only_word_boundary_tokens(si)
      iex> Enum.map(out.tokens, &Map.get(&1, :phrase))
      ["hi"]
      iex> match?([%{stage: :token_boundary, meta: %{kept: 1, fallback?: false}} | _], out.trace)
      true

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
