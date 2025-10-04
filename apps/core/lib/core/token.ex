defmodule Core.Token do
  @moduledoc """
  Core.Token carries the token struct **and** the tokenizer.

  Emits **word-grams only** with **word-index** spans by default (end-exclusive: `{i, i+n}`).
  Per start index, order is **longest → shortest**.

  NEW:
    • `span_mode: :words | :chars` (default `:words`). When `:chars`, spans are character
      indices over the normalized sentence (end-exclusive: `{start, end}`).
    • `to_char_spans/1,2` — convert existing word spans to char spans.
    • `check_span_invariants/1` — optional invariant validation.

  `tokenize/2` returns a %Core.SemanticInput{sentence, tokens}.
  """

  @enforce_keys [:phrase, :span, :n]
  defstruct phrase: "",
            span: {0, 0},
            mw: false,
            source: nil,
            instances: [],
            n: 1

  @type t :: %__MODULE__{
          phrase: String.t(),
          # When span_mode=:words -> word-index span (end-exclusive): {i, i+n}
          # When span_mode=:chars -> character-index span (end-exclusive): {start, end}
          span: {non_neg_integer(), non_neg_integer()},
          mw: boolean(),
          instances: list(),
          n: pos_integer(),
          source: any()
        }

  @type span_mode :: :words | :chars

  @doc """
  Tokenize a sentence into **all contiguous word n-grams**.

  Options:
    * `:max_wordgram_n` (pos int, default 3) — cap n-gram length
    * `:span_mode`      (`:words` | `:chars`, default `:words`)
  """
  @spec tokenize(String.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(sentence, opts \\ []) when is_binary(sentence) do
    max_n = Keyword.get(opts, :max_wordgram_n, 3) |> max(1)
    span_mode = Keyword.get(opts, :span_mode, :words)

    s =
      sentence
      |> to_string()
      |> String.trim()
      |> String.replace(~r/\s+/u, " ")

    words = if s == "", do: [], else: String.split(s, " ")

    tokens =
      case words do
        [] -> []
        _ -> build_wordgrams(words, max_n)
      end

    tokens =
      case span_mode do
        :words -> tokens
        :chars -> to_char_spans(tokens, s)
      end

    %Core.SemanticInput{sentence: s, tokens: tokens}
  end

  # ---------- helpers ----------

  # Build all word-grams with word-index spans {i, i+n}, longest→shortest per start.
  defp build_wordgrams(words, max_n) do
    k = length(words)

    0..(k - 1)
    |> Enum.flat_map(fn i ->
      max_here = min(max_n, k - i)

      for n <- max_here..1//-1 do
        %__MODULE__{
          phrase: Enum.slice(words, i, n) |> Enum.join(" "),
          span: {i, i + n},
          mw: n > 1,
          instances: [],
          n: n
        }
      end
    end)
  end

  @doc """
  Convert a `%Core.SemanticInput{}` whose tokens use word-index spans `{i, i+n}`
  into **character spans** over the input sentence.

  Returns an updated `%Core.SemanticInput{}`.
  """
  @spec to_char_spans(Core.SemanticInput.t()) :: Core.SemanticInput.t()
  def to_char_spans(%Core.SemanticInput{sentence: s, tokens: tokens} = si) do
    %{si | tokens: to_char_spans(tokens, s)}
  end

  @doc """
  Convert a list of `%Core.Token{}` (word-index spans) into **character spans** `{start, end}`
  over the given normalized sentence `s`. End is exclusive.

  This is precise for your normalized splitter (single spaces between words).
  """
  @spec to_char_spans([t], String.t()) :: [t]
  def to_char_spans(tokens, s) when is_list(tokens) and is_binary(s) do
    words = if s == "", do: [], else: String.split(s, " ")
    {starts, ends} = word_boundaries(words)

    Enum.map(tokens, fn %__MODULE__{span: {i, j}, n: n} = tok ->
      # n == j - i by construction for word-index spans
      if n <= 0 or j < i do
        # Defensive: produce a zero-length span at 0 to avoid crashes; caller can validate later.
        %__MODULE__{tok | span: {0, 0}}
      else
        start_char = Enum.at(starts, i, 0)
        last_idx = j - 1
        end_char = Enum.at(ends, last_idx, start_char)

        %__MODULE__{tok | span: {start_char, end_char}}
      end
    end)
    # Keep deterministic order by character start
    |> Enum.sort_by(fn %__MODULE__{span: {start, _}} -> start end)
  end

  # Compute char start/end for each word under the normalized splitter:
  # starts[w_i] = char index of the first character of word_i
  # ends[w_i]   = char index *after* the last character of word_i (end-exclusive)
  defp word_boundaries(words) do
    len = length(words)

    {_cur, starts_acc, ends_acc} =
      Enum.reduce(Enum.with_index(words), {0, [], []}, fn {w, idx}, {cur, sacc, eacc} ->
        s = cur
        e = s + String.length(w)
        # Advance by a single space if not the last word (we normalized spaces)
        next = if idx < len - 1, do: e + 1, else: e
        {next, [s | sacc], [e | eacc]}
      end)

    {Enum.reverse(starts_acc), Enum.reverse(ends_acc)}
  end

  @doc """
  Optional invariant check:
    • spans are non-decreasing by start
    • each token's phrase equals String.slice(sentence, start, end-start)

  Returns:
    `{:ok, si}` on success
    `{:error, failures}` where failures is a list of maps describing the bad tokens
  """
# in Core.Token

@spec check_span_invariants(Core.SemanticInput.t()) ::
        {:ok, Core.SemanticInput.t()} | {:error, [map()]}
def check_span_invariants(%Core.SemanticInput{sentence: s, tokens: tokens} = si) do
  {failures, _last_start} =
    tokens
    |> Enum.with_index()
    |> Enum.reduce({[], -1}, fn {t, idx}, {acc, last_start} ->
      # tolerate struct or map; atom or string keys
      p =
        Map.get(t, :phrase) ||
          Map.get(t, "phrase") ||
          ""

      span_raw =
        Map.get(t, :span) ||
          Map.get(t, "span") ||
          {0, 0}

      {st, en} = normalize_span(span_raw)
      slice = safe_slice(s, st, en)

      reasons = []
      reasons = if is_binary(p), do: reasons, else: [{:phrase_type, p} | reasons]
      reasons = if en >= st, do: reasons, else: [{:order, {st, en}} | reasons]
      reasons = if p == slice, do: reasons, else: [{:slice_mismatch, {p, slice}} | reasons]
      reasons = if st >= last_start, do: reasons, else: [{:start_order, {last_start, st}} | reasons]

      if reasons == [] do
        {acc, st}
      else
        fail = %{
          index: idx,
          token: t,
          span: {st, en},
          expected_slice: slice,
          reasons: Enum.reverse(reasons)
        }

        {[fail | acc], st}
      end
    end)

  case failures do
    [] -> {:ok, si}
    _ -> {:error, Enum.reverse(failures)}
  end
end

# Accept tuple or [start,end]; default safely
defp normalize_span({st, en}) when is_integer(st) and is_integer(en), do: {st, en}
defp normalize_span([st, en]) when is_integer(st) and is_integer(en), do: {st, en}
defp normalize_span(_), do: {0, 0}

defp safe_slice(s, st, en)
     when is_binary(s) and is_integer(st) and is_integer(en) and en >= st do
  String.slice(s, st, en - st) || ""
end

defp safe_slice(_s, _st, _en), do: ""

end

