defmodule Core.Token do
  @moduledoc """
  Core.Token carries the token struct **and** the tokenizer.

  Emits **word-grams only** with **word-index** spans by default (end-exclusive: `{i, i+n}`).
  Per start index, order is **longest → shortest**.

  NEW:
    • Explicit punctuation splitting & tagging (Unicode-safe).
      - Punctuation is *not* included in word-grams.
      - In `span_mode: :chars`, punctuation tokens are emitted with char spans.
      - In `span_mode: :words`, punctuation tokens are omitted (word-grams only).
    • `span_mode: :words | :chars` (default `:words`). When `:chars`, n-gram spans use
      character indices over the normalized sentence (end-exclusive: `{start, end}`) and
      punctuation tokens are included as single-symbol tokens with `kind: :punct`.
    • `to_char_spans/1,2` — convert existing word spans to char spans (unchanged).
    • `check_span_invariants/1` — optional invariant validation (unchanged).

  `tokenize/2` returns a %Core.SemanticInput{sentence, tokens}.
  """

  @enforce_keys [:phrase, :span, :n]
  defstruct index: nil,
            phrase: "",
            span: {0, 0},
            mw: false,
            source: nil,
            instances: [],
            n: 1,
            # Optional hints; safe for downstream that uses maps
            # :word | :punct
            kind: :word,
            # "punct" for punctuation tokens (only when span_mode=:chars)
            pos: nil,
            # :sentence_final | :comma | :dash | ...
            subpos: nil

  @type t :: %__MODULE__{
          index: nil | non_neg_integer(),
          phrase: String.t(),
          span: {non_neg_integer(), non_neg_integer()},
          mw: boolean(),
          instances: list(),
          n: pos_integer(),
          source: any(),
          kind: :word | :punct,
          pos: nil | String.t(),
          subpos: nil | atom()
        }

  @type span_mode :: :words | :chars

  @spec tokenize(String.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(sentence, opts \\ []) when is_binary(sentence) do
    max_n = Keyword.get(opts, :max_wordgram_n, 3) |> max(1)
    span_mode = Keyword.get(opts, :span_mode, :words)

    s =
      sentence
      |> to_string()
      |> String.trim()
      |> String.replace(~r/\s+/u, " ")

    # 1) Scan the normalized string into primitive tokens (words & punct) with char (grapheme) spans.
    prim = scan_primitive_tokens(s)

    # 2) Derive the list of word tokens (order preserved).
    word_tokens =
      prim
      |> Enum.with_index()
      |> Enum.filter(fn {t, _i} -> t.kind == :word end)

    # 3) Build word-grams (only over word tokens).
    grams = build_wordgrams_from_word_tokens(word_tokens, max_n, span_mode)

    # 4) Optionally include punctuation tokens in :chars mode.
    tokens =
      case span_mode do
        :words ->
          grams

        :chars ->
          puncts =
            prim
            |> Enum.filter(&(&1.kind == :punct))
            |> Enum.map(fn p ->
              %__MODULE__{
                phrase: p.text,
                span: {p.start, p.stop},
                n: 1,
                mw: false,
                source: :punct,
                kind: :punct,
                pos: "punct",
                subpos: p.subpos
              }
            end)

          (grams ++ puncts)
          |> Enum.sort_by(fn %__MODULE__{span: {st, _}} -> st end)
      end

    tokens =
      tokens
      |> Enum.with_index()
      |> Enum.map(fn {t, idx} -> %__MODULE__{t | index: idx} end)

    %Core.SemanticInput{sentence: s, tokens: tokens}
  end

  # ---------- primitive scan (Unicode-safe) ----------

  @doc false
  # Classify a single grapheme as :space | :punct | :word
  defp char_class(g) do
    cond do
      g =~ ~r/\s/u -> :space
      # Unicode P (punctuation) or S (symbol) -> treat as punctuation
      g =~ ~r/[\p{P}\p{S}]/u -> :punct
      true -> :word
    end
  end

  @punct_sf MapSet.new(["?", "!", "."])
  @punct_map %{
    "," => :comma,
    ";" => :semicolon,
    ":" => :colon,
    "—" => :dash,
    "-" => :dash,
    "…" => :ellipsis,
    "\"" => :quote,
    "”" => :quote,
    "“" => :quote,
    "’" => :quote,
    "'" => :quote,
    "(" => :paren,
    ")" => :paren,
    "[" => :paren,
    "]" => :paren,
    "{" => :paren,
    "}" => :paren
  }

  @doc false
  defp punct_pos(sym) do
    if MapSet.member?(@punct_sf, sym),
      do: {:punct, :sentence_final},
      else: {:punct, Map.get(@punct_map, sym, :other)}
  end

  @doc false
  # Deterministic scanner with a separate running-word buffer.
  # Returns a list of maps: %{text,start,stop,kind,:subpos?}
  #
  # NOTE: start/stop are *grapheme indices* (compatible with String.slice/3).
  defp scan_primitive_tokens(s) do
    graphemes = String.graphemes(s)

    {idx, run, acc} =
      Enum.reduce(graphemes, {0, nil, []}, fn g, {i, run, acc} ->
        cls = char_class(g)
        w = 1

        case {cls, run} do
          {:space, nil} ->
            {i + w, nil, acc}

          {:space, {text, st, _en}} ->
            acc1 = [%{text: text, start: st, stop: i, kind: :word} | acc]
            {i + w, nil, acc1}

          {:punct, nil} ->
            {_p, sub} = punct_pos(g)
            acc1 = [%{text: g, start: i, stop: i + w, kind: :punct, subpos: sub} | acc]
            {i + w, nil, acc1}

          {:punct, {text, st, _en}} ->
            {_p, sub} = punct_pos(g)
            acc1 = [%{text: text, start: st, stop: i, kind: :word} | acc]
            acc2 = [%{text: g, start: i, stop: i + w, kind: :punct, subpos: sub} | acc1]
            {i + w, nil, acc2}

          {:word, nil} ->
            {i + w, {g, i, i + w}, acc}

          {:word, {text, st, _en}} ->
            {i + w, {text <> g, st, i + w}, acc}
        end
      end)

    acc =
      case run do
        nil -> acc
        {text, st, _en} -> [%{text: text, start: st, stop: idx, kind: :word} | acc]
      end

    acc |> Enum.reverse()
  end

  # ---------- n-gram builder over word tokens ----------

  # word_tokens: [{%{text,start,stop,kind: :word}, primitive_index}, ...]
  defp build_wordgrams_from_word_tokens(word_tokens, max_n, span_mode) do
    k = length(word_tokens)

    if k == 0 do
      []
    else
      0..(k - 1)
      |> Enum.flat_map(fn wi ->
        max_here = min(max_n, k - wi)

        for n <- max_here..1//-1 do
          {first, _} = Enum.at(word_tokens, wi)
          {last, _} = Enum.at(word_tokens, wi + n - 1)

          phrase =
            word_tokens
            |> Enum.slice(wi, n)
            |> Enum.map(fn {w, _} -> w.text end)
            |> Enum.join(" ")

          span =
            case span_mode do
              :words -> {wi, wi + n}
              :chars -> {first.start, last.stop}
            end

          %__MODULE__{
            phrase: phrase,
            span: span,
            mw: n > 1,
            instances: [],
            n: n,
            kind: :word
          }
        end
      end)
    end
  end

  # ---------- legacy helpers (unchanged) ----------

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
      cond do
        is_integer(i) and is_integer(j) and i >= 0 and j >= i and j <= length(words) and
            n == j - i ->
          start_char = Enum.at(starts, i, 0)
          last_idx = j - 1
          end_char = Enum.at(ends, last_idx, start_char)
          %__MODULE__{tok | span: {start_char, end_char}}

        true ->
          tok
      end
    end)
    |> Enum.sort_by(fn %__MODULE__{span: {start, _}} -> start end)
  end

  defp word_boundaries(words) do
    len = length(words)

    {_cur, starts_acc, ends_acc} =
      Enum.reduce(Enum.with_index(words), {0, [], []}, fn {w, idx}, {cur, sacc, eacc} ->
        s = cur
        e = s + String.length(w)
        next = if idx < len - 1, do: e + 1, else: e
        {next, [s | sacc], [e | eacc]}
      end)

    {Enum.reverse(starts_acc), Enum.reverse(ends_acc)}
  end

  @doc """
  Optional invariant check:
    • spans are non-decreasing by start
    • each token's phrase equals String.slice(sentence, start, end-start)
  """
  @spec check_span_invariants(Core.SemanticInput.t()) ::
          {:ok, Core.SemanticInput.t()} | {:error, [map()]}
  def check_span_invariants(%Core.SemanticInput{sentence: s, tokens: tokens} = si) do
    {failures, _last_start} =
      tokens
      |> Enum.with_index()
      |> Enum.reduce({[], -1}, fn {t, idx}, {acc, last_start} ->
        p = Map.get(t, :phrase) || Map.get(t, "phrase") || ""
        span_raw = Map.get(t, :span) || Map.get(t, "span") || {0, 0}
        {st, en} = normalize_span(span_raw)
        slice = safe_slice(s, st, en)

        reasons = []
        reasons = if is_binary(p), do: reasons, else: [{:phrase_type, p} | reasons]
        reasons = if en >= st, do: reasons, else: [{:order, {st, en}} | reasons]
        reasons = if p == slice, do: reasons, else: [{:slice_mismatch, {p, slice}} | reasons]

        reasons =
          if st >= last_start, do: reasons, else: [{:start_order, {last_start, st}} | reasons]

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

  defp normalize_span({st, en}) when is_integer(st) and is_integer(en), do: {st, en}
  defp normalize_span([st, en]) when is_integer(st) and is_integer(en), do: {st, en}
  defp normalize_span(_), do: {0, 0}

  defp safe_slice(s, st, en)
       when is_binary(s) and is_integer(st) and is_integer(en) and en >= st do
    String.slice(s, st, en - st) || ""
  end

  defp safe_slice(_s, _st, _en), do: ""
end
