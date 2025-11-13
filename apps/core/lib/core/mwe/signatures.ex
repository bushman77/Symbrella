defmodule Core.MWE.Signatures do
  @moduledoc """
  Sliding-window MWE detector that emits symbolic intent bias.

  • Pure, struct-safe (uses Map.get/update, no Access).
  • Two stages:
      - :early — light lexical rules only (no DB dependence)
      - :late  — lexical OR POS-pattern matches (uses `si.active_cells` if present)
  • Returns SI with:
      - `:intent_bias` :: %{token_index(integer) => float}
      - `:mwe_matches`  :: [%{span: {i,j}, size: 2..5, phrase: binary, pos: [binary], kind: :lex | :pos | :both}]
      - `:trace` updated with {:mwe_signatures, %{stage: stage, accepted: n}}

  Options:
    * `stage: :early | :late` (default :late)
    * `extra_lex: [binary]` — additional lexical MWEs (normalized)
    * `multiplier: float`   — global bias scaler (default 1.0)
    * `demote_funcs?: boolean` — demote isolated function-word unigrams (default true)
  """

  @min_n 2
  @max_n 5

  # ——— Lexical idioms (normalized strings; multi-word kept intact) ———
  @lex_2 [
    "set up",
    "set down",
    "turn on",
    "turn off",
    "log in",
    "log out",
    "sign in",
    "sign out",
    "figure out",
    "point out",
    "find out",
    "end up",
    "give up",
    "pick up",
    "put off",
    "put on",
    "take off",
    "take over",
    "come up",
    "come across",
    "look up",
    "look into",
    "look for",
    "look after",
    "fall back",
    "back up",
    "check in",
    "check out",
    "speed up",
    "slow down",
    "shut down",
    "boot up",
    "power on",
    "power off"
  ]

  @lex_3 [
    "get rid of",
    "in front of",
    "as well as",
    "in case of",
    "in spite of",
    "on top of",
    "out of date",
    "in order to",
    "due to the",
    "according to",
    "by means of",
    "a lot of",
    "make sure to",
    "pay attention to",
    "take care of",
    "keep track of"
  ]

  @lex_4 [
    "on the other hand",
    "at the end of",
    "as soon as possible",
    "as a matter of",
    "for the first time",
    "in the middle of",
    "to the best of"
  ]

  @lex_5 [
    "as far as i know",
    "as long as it takes",
    "at the end of the"
  ]

  # ——— Function word buckets (normalized) ———
  @preps ~w(of to in on at by for from with about into over after between through during before under without within along across behind beyond up down off near among)
  @dets ~w(the a an this that these those some any each every no neither either)
  @conjs ~w(and or but nor so yet for)
  @auxes ~w(be am is are was were being been do does did have has had having)
  @modals ~w(can could may might must shall should will would)
  @pron ~w(i you he she it we they me him her us them my your his her our their mine yours hers ours theirs myself yourself himself herself itself ourselves yourselves themselves)
  @neg ~w(not never)
  @interj ~w(hello hi hey yo sup thanks thank)

  # POS precedence for fallbacks
  @pref [
    "verb",
    "noun",
    "adjective",
    "adverb",
    "preposition",
    "particle",
    "auxiliary",
    "modal",
    "pronoun",
    "determiner",
    "interjection",
    "conjunction",
    "numeral",
    "other"
  ]

  # ——— POS-patterns per length (all strings to mirror DB `pos`) ———
  @pos2 [
    ["verb", "particle"],
    ["verb", "preposition"],
    ["verb", "adverb"],
    ["auxiliary", "verb"],
    ["modal", "verb"],
    ["adjective", "noun"],
    ["noun", "noun"],
    ["noun", "preposition"],
    ["preposition", "noun"],
    ["determiner", "noun"],
    ["pronoun", "verb"],
    ["interjection", "noun"],
    ["interjection", "pronoun"],
    ["adverb", "adjective"]
  ]

  @pos3 [
    ["verb", "particle", "preposition"],
    ["preposition", "noun", "preposition"],
    ["adverb", "auxiliary", "pronoun"],
    ["auxiliary", "pronoun", "verb"],
    ["modal", "pronoun", "verb"],
    ["determiner", "noun", "preposition"],
    ["noun", "preposition", "noun"],
    ["adjective", "noun", "noun"],
    ["noun", "noun", "noun"],
    ["verb", "preposition", "noun"],
    ["verb", "noun", "preposition"],
    ["verb", "adverb", "preposition"],
    # Added: covers “shut the door”, “open the file”, etc.
    ["verb", "determiner", "noun"]
  ]

  @pos4 [
    ["preposition", "determiner", "other", "noun"],
    ["preposition", "determiner", "noun", "preposition"],
    ["conjunction", "adverb", "conjunction", "adjective"],
    ["preposition", "determiner", "noun", "preposition"],
    ["verb", "noun", "preposition", "noun"],
    ["auxiliary", "pronoun", "adverb", "verb"],
    ["modal", "pronoun", "adverb", "verb"]
  ]

  @pos5 [
    ["preposition", "other", "other", "other", "noun"],
    ["preposition", "determiner", "noun", "preposition", "determiner"],
    ["conjunction", "adverb", "conjunction", "determiner", "noun"]
  ]

  @doc """
  Run the detector and attach bias to SI.
  """
  def run(si, opts \\ []) when is_map(si) do
    stage = Keyword.get(opts, :stage, :late)
    extra_lex = (Keyword.get(opts, :extra_lex, []) || []) |> Enum.map(&norm/1)
    multiplier = Keyword.get(opts, :multiplier, 1.0)
    demote_funcs? = Keyword.get(opts, :demote_funcs?, true)

    tokens = Map.get(si, :tokens, [])
    words = tokens |> Enum.filter(&(Map.get(&1, :n, 1) == 1)) |> Enum.sort_by(&span_start/1)

    # Active cells may be a list or something else; normalize to list.
    ac =
      case Map.get(si, :active_cells) do
        l when is_list(l) -> l
        _ -> []
      end

    pos_cache = build_pos_cache(ac)

    # Collect matches longest-first, non-overlapping by word index
    matches =
      for n <- @max_n..@min_n//-1, reduce: {[], MapSet.new()} do
        {acc, covered} ->
          new =
            indices_for(length(words), n)
            |> Enum.reduce([], fn i, acc2 ->
              seq = Enum.slice(words, i, n)
              {phrase, pos_sig, kind} = classify_window(seq, tokens, stage, pos_cache, extra_lex)

              if kind == :none do
                acc2
              else
                [{i, i + n - 1, n, phrase, pos_sig, kind, seq} | acc2]
              end
            end)
            |> Enum.reverse()

          {accepted, covered2} = take_nonoverlapping(new, covered)
          {acc ++ accepted, covered2}
      end
      |> elem(0)

    # Build bias map
    bias =
      Enum.reduce(matches, %{}, fn {_i, _j, n, _phrase, _pos, kind, seq}, acc ->
        w = base_weight(n, kind) * multiplier
        idx = find_mwe_token_index(tokens, seq)

        acc1 = if is_integer(idx), do: Map.update(acc, idx, w, &(&1 + w)), else: acc

        if is_nil(idx) do
          Enum.reduce(seq, acc1, fn t, acc2 ->
            Map.update(acc2, Map.get(t, :index), w * 0.25, &(&1 + w * 0.25))
          end)
        else
          acc1
        end
      end)

    # Demote isolated function words (only when not inside an accepted idiom)
    bias2 =
      if demote_funcs? do
        demotions =
          words
          |> Enum.reject(&word_in_any_match?(&1, matches))
          |> Enum.filter(&function_word?/1)
          |> Enum.map(&{Map.get(&1, :index), -0.03 * multiplier})
          |> Map.new()

        Map.merge(bias, demotions, fn _k, v1, v2 -> v1 + v2 end)
      else
        bias
      end

    si
    |> Map.update(:intent_bias, bias2, fn old ->
      Map.merge(old, bias2, fn _k, v1, v2 -> v1 + v2 end)
    end)
    |> Map.update(:mwe_matches, format_matches(matches), fn old ->
      old ++ format_matches(matches)
    end)
    |> Map.update(:trace, [], fn tr ->
      [{:mwe_signatures, %{stage: stage, accepted: length(matches)}} | tr]
    end)
  rescue
    _ -> si
  end

  # ——— internals ———

  defp indices_for(words_len, n) when is_integer(words_len) and is_integer(n) and words_len >= n,
    do: 0..(words_len - n)

  defp indices_for(_words_len, _n), do: []

  defp format_matches(ms) do
    Enum.map(ms, fn {i, j, n, phrase, pos_sig, kind, _seq} ->
      %{span: {i, j}, size: n, phrase: phrase, pos: pos_sig, kind: kind}
    end)
  end

  defp take_nonoverlapping(windows, covered) do
    Enum.reduce(windows, {[], covered}, fn {i, j, _n, _phrase, _pos, _kind, _seq} = w,
                                           {acc, cov} ->
      rng = MapSet.new(i..j)

      if MapSet.disjoint?(cov, rng) do
        {acc ++ [w], MapSet.union(cov, rng)}
      else
        {acc, cov}
      end
    end)
  end

  defp classify_window(seq, _all_tokens, stage, pos_cache, extra_lex) do
    phrase = seq |> Enum.map(&Map.get(&1, :phrase, "")) |> Enum.join(" ") |> norm()

    # 1) Lexical hit via curated list / extra list
    lex_hit_curated =
      case length(seq) do
        2 -> phrase in @lex_2 or phrase in extra_lex
        3 -> phrase in @lex_3 or phrase in extra_lex
        4 -> phrase in @lex_4 or phrase in extra_lex
        5 -> phrase in @lex_5 or phrase in extra_lex
        _ -> false
      end

    # 2) DB-driven lexical hit (late stage only): if the *phrase* itself exists in active_cells
    #    we treat it as a lexical MWE regardless of internal POS pattern.
    lex_hit_db =
      if stage == :late do
        Map.has_key?(pos_cache, phrase)
      else
        false
      end

    lex_hit = lex_hit_curated or lex_hit_db

    pos_sig = Enum.map(seq, &best_pos(&1, pos_cache))

    pos_hit =
      if stage == :late do
        case length(seq) do
          2 -> pos_sig in @pos2
          3 -> pos_sig in @pos3
          4 -> pos_sig in @pos4
          5 -> pos_sig in @pos5
          _ -> false
        end
      else
        false
      end

    kind =
      cond do
        lex_hit and pos_hit -> :both
        lex_hit -> :lex
        pos_hit -> :pos
        true -> :none
      end

    {phrase, pos_sig, kind}
  end

  defp base_weight(n, kind) do
    k =
      case n do
        2 -> 0.12
        3 -> 0.16
        4 -> 0.20
        5 -> 0.22
        _ -> 0.10
      end

    bonus =
      case kind do
        :both -> 0.03
        :lex -> 0.02
        :pos -> 0.0
        _ -> 0.0
      end

    Kernel.min(k + bonus, 0.35)
  end

  defp best_pos(word_tok, pos_cache) do
    text = Map.get(word_tok, :phrase, "") |> norm()

    candidates =
      Map.get(pos_cache, text, MapSet.new())
      |> MapSet.to_list()
      |> normalize_pos_fallbacks(text)

    case candidates do
      [] -> "other"
      xs -> Enum.find(@pref, fn p -> p in xs end) || List.first(xs)
    end
  end

  defp normalize_pos_fallbacks(pos_list, text) do
    cond do
      pos_list != [] -> pos_list
      text in @preps -> ["preposition"]
      text in @dets -> ["determiner"]
      text in @conjs -> ["conjunction"]
      text in @auxes -> ["auxiliary"]
      text in @modals -> ["modal"]
      text in @pron -> ["pronoun"]
      text in @neg -> ["adverb"]
      text in @interj -> ["interjection"]
      String.contains?(text, " ") -> ["phrase"]
      true -> []
    end
  end

  defp build_pos_cache(active_cells) when is_list(active_cells) do
    Enum.reduce(active_cells, %{}, fn cell, acc ->
      norm = Map.get(cell, :norm) || Map.get(cell, "norm") || ""
      pos = Map.get(cell, :pos) || Map.get(cell, "pos") || ""
      n = norm |> norm()
      p = pos |> to_string() |> String.downcase() |> String.trim()

      if n == "" or p == "" do
        acc
      else
        Map.update(acc, n, MapSet.new([p]), &MapSet.put(&1, p))
      end
    end)
  end

  defp build_pos_cache(_), do: %{}

  defp find_mwe_token_index(all_tokens, seq) when is_list(seq) do
    phrase = seq |> Enum.map(&Map.get(&1, :phrase, "")) |> Enum.join(" ") |> norm()
    n = length(seq)
    start_char = seq |> hd() |> span_start()
    last = List.last(seq)
    end_char = span_start(last) + span_len(last)

    all_tokens
    |> Enum.filter(&(Map.get(&1, :n, 1) == n))
    |> Enum.find_value(fn t ->
      (norm(Map.get(t, :phrase, "")) == phrase and
         span_start(t) == start_char and
         span_start(t) + span_len(t) == end_char) && Map.get(t, :index)
    end)
  end

  defp find_mwe_token_index(_all_tokens, _seq), do: nil

  defp span_start(t), do: Map.get(t, :span, {0, 0}) |> elem(0)
  defp span_len(t), do: Map.get(t, :span, {0, 0}) |> elem(1)

  defp function_word?(t) do
    w = Map.get(t, :phrase, "") |> norm()

    w in @preps or w in @dets or w in @conjs or w in @auxes or w in @modals or w in @pron or
      w in @neg
  end

  defp word_in_any_match?(t, matches) do
    idx = Map.get(t, :index)

    Enum.any?(matches, fn {_i, _j, _n, _p, _pos, _k, seq} ->
      Enum.any?(seq, &(Map.get(&1, :index) == idx))
    end)
  end

  # cheap, unicode-tolerant normalizer
  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(v), do: v |> to_string() |> norm()
end
