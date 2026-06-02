defmodule Core.ResolveInputTest do
  use ExUnit.Case, async: true

  # ---- Fakes ----------------------------------------------------------

  defmodule LexiconFake do
    def run(si), do: si
    def run(si, _opts), do: si
  end

  defmodule GateSkip do
    def gate(si, _opts), do: {:skip, si}
  end

  defmodule GatePlan do
    def gate(si, _opts), do: {:plan, %{kind: :test_plan}, si}
  end

  defmodule ExecuteMark do
    def execute(si, _plan), do: %{si | source: :executed}
  end

  defmodule PhraseRepoNull do
    def exists?(_), do: false
  end

  defmodule PhraseRepoFake do
    def exists?(phrase) do
      p = phrase |> String.downcase() |> String.trim()
      p in ["kick the bucket", "kick the"]
    end
  end

  # ---- Helpers --------------------------------------------------------

  defp resolve(sentence, extra_opts \\ []) do
    base_opts = [
      lexicon_mod: LexiconFake,
      gate_mod: GateSkip,
      execute_mod: ExecuteMark,
      phrase_repo: PhraseRepoNull
    ]

    Core.resolve_input(sentence, Keyword.merge(base_opts, extra_opts))
  end

  defp token_sig(tokens) do
    Enum.map(tokens, fn t ->
      phrase = (Map.get(t, :phrase) || "") |> String.downcase()
      span = Map.get(t, :span)
      mw? = Map.get(t, :mw)
      {phrase, span, mw?}
    end)
  end

  defp sorted_by_start?(tokens) do
    starts = for t <- tokens, do: elem(Map.fetch!(t, :span), 0)
    starts == Enum.sort(starts)
  end

  # Given a token span and the (normalized) sentence, try both interpretations:
  #   1) Word-index {start, stop_exclusive}  -> build from words
  #   2) Char-based {start, len} or {start, stop_exclusive} -> slice string
  defp candidate_extractions({s, k}, sent)
       when is_integer(s) and is_integer(k) and is_binary(sent) do
    sent_norm = sent |> String.trim() |> String.replace(~r/\s+/u, " ")
    words = if sent_norm == "", do: [], else: String.split(sent_norm, " ")
    word_len = length(words)

    # Option A: word-index end-exclusive
    word_str =
      if s >= 0 and k > s and k <= word_len do
        words |> Enum.slice(s, k - s) |> Enum.join(" ") |> String.downcase() |> String.trim()
      else
        nil
      end

    # Option B: char-based
    sent_len = String.length(sent_norm)
    {start, stop} = if s + k <= sent_len, do: {s, s + k}, else: {s, k}

    char_str =
      if start >= 0 and stop > start and stop <= sent_len do
        sent_norm |> String.slice(start, stop - start) |> String.downcase() |> String.trim()
      else
        nil
      end

    Enum.filter([word_str, char_str], & &1)
  end

  defp token_char_interval(token, sent) do
    phrase =
      token
      |> Map.get(:phrase, "")
      |> String.downcase()
      |> String.trim()

    {s, k} = Map.fetch!(token, :span)
    sent_norm = sent |> String.trim() |> String.replace(~r/\s+/u, " ")

    cond do
      char_slice_matches?(sent_norm, s, k, phrase) ->
        {s, s + k}

      k > s and char_slice_matches?(sent_norm, s, k - s, phrase) ->
        {s, k}

      true ->
        word_index_interval(sent_norm, s, k)
    end
  end

  defp char_slice_matches?(sent, start, len, phrase)
       when is_integer(start) and is_integer(len) and start >= 0 and len > 0 do
    sent
    |> String.slice(start, len)
    |> to_string()
    |> String.downcase()
    |> String.trim()
    |> Kernel.==(phrase)
  end

  defp char_slice_matches?(_sent, _start, _len, _phrase), do: false

  defp word_index_interval(sent, start_word, stop_word) do
    words = if sent == "", do: [], else: String.split(sent, " ")

    starts =
      words
      |> Enum.reduce({[], 0}, fn word, {acc, pos} ->
        {[pos | acc], pos + String.length(word) + 1}
      end)
      |> elem(0)
      |> Enum.reverse()

    ends = Enum.zip_with(starts, words, fn start, word -> start + String.length(word) end)

    if start_word >= 0 and stop_word > start_word and stop_word <= length(words) do
      {Enum.at(starts, start_word), Enum.at(ends, stop_word - 1)}
    else
      flunk("Cannot derive char interval for span #{inspect({start_word, stop_word})}")
    end
  end

  # ---- Tests ----------------------------------------------------------

  test "returns a SemanticInput struct and echoes the sentence/source" do
    si = resolve("Hello world")
    assert match?(%Core.SemanticInput{}, si)
    assert si.sentence == "Hello world"
    assert is_list(si.tokens)
    assert is_atom(si.source) or is_nil(si.source)
  end

  test "tokens (if present) expose phrase + span shape and are sorted by start" do
    si = resolve("Hello brave new world")

    for t <- si.tokens do
      assert is_binary(Map.get(t, :phrase))

      assert match?(
               {s, k} when is_integer(s) and s >= 0 and is_integer(k) and k > 0,
               Map.get(t, :span)
             )
    end

    assert sorted_by_start?(si.tokens)
  end

  test "span substring roughly matches token.phrase (supports word-index spans)" do
    input = "Kick the bucket today"
    si = resolve(input, phrase_repo: PhraseRepoFake)

    for t <- si.tokens do
      {s, k} = t.span
      candidates = candidate_extractions({s, k}, si.sentence)

      token_phrase =
        t.phrase
        |> String.downcase()
        |> String.trim()

      assert Enum.any?(candidates, fn c ->
               c == token_phrase or String.contains?(c, token_phrase) or
                 String.contains?(token_phrase, c)
             end),
             """
             No match for token #{inspect(t)} in sentence #{inspect(si.sentence)}.
             Tried candidates: #{inspect(candidates)}
             """
    end
  end

  test "word-gram candidates are separated from confirmed MWE tokens" do
    si = resolve("Kick the bucket today", phrase_repo: PhraseRepoFake)

    candidate =
      Enum.find(si.phrase_candidates, fn t ->
        String.downcase(t.phrase) == "kick the bucket" and Map.get(t, :mw) == false and
          Map.get(t, :confirmed?) == false
      end)

    assert candidate, """
    Expected an unconfirmed phrase candidate 'Kick the bucket'.
    Got phrase candidates:

      #{inspect(si.phrase_candidates, pretty: true)}
    """

    refute Enum.any?(si.tokens, fn t ->
             String.downcase(t.phrase) == "kick the bucket" and Map.get(t, :mw) == true
           end)

    assert Enum.any?(si.tokens, fn t -> String.downcase(t.phrase) == "today" end)
  end

  test "idempotence: repeated calls produce the same token signature" do
    sentence = "Kick the bucket today"
    a_si = resolve(sentence, phrase_repo: PhraseRepoFake)
    b_si = resolve(sentence, phrase_repo: PhraseRepoFake)
    a = token_sig(a_si.tokens)
    b = token_sig(b_si.tokens)
    assert a == b
  end

  @tag :emoji
  test "does not crash on emoji, punctuation, or extra whitespace" do
    sentence = "  Hello,   world!  👋🧠  "
    si = resolve(sentence)
    assert match?(%Core.SemanticInput{}, si)
    assert is_list(si.tokens)
  end

  test "final tokens do not overlap" do
    si = resolve("Kick the bucket today", phrase_repo: PhraseRepoFake)

    intervals =
      si.tokens
      |> Enum.map(&token_char_interval(&1, si.sentence))
      |> Enum.sort()

    overlaps =
      intervals
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.filter(fn [{_a_start, a_stop}, {b_start, _b_stop}] -> a_stop > b_start end)

    assert intervals != []
    assert overlaps == []
  end
end
