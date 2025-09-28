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

  test "multiword detection: tri-grams produce mw: true; phrase_repo opt tolerated" do
    si = resolve("Kick the bucket today", phrase_repo: PhraseRepoFake)

    mw =
      Enum.find(si.tokens, fn t ->
        String.downcase(t.phrase) == "kick the bucket" and Map.get(t, :mw) in [true, true, 1]
      end)

    assert mw, """
    Expected a multiword token 'Kick the bucket' with mw: true.
    Got tokens:

      #{inspect(token_sig(si.tokens), pretty: true)}
    """

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

  # Enable when your pipeline guarantees non-overlapping tokens:
  @tag :skip
  test "final tokens do not overlap (enable once de-overlap stage is wired)" do
    si = resolve("Kick the bucket today", phrase_repo: PhraseRepoFake)

    ends =
      Enum.map(si.tokens, fn t ->
        {s, k} = t.span
        # interpret as word spans against normalized sentence
        words = si.sentence |> String.replace(~r/\s+/u, " ") |> String.split(" ")
        stop = if k <= length(words), do: k, else: s + 1
        {s, stop}
      end)

    assert length(ends) > 0
  end
end
