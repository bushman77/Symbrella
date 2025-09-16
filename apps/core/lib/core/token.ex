defmodule Core.Token do
  defstruct [:phrase, :span, :mw, :source, :instances, :n]

  @type t :: %__MODULE__{
          phrase: String.t(),
          span: {non_neg_integer(), non_neg_integer()},
          mw: boolean(),
          source: :db | :assumed,
          instances: list(),
          n: pos_integer()
        }

  # public
  def tokenize(sentence, opts \\ []) when is_binary(sentence) do
    s = String.trim(sentence)
    words = Regex.scan(~r/\S+/, s) |> Enum.map(&hd/1)
    max_n = Keyword.get(opts, :max_n, 4)
    assume_all? = Keyword.get(opts, :assume_all, true)

    tokens = words_to_ngrams(words, s, max_n, assume_all?: assume_all?)
    {:ok, tokens}
  end

  # ——— internals ———

  defp words_to_ngrams(words, sentence, max_n, opts) do
    assume_all? = Keyword.get(opts, :assume_all, true)

    for i <- 0..(length(words) - 1),
        n <- 1..min(max_n, length(words) - i) do
      phrase =
        words
        |> Enum.slice(i, n)
        |> Enum.join(" ")

      {start, stop} = span_in(sentence, phrase)

      known? = phrase_exists?(phrase, opts)
      %__MODULE__{
        phrase: phrase,
        span: {start, stop},
        mw: n > 1,
        source: if(known?, do: :db, else: :assumed),
        instances: [],
        n: n
      }
    end
  end

  defp span_in(sentence, phrase) do
    # naive first match; good enough for pipeline plumbing
    case :binary.match(sentence, phrase) do
      {pos, len} -> {pos, pos + len}
      :nomatch -> {0, 0}
    end
  end

  defp phrase_exists?(phrase, _opts) do
    try do
      Core.PhraseRepo.Default.exists?(phrase)
    rescue
      _ -> false
    end
  end
end

