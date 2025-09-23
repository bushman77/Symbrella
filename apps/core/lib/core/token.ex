defmodule Core.Token do
  defstruct [:phrase, :span, :mw, :source, :instances, :n]

  @type t :: %__MODULE__{
          phrase: String.t(),
          # word-index span (exclusive end): {i, i+n}
          span: {non_neg_integer(), non_neg_integer()},
          mw: boolean(),
          instances: list(),
          n: pos_integer()
        }

  @doc """
  Tokenize a sentence into ALL contiguous n-grams (ignorant mode), emitting
  longest→shortest per start index. Spans are *word-index* ({i, i+n}).

  Returns a %Core.SemanticInput{} with tokens filled.
  """
  def tokenize(sentence) when is_binary(sentence) do
    s = String.trim(sentence)
    words = String.split(s)
    %Core.SemanticInput{
      sentence: s,
      tokens: words_to_ngrams(words)
    }
  end

  # ---------- private ----------
  defp words_to_ngrams(words) do
    k = length(words)
    Enum.reduce(0..(k - 1), [], fn i, acc ->
      acc ++
       for n <- min(5, k - i)..1//-1 do
         %__MODULE__{
           phrase: words |> Enum.slice(i, n) |> Enum.join(" "),
           span: {i, i + n},      # word-index span
           mw: n > 1,
           instances: [],
           n: n
        }
      end
    end)
  end
end

