defmodule Core.Token do
  @moduledoc """
  Core.Token carries the token struct **and** the tokenizer.
  Emits **word-grams only** with word-index spans (end-exclusive: {i, i+n}).
  Per start index, order is **longest → shortest**.

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
          # word-index span (end-exclusive): {i, i+n}
          span: {non_neg_integer(), non_neg_integer()},
          mw: boolean(),
          instances: list(),
          n: pos_integer(),
          source: any()
        }
  @doc """
  Tokenize a sentence into **all contiguous word n-grams**.
                                                                             Options:
    * `:max_wordgram_n` (pos int, default 3) – cap n-gram length

  Returns %Core.SemanticInput{} with normalized `sentence` and ordered `tokens`.
  """
  @spec tokenize(String.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(sentence, opts \\ []) when is_binary(sentence) do
    max_n = Keyword.get(opts, :max_wordgram_n, 3) |> max(1)

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

    %Core.SemanticInput{sentence: s, tokens: tokens}
  end

  # ---------- helpers ----------

  defp build_wordgrams(words, max_n) do
    k = length(words)

    0..(k - 1)
    |> Enum.flat_map(fn i ->
      max_here = min(max_n, k - i)

      # longest → shortest per start index
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
end
