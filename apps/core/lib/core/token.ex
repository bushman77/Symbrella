defmodule Core.Token do
  @moduledoc """
  Token struct + tokenizer in one place.

  • Pure function: `tokenize/2` → [%Core.Token{}]
  • No DB, no processes, no side effects.
  • Unicode-aware; tracks byte offsets for later alignment.
  """

  @enforce_keys [:text, :start, :stop]
  defstruct [
    :text,       # original slice
    :phrase,     # normalized form (configurable)
    :kind,       # :word | :number | :symbol | :punct
    :pos,        # to be filled by POS later
    :start,      # byte start (inclusive)
    :stop,       # byte stop  (exclusive)
    :lower,      # lowercase cache
    :keyword,    # optional keyword hook
    features: %{}, # freeform map for later stages
    cell: nil      # hook for brain cell association (not used here)
  ]

  @type kind :: :word | :number | :symbol | :punct
  @type t :: %__MODULE__{
          text: binary(),
          phrase: binary(),
          kind: kind(),
          pos: atom() | nil,
          start: non_neg_integer(),
          stop: non_neg_integer(),
          lower: binary(),
          keyword: binary() | nil,
          features: map(),
          cell: any()
        }

  @doc """
  Split `sentence` into `%Core.Token{}` structs.

  Options:
    * `:keep_punct` (bool, default: true) — include punctuation/symbol tokens.
    * `:normalize`  (:none | :lower | :phrase, default: :none)
       - :none   → phrase == original slice
       - :lower  → phrase = lowercase slice
       - :phrase → phrase = normalized for lexicon keys (lowercase, trim, strip non-word chars except ' and -)

  Returns: a list of `%Core.Token{}` in reading order, each with byte offsets.
  """
  @spec tokenize(binary(), keyword()) :: [t()]
  def tokenize(sentence, opts \\ []) when is_binary(sentence) do
    keep_punct? = Keyword.get(opts, :keep_punct, true)
    norm_mode   = Keyword.get(opts, :normalize, :none)

    text =
      sentence
      |> String.trim_trailing()
      |> String.replace(~r/(?:\r\n|\r|\n){3,}/, "\n\n")

    # Words (letters with optional internal ’ or -), numbers (with . or , and optional %),
    # then single symbols/punct (kept as single-char tokens).
    pattern = ~r/\p{L}+(?:['’\-]\p{L}+)*|\p{N}+(?:[.,]\p{N}+)*%?|[^\s\p{L}\p{N}]/u

    Regex.scan(pattern, text, return: :index)
    |> Enum.map(fn [{start, len}] ->
      slice = :binary.part(text, start, len)
      kind  = classify(slice)
      lower = String.downcase(slice)
      phrase =
        case norm_mode do
          :none   -> slice
          :lower  -> lower
          :phrase -> normalize_phrase(slice)
        end

      %__MODULE__{
        text: slice,
        phrase: phrase,
        kind: kind,
        start: start,
        stop: start + len,
        lower: lower,
        pos: nil,
        keyword: nil,
        features: %{}
      }
    end)
    |> maybe_drop_punct(keep_punct?)
  end

  # ——— Private helpers ———

  defp maybe_drop_punct(tokens, true),  do: tokens
  defp maybe_drop_punct(tokens, false), do:
    Enum.reject(tokens, fn
      %__MODULE__{kind: k} when k in [:punct, :symbol] -> true
      _ -> false
    end)

  @compile {:inline, classify: 1}
  defp classify(s) do
    cond do
      String.match?(s, ~r/\p{L}/u) -> :word
      String.match?(s, ~r/\p{N}/u) -> :number
      String.match?(s, ~r/[\p{S}]/u) -> :symbol  # includes emoji and currency, etc.
      true -> :punct
    end
  end

  @compile {:inline, normalize_phrase: 1}
  defp normalize_phrase(s) do
    s
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}\s'\-]/u, "")
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end
end

