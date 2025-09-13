defmodule Core.Token do
  @moduledoc """
  Stage-0 tokenize: **normalize → all n-gram chunks (assumed) → SemanticInput**.

  Tokens are lightweight: just phrase, span, multiword flag, provenance, instances, and n.
  """

  @enforce_keys [:phrase, :span, :mw, :source]
  defstruct [
    :phrase,                 # string
    :span,                   # {start, stop}
    :mw,                     # boolean (multiword?)
    :source,                 # :assumed | :active | :db | :remote | :fallback
    instances: [],           # instance ids (when known)
    n: 1                     # word count
  ]

  @type t :: %__MODULE__{
          phrase: String.t(),
          span: {non_neg_integer(), non_neg_integer()},
          mw: boolean(),
          source: :assumed | :active | :db | :remote | :fallback,
          instances: [String.t()],
          n: pos_integer()
        }

  alias Core.SemanticInput, as: SI
  alias Core.Token, as: Tok

  @spec tokenize(String.t()) :: SI.t()
  def tokenize(sentence) when is_binary(sentence) do
    # 1) normalize
    norm = Core.Text.normalize(sentence)

    # 2) dynamic cap (min(5, word_count))
    cap = norm |> count_words() |> min(5)

    # 3) segment and wrap into %Core.Token{}
    tokens =
      norm
      |> Core.Segmenter.segment_phrases(max_len: cap)
      |> Enum.map(&to_token/1)

    # 4) build SemanticInput
    %SI{
      sentence: norm,
      original_sentence: sentence,
      source: :console,
      tokens: tokens
    }
  end

  # ——— private ———

  defp to_token(seg) do
    %Tok{
      phrase: seg.text,
      span: {seg.start, seg.stop},
      mw: Map.get(seg, :mw?, Map.get(seg, :mw, false)),
      source: seg.source,
      instances: Map.get(seg, :instances, []),
      n: count_words(seg.text)
    }
  end

  defp count_words(s) when is_binary(s),
    do: Regex.scan(~r/\S+/u, s) |> length()
end

