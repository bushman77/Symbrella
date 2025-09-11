defmodule Core.SemanticInput do
  @moduledoc """
  Master input state passed through the semantic pipeline.

  Build via `new/2`, optionally tokenizing (prefers Core.Tokenizer).
  Then enrich with helpers like `put_tokens/2`, `put_pos_list/2`,
  `with_intent/3`, `with_keyword/2`, `with_mood/2`, `merge_meta/2`.
  """

  alias Core.Token, as: Tok

  @typedoc "Semantic pipeline state"
  @type t :: %__MODULE__{
          sentence: binary(),
          tokens: [Tok.t()],            # canonical token list
          pos_list: [atom() | nil],     # POS (may include nil until tagged)
          intent: atom() | nil,
          keyword: binary() | nil,
          confidence: float(),
          mood: atom(),
        }

  defstruct sentence: "",
            tokens: [],
            pos_list: [],
            intent: nil,
            keyword: nil,
            confidence: 0.0,
            mood: :neutral

  # -- Public API -------------------------------------------------------------
  # -- Private ---------------------------------------------------------------

  defp normalize(raw) do
    raw
    |> String.replace(~r/\r\n?/, "\n")      # CRLF -> LF
    |> String.replace(~r/[ \t]+(\n)/, "\\1")# strip trailing spaces on lines
    |> String.replace(~r/\n{3,}/, "\n\n")   # collapse 3+ newlines
    |> String.trim()                        # trim both ends
  end
end

