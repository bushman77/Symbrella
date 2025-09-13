defmodule Core.SemanticInput do
  @moduledoc "Master input state for the semantic pipeline."

  alias Core.Token, as: Tok

  @typedoc "Semantic pipeline state"
  @type t :: %__MODULE__{
          sentence: binary(),
          original_sentence: binary(),
          source: atom(),
          tokens: [Tok.t()],
          pos_list: [atom() | nil],
          intent: atom() | nil,
          keyword: binary() | nil,
          confidence: float(),
          mood: atom()
        }

  defstruct sentence: "",
            original_sentence: "",
            source: :user,
            tokens: [],
            pos_list: [],
            intent: nil,
            keyword: nil,
            confidence: 0.0,
            mood: :neutral
end

