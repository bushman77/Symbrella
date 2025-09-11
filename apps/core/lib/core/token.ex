defmodule Core.Token do
  @moduledoc """
  Token struct + tokenizer in one place.

  • Pure function: `tokenize/2` → [%Core.Token{}]
  • No DB, no processes, no side effects.
  • Unicode-aware; tracks byte offsets for later alignment.
  """
  alias Core.{SemanticInput}

  @enforce_keys [:text]
  defstruct [
    :text,       # original slice
    :kind,       # :word | :number | :symbol | :punct
    :pos,        # to be filled by POS later
    features: %{}, # freeform map for later stages
    cell: nil      # hook for brain cell association (not used here)
  ]

  @type kind :: :word | :number | :symbol | :punct
  @type t :: %__MODULE__{
          text: binary(),
          kind: kind(),
          pos: atom() | nil,
          features: map(),
          cell: any()
        }

  def tokenize(sentence) do
    si = %SemanticInput{}
    %{si | sentence: sentence,
      tokens:
      sentence
      |> String.split(" ")
      |> Enum.map(fn word -> 
        %__MODULE__{text: word }  
      end)
    }
  end
end

