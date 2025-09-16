defmodule Core do
  alias Core.{SemanticInput, Token}

  @spec resolve_input(String.t(), atom()) :: SemanticInput.t()
  def resolve_input(sentence, source \\ :console) when is_binary(sentence) do
    {:ok, tokens} = Token.tokenize(sentence, assume_all: true, max_n: 4)
    # Build struct with expected fields set
    %SemanticInput{
      original_sentence: sentence,
      sentence: sentence,
      source: source,
      tokens: tokens,
      pos_list: [],
      intent: nil,
      keyword: nil,
      confidence: 0.0,
      mood: nil,
      phrase_matches: [],
      activation_summary: %{},
      pattern_roles: %{},
      active_cells: [],
      brain_state_ref: nil,
      trace: []
    }
  end
end
