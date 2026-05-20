defmodule Core.Response.Skills do
  @moduledoc """
  Micro-skills registry + arbitration.
  Returns at most one inline helper text for the current turn, or nil.

  Keep this conservative to avoid spam. Throttling is a future enhancement.
  """

  @type decision_like :: %{
          required(:mode) => atom,
          required(:tone) => atom,
          required(:action) => atom
        }

  @type features_like :: %{
          required(:intent) => atom,
          required(:benign?) => boolean,
          required(:hostile?) => boolean,
          required(:guardrail?) => boolean,
          required(:approve_token?) => boolean
        }

  @spec pick(String.t(), features_like, decision_like) ::
          nil | %{id: atom, inline_text: String.t(), reason: String.t()}
  def pick(_text, _features, _decision), do: nil
end
