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

  @spec pick(String.t(), features_like, decision_like) :: nil | %{id: atom, inline_text: String.t(), reason: String.t()}
  def pick(text, features, decision) do
    t = (text || "") |> String.downcase()

    # Example skill: simple banana how-to (demo)
    cond do
      banana?(t) and can_inline?(features, decision) ->
        %{
          id: :howto_banana,
          inline_text: banana_answer(),
          reason: "matched banana how-to trigger"
        }

      true ->
        nil
    end
  end

  defp can_inline?(f, d) do
    # Don’t inline during guardrail intercepts or hostile turns
    not f.guardrail? and not f.hostile? and d.action != :ask_first
  end

  defp banana?(t) do
    String.contains?(t, "banana") and
      (Regex.match?(~r/\bhow\s+(do\s+i|to)\b.*banana/, t) or
       Regex.match?(~r/\beat\b.*banana/, t) or
       Regex.match?(~r/\bbanana\b/, t))
  end

  defp banana_answer do
    """
    Here’s a quick way to eat a banana:

    1) Check ripeness — yellow with a few brown specks is sweetest.
    2) Rinse the peel (optional).
    3) Peel from the non-stem end (pinch and pull) or from the stem.
    4) Eat as-is, or slice into yogurt/cereal/smoothie.
    5) Dispose of the peel (don’t leave it on the ground).

    Tip: Underripe (greener) = firmer, less sweet. Overripe = great for smoothies or banana bread.
    """
  end
end

