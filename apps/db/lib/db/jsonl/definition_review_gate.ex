defmodule Db.JSONL.DefinitionReviewGate do
  @moduledoc """
  Extra, pure approval checks for a *single* reviewed definition-only plan.

  Verifies that the full-source review and the imminent apply see the exact
  same word, POS, number of senses, number of patches and fingerprint.

  This does not substitute for DefinitionOnlyBackfill.apply/2, which verifies
  the fingerprint again and uses guarded updates in one transaction.
  """

  @spec validate(map(), String.t(), String.t(), pos_integer(), pos_integer(), String.t()) ::
          :ok | {:error, term()}
  def validate(plan, word, pos, expected_senses, expected_patches, fingerprint)
      when is_map(plan) and is_binary(word) and is_binary(pos) and
           is_integer(expected_senses) and is_integer(expected_patches) and
           is_binary(fingerprint) do
    cond do
      plan.word != word or plan.pos != pos ->
        {:error, :unexpected_word_or_pos}

      expected_senses <= 0 or expected_patches <= 0 ->
        {:error, :expected_counts_must_be_positive}

      plan.row_count != expected_senses ->
        {:error, {:sense_count_changed, expected_senses, plan.row_count}}

      plan.patch_count != expected_patches ->
        {:error, {:patch_count_changed, expected_patches, plan.patch_count}}

      not Regex.match?(~r/\A[0-9a-f]{64}\z/, fingerprint) ->
        {:error, :invalid_fingerprint_format}

      plan.fingerprint != fingerprint ->
        {:error, :review_fingerprint_changed}

      true ->
        :ok
    end
  end

  def validate(_, _, _, _, _, _), do: {:error, :invalid_review_approval}
end
