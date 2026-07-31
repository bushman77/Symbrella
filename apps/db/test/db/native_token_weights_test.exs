defmodule Db.NativeTokenWeightsTest do
  use ExUnit.Case, async: true

  alias Db.NativeTokenWeights

  describe "weights_for_pair_rows/3" do
    test "builds none weights with PAD as zero" do
      left_rows = [
        [2, 10, 11, 0, 0],
        [2, 12, 13, 0, 0]
      ]

      right_rows = [
        [2, 10, 14, 0, 0],
        [2, 12, 15, 0, 0]
      ]

      {left_weights, right_weights, stats} =
        NativeTokenWeights.weights_for_pair_rows(left_rows, right_rows,
          mode: :none,
          pad_id: 0,
          unk_id: 1
        )

      assert left_weights == [
               [1.0, 1.0, 1.0, 0.0, 0.0],
               [1.0, 1.0, 1.0, 0.0, 0.0]
             ]

      assert right_weights == [
               [1.0, 1.0, 1.0, 0.0, 0.0],
               [1.0, 1.0, 1.0, 0.0, 0.0]
             ]

      assert stats.mode == :none
      assert stats.row_count == 4
      assert stats.token_count == 20
      assert stats.zero_weight_count == 8
      assert stats.nonzero_weight_count == 12
    end

    test "builds batch_idf weights and downweights common tokens" do
      left_rows = [
        [2, 10, 11, 0],
        [2, 10, 12, 0]
      ]

      right_rows = [
        [2, 10, 13, 0],
        [2, 10, 14, 0]
      ]

      {left_weights, right_weights, stats} =
        NativeTokenWeights.weights_for_pair_rows(left_rows, right_rows,
          mode: :batch_idf,
          pad_id: 0,
          unk_id: 1
        )

      # PAD is always zero.
      assert Enum.all?(left_weights ++ right_weights, fn row ->
               List.last(row) == 0.0
             end)

      # Token 10 appears in every row, so it should receive less weight
      # than row-specific tokens like 11, 12, 13, and 14.
      common_weight =
        left_weights
        |> hd()
        |> Enum.at(1)

      rare_weight =
        left_weights
        |> hd()
        |> Enum.at(2)

      assert rare_weight > common_weight

      assert stats.mode == :batch_idf
      assert stats.row_count == 4
      assert stats.token_count == 16
      assert stats.zero_weight_count == 4
      assert stats.nonzero_weight_count == 12
      assert stats.unique_token_id_count == 7
    end

    test "UNK gets configured low weight in batch_idf mode" do
      left_rows = [[2, 1, 10, 0]]
      right_rows = [[2, 1, 11, 0]]

      {left_weights, right_weights, _stats} =
        NativeTokenWeights.weights_for_pair_rows(left_rows, right_rows,
          mode: :batch_idf,
          pad_id: 0,
          unk_id: 1,
          unk_weight: 0.25
        )

      assert Enum.at(hd(left_weights), 1) == 0.25
      assert Enum.at(hd(right_weights), 1) == 0.25
    end

    test "raises on invalid mode" do
      assert_raise ArgumentError, fn ->
        NativeTokenWeights.weights_for_pair_rows([[1]], [[1]],
          mode: :garbage,
          pad_id: 0
        )
      end
    end
  end
end
