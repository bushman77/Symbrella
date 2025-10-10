# lib/brain/lifg/gate.ex
defmodule Brain.LIFG.Gate do
  @moduledoc """
  Stage-1 → WM bridge: build WM candidates from LIFG choices.
  """

  @spec stage1_wm_candidates([map()], non_neg_integer(), float()) :: [map()]
  def stage1_wm_candidates(choices, now_ms, min) when is_list(choices) do
    choices
    |> Enum.reduce([], fn ch, acc ->
      norm =
        (Map.get(ch, :scores) && Map.get(ch.scores, ch.chosen_id)) ||
          get_in(ch, [:features, :score_norm]) ||
          get_in(ch, [:features, :score_raw]) ||
          0.0

      if norm >= min do
        [
          %{
            id: ch.chosen_id |> to_string(),
            lemma: Map.get(ch, :lemma),
            token_index: ch.token_index,
            score: norm,
            source: :lifg,
            reason: :lifg_stage1,
            ts: now_ms
          }
          | acc
        ]
      else
        acc
      end
    end)
    |> Enum.reverse()
  end

  def stage1_wm_candidates(_, _now_ms, _min), do: []
end
