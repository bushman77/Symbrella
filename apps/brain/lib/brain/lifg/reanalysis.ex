defmodule Brain.LIFG.Reanalysis do
  @moduledoc """
  Reanalysis fallback for LIFG choices.

  If downstream integration rejects a chosen sense, flip to the next-best alt id.
  Emits telemetry for each fallback.

  Usage:
    fail_fun = fn %{token_index: i, chosen_id: id} -> should_flip?(i, id) end
    %{choices: updated} = Brain.LIFG.Reanalysis.fallback(choices, fail_fun)
  """

  @type choice :: %{
          token_index: non_neg_integer(),
          chosen_id: any(),
          alt_ids: list(),
          margin: number(),
          scores: map(),
          prob_margin: number()
        }

  @spec fallback(list(choice), (map() -> boolean()), keyword()) ::
          %{choices: list(choice), flips: non_neg_integer()}
  def fallback(choices, fail_fun, _opts \\ [])
      when is_list(choices) and is_function(fail_fun, 1) do
    {out, flips} =
      Enum.map_reduce(choices, 0, fn ch, acc ->
        if fail_fun.(ch) do
          case ch.alt_ids do
            [next | rest] ->
old = ch.chosen_id
next_s = to_string(next)

alt2 =
  (rest ++ [old])
  |> Enum.reject(&is_nil/1)
  |> Enum.map(&to_string/1)

flipped =
  ch
  |> Map.put(:chosen_id, next_s)
  |> Map.put(:alt_ids, alt2)

              :telemetry.execute(
                [:brain, :lifg, :reanalysis, :fallback],
                %{count: 1},
                %{token_index: ch.token_index, from: ch.chosen_id, to: next, v: 1}
              )

              {flipped, acc + 1}

            [] ->
              :telemetry.execute(
                [:brain, :lifg, :reanalysis, :no_alternative],
                %{count: 1},
                %{token_index: ch.token_index, chosen_id: ch.chosen_id, v: 1}
              )

              {ch, acc}
          end
        else
          {ch, acc}
        end
      end)

    %{choices: out, flips: flips}
  end
end
