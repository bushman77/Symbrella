defmodule Brain.LIFG.Post do
  @moduledoc """
  Post-processing for LIFG Stage1 results.

  - Optional reanalysis: flip rejected winners to the next-best alt.
  - Non-overlapping cover: choose a consistent set of winners across spans.

  Usage:
    {:ok, %{choices: choices}} = Brain.LIFG.Stage1.run(si, [])
    out = Brain.LIFG.Post.finalize(si, choices,
            reanalysis?: true,
            fail_fun: fn ch -> should_flip?(ch.token_index, ch.chosen_id) end,
            allow_overlaps?: false)

    out => %{si: map, choices: list, cover: list(%{span:, id:, ...}), flips: n}
  """

  alias Brain.LIFG.{Cover, Reanalysis}
  alias Brain.Utils.Safe

  @type choice :: %{
          token_index: non_neg_integer(),
          chosen_id: any(),
          alt_ids: list(),
          margin: number(),
          scores: map(),
          prob_margin: number()
        }

  @spec finalize(map(), list(choice), keyword()) ::
          %{si: map(), choices: list(choice), cover: list(map()), flips: non_neg_integer()}
  def finalize(si, choices, opts \\ []) when is_list(choices) and is_list(opts) do
    si0 = Safe.to_plain(si)

    reanalysis? = Keyword.get(opts, :reanalysis?, false)
    fail_fun = Keyword.get(opts, :fail_fun, fn _ -> false end)

    {choices2, flips} =
      if reanalysis? and is_function(fail_fun, 1) do
        out = Reanalysis.fallback(choices, fail_fun)
        {out.choices, out.flips}
      else
        {choices, 0}
      end

    %{cover: cover} =
      Cover.resolve_cover(si0, choices2,
        allow_overlaps?: Keyword.get(opts, :allow_overlaps?, false)
      )

    total_span =
      Enum.reduce(cover, 0, fn
        %{span: {i, j}}, acc -> acc + max(j - i, 0)
        _, acc -> acc
      end)

    :telemetry.execute(
      [:brain, :lifg, :cover, :final],
      %{count: length(cover), span: total_span},
      %{flips: flips, v: 1}
    )

    %{si: si0, choices: choices2, cover: cover, flips: flips}
  end
end
