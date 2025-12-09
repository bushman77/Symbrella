defmodule Brain.LIFG.Explanation do
  @moduledoc """
  Deterministic, compact explanation for LIFG decisions.
  Produces a human one-liner + structured map for the UI and tests.
  """

  @type last_like :: %{
          optional(:meta) => map(),
          optional(:intent) => atom(),
          optional(:confidence) => number(),
          optional(:feature_mix) => map(),
          optional(:guards) => map(),
          optional(:choices) => list(),
          optional(:finalists) => list()
        }

  @spec build(last_like) :: map()
  def build(last) when is_map(last) do
    mix = Map.get(last, :feature_mix, %{})
    guard = Map.get(last, :guards, %{})
    meta = Map.get(last, :meta, %{})

    # winner / margin / runner-up
    {tok_idx, winner, margin} = winner_tuple(last)
    threshold = meta[:margin_threshold] || 0.15
    weak? = is_number(margin) and margin < threshold

    runner =
      last
      |> Map.get(:finalists, [])
      |> Enum.find_value(fn
        %{ranking: [{id1, _}, {id2, _} | _]} -> if id1 == winner, do: id2, else: id1
        _ -> nil
      end)

    # one-liner (<= ~160 chars)
    text =
      "t#{tok_idx} → #{winner} " <>
        "(margin #{fmt(margin)}#{if is_number(threshold), do: " < #{fmt(threshold)}", else: ""}" <>
        "#{if weak?, do: ", weak", else: ""}). " <>
        "mix: #{mix_text(mix)}. guards: #{guards_text(guard)}."

    %{
      text: text,
      decision: %{
        token_index: tok_idx,
        winner: winner,
        runner_up: runner,
        margin: margin,
        threshold: threshold,
        weak?: weak?
      },
      features: %{
        lex_fit: norm(mix[:lex_fit]),
        rel_prior: norm(mix[:rel_prior]),
        activation: norm(mix[:activation]),
        intent_bias: norm(mix[:intent_bias])
      },
      guards: guard
    }
  end

  # ---------- helpers ----------

  defp winner_tuple(%{choices: [%{token_index: i, chosen_id: id, margin: m} | _]}), do: {i, id, m}
  defp winner_tuple(_), do: {0, "∅", nil}

  defp mix_text(m) do
    "lex " <>
      fmt(m[:lex_fit]) <>
      " · prior " <>
      fmt(m[:rel_prior]) <>
      " · act " <>
      fmt(m[:activation]) <>
      " · bias " <> fmt(m[:intent_bias])
  end

defp guards_text(%{chargram_violation: v, rejected_by_boundary: r} = g) do
    parts = []
    parts = if is_number(v) and v > 0, do: parts ++ ["chargram=" <> to_string(v)], else: parts

    parts =
      if is_list(r) and r != [],
        do: parts ++ ["boundary=" <> Integer.to_string(length(r))],
        else: parts
parts =
      case Map.get(g, :missing_candidates) do
        n when is_integer(n) and n > 0 -> parts ++ ["missing_candidates=" <> Integer.to_string(n)]
        _ -> parts
      end

    parts =
      case Map.get(g, :missing_candidate_tokens) do
        [_ | _] = idxs ->
          joined =
            idxs
            |> Enum.map(&Integer.to_string/1)
            |> Enum.join(",")

          parts ++ ["missing_idx=[" <> joined <> "]"]

        _ ->
          parts
      end
    if parts == [], do: "none", else: Enum.join(parts, ", ")
  end

  defp guards_text(_), do: "none"

  defp fmt(nil), do: "—"
  defp fmt(v) when is_number(v), do: :erlang.float_to_binary(v, decimals: 2)
  defp fmt(v), do: to_string(v)

  defp norm(v) when is_number(v), do: Float.round(v, 4)
  defp norm(_), do: nil
end
