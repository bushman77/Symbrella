defmodule Core.Curiosity.SelectionPolicy do
  @moduledoc """
  Pure comparison of legacy issue ordering and SelfModel-aware priority.
  A proposal never bypasses the existing Investigator proof/reconciliation path.
  In shadow mode, this function reports the alternative without changing work.
  """
  alias Brain.CuriosityPolicy

  @kinds [:missing_link, :direct, :two_hop, :acquisition]

  @spec choose([map()], map(), keyword()) :: map()
  def choose(candidates, state, opts \\ [])

  def choose([], _state, _opts),
    do: %{status: :idle, selected: nil, shadow_selected: nil}

  def choose([legacy | _] = candidates, state, opts) do
    active? = Keyword.get(opts, :active?, false) == true
    ranked =
      candidates
      |> Enum.with_index()
      |> Enum.map(fn {candidate, index} ->
        kind = kind(candidate)
        {score, assessment} = CuriosityPolicy.score(priority(candidate), kind, state)
        %{candidate: candidate, index: index, score: score, assessment: assessment}
      end)
      |> Enum.sort_by(fn item -> {-item.score, item.index} end)

    best = hd(ranked)
    selected = if active?, do: best.candidate, else: legacy
    gate = CuriosityPolicy.assess(:missing_link, state)

    cond do
      active? and not gate.allowed? ->
        %{status: :deferred, reason: gate.reason, selected: nil,
          shadow_selected: best.candidate, legacy_selected: legacy,
          mode: :active, would_defer?: true,
          learned_bias: gate.learned_bias, score: best.score}

      true ->
        %{status: :ready, selected: selected,
          shadow_selected: best.candidate, legacy_selected: legacy,
          mode: if(active?, do: :active, else: :shadow),
          would_defer?: not gate.allowed?,
          learned_bias: gate.learned_bias, score: best.score}
    end
  end

  defp priority(candidate) do
    value = Map.get(candidate, :priority, Map.get(candidate, "priority", 0.5))
    if is_number(value), do: min(1.0, max(0.0, value * 1.0)), else: 0.5
  end

  defp kind(candidate) do
    kind = Map.get(candidate, :strategy, Map.get(candidate, "strategy", :missing_link))
    if kind in @kinds, do: kind, else: :missing_link
  end
end
