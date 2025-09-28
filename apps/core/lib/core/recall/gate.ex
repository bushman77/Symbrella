defmodule Core.Recall.Gate do
  @moduledoc """
  Decides whether we should consult LTM/DB this turn.

  Pure, DB-free, side-effect-free except appending a `:recall_gate` trace event.

  Returns:
    {:skip, si_with_trace}
    {:plan, %Core.Recall.Plan{} = plan, si_with_trace}

  Inputs (opts):
    * :confidence           – float 0..1 (defaults from SI trace if present)
    * :requires_knowledge?  – boolean (intent needs external knowledge)
    * :oov_terms            – [binary] (caller-computed if tokens don't carry is_oov)
    * :unmet_slots          – [atom] (planner slots not filled)
    * :strategies           – override retrieval order (default [:exact,:synonym,:embedding])
    * :budget_ms            – override (default from config)
    * :max_items            – override (default from config)
  """

  alias Core.SemanticInput, as: SI
  alias Core.Recall.Plan

  @spec gate(SI.t(), keyword()) :: {:skip, SI.t()} | {:plan, Plan.t(), SI.t()}
  def gate(%SI{} = si, opts \\ []) do
    now = now_ms()

    conf =
      case Keyword.get(opts, :confidence) do
        nil -> primary_conf_from_trace(si)
        v when is_number(v) -> v
      end

    requires_knowledge? = !!Keyword.get(opts, :requires_knowledge?, false)
    oov_terms = Keyword.get(opts, :oov_terms, [])
    unmet_slots = Keyword.get(opts, :unmet_slots, [])

    conf_threshold = conf_threshold()
    budget_ms = Keyword.get(opts, :budget_ms, recall_budget_ms())
    max_items = Keyword.get(opts, :max_items, recall_max_items())
    strategies = Keyword.get(opts, :strategies, [:exact, :synonym, :embedding])

    reasons =
      []
      |> maybe_add(:low_conf, conf && conf < conf_threshold)
      |> maybe_add(:oov_terms_present, not Enum.empty?(oov_terms))
      |> maybe_add(:unmet_slots, not Enum.empty?(unmet_slots))
      |> maybe_add(:intent_requires_knowledge, requires_knowledge?)

    decision =
      case reasons do
        [] -> :skip
        _ -> :plan
      end

    trace_ev = %{
      stage: :recall_gate,
      ts_ms: now,
      meta: %{
        decision: decision,
        reasons: reasons,
        conf: conf,
        oov_terms: oov_terms,
        unmet_slots: unmet_slots
      }
    }

    si2 = %SI{si | trace: si.trace ++ [trace_ev]}

    case decision do
      :skip ->
        {:skip, si2}

      :plan ->
        plan = %Plan{
          triggers: reasons,
          budget_ms: budget_ms,
          max_items: max_items,
          strategies: normalize_strategies(strategies, oov_terms)
        }

        {:plan, plan, si2}
    end
  end

  # ---------- helpers ----------

  defp primary_conf_from_trace(%SI{trace: trace}) do
    trace
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{stage: :intent_candidates, meta: %{primary_conf: c}} when is_number(c) -> c
      _ -> nil
    end)
  end

  defp maybe_add(list, _reason, false), do: list
  defp maybe_add(list, reason, true), do: [reason | list]

  defp normalize_strategies(strats, oov_terms) do
    clean =
      strats
      |> Enum.uniq()
      |> Enum.filter(&(&1 in [:exact, :synonym, :embedding]))

    if Enum.empty?(clean) do
      default = [:exact, :synonym, :embedding]
      if oov_terms == [], do: default, else: [:exact, :embedding, :synonym]
    else
      clean
    end
  end

  # ---- knobs (now “wide open” by default; still overridable via config) ----

  defp conf_threshold,
    do: Application.get_env(:core, :recall_conf_threshold, 0.55)

  # was 40 -> make recall practically unconstrained time-wise for simple checks
  defp recall_budget_ms,
    do: Application.get_env(:core, :recall_budget_ms, 1_000)

  # was 8 -> allow a lot more items per turn
  defp recall_max_items,
    do: Application.get_env(:core, :recall_max_items, 10_000)

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end
