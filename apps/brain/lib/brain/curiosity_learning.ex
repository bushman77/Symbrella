defmodule Brain.CuriosityLearning do
  @moduledoc """
  Read-only, versioned projection of durable Curiosity investigation receipts.

  The operational audit is *not* factual evidence. A positive learning signal
  requires a matching resolved CognitiveIssue with a proof-authorized resolution.
  No ledger row, telemetry event, or SelfModel field may authorize a fact.
  Missing database access yields neutral biases, never guessed success.
  """

  import Ecto.Query, only: [from: 2]
  alias Db.{CognitiveIssue, CuriosityInvestigations}

  @version 1
  @window 100
  @keys [:missing_link, :direct, :two_hop, :acquisition]
  @conflict_reasons ["proof_not_authorized", "direct_proof_not_authorized",
                     "invalid_two_hop_candidate"]

  def neutral do
    %{v: @version, biases: Map.new(@keys, &{&1, 0.0}),
      sample_count: 0, success_count: 0, failure_count: 0}
  end

  @doc "Rebuild a bounded preference snapshot from committed audit and issue rows."
  def snapshot do
    attempts = CuriosityInvestigations.recent(@window)
    ids = attempts |> Enum.map(& &1.issue_id) |> Enum.filter(&is_integer/1) |> Enum.uniq()

    issues =
      case ids do
        [] -> %{}
        _ ->
          Db.all(from(issue in CognitiveIssue, where: issue.id in ^ids))
          |> Map.new(&{&1.id, &1})
      end

    project(attempts, issues)
  rescue
    _ -> neutral()
  catch
    :exit, _ -> neutral()
  end

  @doc "Pure, order-preserving receipt projection. Newest row wins per issue."
  def project(attempts, issues) when is_list(attempts) and is_map(issues) do
    attempts
    |> Enum.take(@window)
    |> Enum.filter(&(is_integer(get(&1, :issue_id)) and not is_nil(get(&1, :completed_at))))
    |> Enum.uniq_by(&get(&1, :issue_id))
    |> Enum.map(fn row -> classify(row, Map.get(issues, get(row, :issue_id))) end)
    |> Enum.reject(&(&1 == :ignored))
    |> Enum.reduce(neutral(), &accumulate/2)
    |> normalize()
  end

  def project(_, _), do: neutral()

  @doc "Classify a receipt, only crediting resolutions corroborated by durable issue state."
  def classify(row, issue) when is_map(row) do
    case get(row, :status) do
      "resolved" -> classify_resolved(row, issue)
      "failed" -> :failed
      "unresolved" ->
        if get(row, :reason) in @conflict_reasons,
          do: :unresolved_conflict, else: :unresolved
      _ -> :ignored
    end
  end

  def classify(_, _), do: :ignored

  defp classify_resolved(row, issue) when is_map(issue) do
    resolution = get(issue, :resolution, %{}) || %{}
    rule = get(resolution, :reasoning_rule)
    origin = get(row, :origin)
    acquired = get(row, :acquired_proof_id)
    acquired? = get(row, :acquisition) == "captured" and is_integer(acquired) and acquired > 0

    cond do
      get(issue, :status) != "resolved" -> :ignored

      origin in ["verified_direct_proof", "verified_direct_db_fallback"] and
        rule == "direct_population_comparison" and
        is_integer(get(resolution, :proof_id)) and get(resolution, :proof_id) > 0 ->
        if acquired? and acquired == get(resolution, :proof_id),
          do: :verified_acquired_direct, else: :verified_existing_direct

      origin == "hippocampal_two_hop_cue" and
        rule == "transitive_population_order" and
        valid_parents?(get(row, :parent_proof_ids), get(resolution, :parent_proof_ids)) ->
        if acquired? and acquired in get(resolution, :parent_proof_ids),
          do: :verified_acquired_two_hop, else: :verified_two_hop

      true -> :ignored
    end
  end

  defp classify_resolved(_, _), do: :ignored

  defp valid_parents?(a, b) when is_list(a) and is_list(b),
    do: length(a) == 2 and Enum.uniq(a) == a and Enum.all?(a, &(is_integer(&1) and &1 > 0)) and a == b

  defp valid_parents?(_, _), do: false

  defp accumulate(outcome, state) do
    {credit, keys, success?, failure?} =
      case outcome do
        :verified_existing_direct -> {0.025, [:missing_link, :direct], true, false}
        :verified_acquired_direct -> {0.060, [:missing_link, :direct, :acquisition], true, false}
        :verified_two_hop -> {0.040, [:missing_link, :two_hop], true, false}
        :verified_acquired_two_hop -> {0.060, [:missing_link, :two_hop, :acquisition], true, false}
        :unresolved_conflict -> {-0.012, [:missing_link], false, true}
        :failed -> {-0.008, [:missing_link], false, true}
        _ -> {0.0, [], false, false}
      end

    bias =
      Enum.reduce(keys, state.biases, fn key, acc ->
        Map.update!(acc, key, &bound(&1 + credit))
      end)

    %{state |
      biases: bias,
      sample_count: state.sample_count + 1,
      success_count: state.success_count + if(success?, do: 1, else: 0),
      failure_count: state.failure_count + if(failure?, do: 1, else: 0)}
  end

  @doc "Reject unknown versions and clamp all supplied values before use."
  def normalize(%{} = value) do
    if get(value, :v) == @version do
      raw = get(value, :biases, %{})
      biases =
        Map.new(@keys, fn k -> {k, bound(number(get(raw, k, 0.0)))} end)

      %{v: @version, biases: biases,
        sample_count: count(get(value, :sample_count)),
        success_count: count(get(value, :success_count)),
        failure_count: count(get(value, :failure_count))}
    else
      neutral()
    end
  end

  def normalize(_), do: neutral()

  def bias(snapshot, kind) when kind in @keys do
    snapshot |> normalize() |> Map.fetch!(:biases) |> Map.fetch!(kind)
  end
  def bias(_, _), do: 0.0

  defp count(n) when is_integer(n) and n >= 0, do: min(n, @window)
  defp count(_), do: 0
  defp number(n) when is_number(n), do: n * 1.0
  defp number(_), do: 0.0
  defp bound(n) when is_number(n), do: max(-0.12, min(n, 0.12))
  defp bound(_), do: 0.0
  defp get(m, key, default \\ nil)
  defp get(m, key, default) when is_map(m),
    do: Map.get(m, key, Map.get(m, Atom.to_string(key), default))
  defp get(_, _, default), do: default
end
