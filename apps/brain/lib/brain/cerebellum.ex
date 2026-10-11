defmodule Brain.Cerebellum do
  @moduledoc """
  Cerebellum-lite with durable state:
  - Predicts tiny per-candidate deltas to help LIFG Stage-1 in ambiguous cases.
  - Learns online using a pairwise hinge-like update.
  - Stores compact parameters per (scope, context_key) in ETS with periodic DB flush.

  Also runs a tiny GenServer registered as `Brain.Cerebellum` so dashboards can call `:status`.
  """

  use GenServer
  alias Brain.Cerebellum.Store

  # ------- GenServer (status only) -------------------------------------------

  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  def ensure_started do
    case Process.whereis(__MODULE__) do
      nil ->
        {:ok, _} = start_link([])
        :ok

      _pid ->
        :ok
    end
  end

  @impl true
  def init(_opts) do
    # Ensure store is up too, since status proxies stats.
    :ok = Store.ensure_started()
    {:ok, %{started_at: System.system_time(:millisecond)}}
  end

  @impl true
  def handle_call(:status, _from, state) do
    st = Store.stats()

    reply = %{
      region: :cerebellum,
      pid: self(),
      store_pid: Process.whereis(Store),
      store: st
    }

    {:reply, reply, state}
  end

  # ------- Public API (unchanged) --------------------------------------------

  @doc """
  Build a stable 32-byte binary key from any context term.
  Example:
      ctx = Brain.Cerebellum.context_key({:lifg_stage1, intent: :ask, pos: [:pron, :verb]})
  """
  def context_key(term) do
    :crypto.hash(:sha256, :erlang.term_to_binary({cfg(:feature_schema, 1), term}))
  end

  @doc """
  Compute small delta-scores per candidate from current weights of (scope, context_key).

  Options:
    - :scope (default "lifg_stage1")
    - :context_key (binary; default is global key)
    - :base_scores (for telemetry margin reporting)
  """
  def predict_lifg(_si, candidates, opts \\ []) when is_list(candidates) do
    w =
      Store.get_weights(opts[:scope] || "lifg_stage1", opts[:context_key] || context_key(:global))

    deltas =
      for c <- candidates, into: %{} do
        {candidate_id(c), dot(w, feat(c))}
      end

    case opts[:base_scores] do
      %{} = base ->
        m0 = rank_margin(base, %{})
        m1 = rank_margin(base, deltas)

        :telemetry.execute([:brain, :cerebellum, :lifg, :predict], %{margin0: m0, margin1: m1}, %{
          n: length(candidates),
          scope: opts[:scope] || "lifg_stage1"
        })

      _ ->
        :ok
    end

    deltas
  end

  @doc """
  Optionally add cerebellar deltas to base scores *only when* ambiguity/conflict is high.
  Returns the original base_scores or the corrected map.

  Options:
    - :scope (default "lifg_stage1")
    - :context_key (binary; default global)
    - :margin_tau (default cfg margin)
    - :acc_conflict (0.0..1.0)
    - :acc_conflict_tau (default cfg)
  """
  def calibrate_scores(si, base_scores, candidates, opts \\ []) do
    margin_tau = opts[:margin_tau] || cfg(:margin_tau, 0.12)
    acc_conflict = opts[:acc_conflict]
    acc_conflict_tau = opts[:acc_conflict_tau] || cfg(:acc_conflict_tau, 0.50)

    m0 = rank_margin(base_scores, %{})
    need_by_margin? = m0 < margin_tau
    need_by_conflict? = is_number(acc_conflict) and acc_conflict >= acc_conflict_tau

    if need_by_margin? or need_by_conflict? do
      deltas = predict_lifg(si, candidates, Map.put(opts, :base_scores, base_scores))
      Map.new(base_scores, fn {id, s} -> {id, s + Map.get(deltas, id, 0.0)} end)
    else
      base_scores
    end
  end

  @doc """
  Online learning after LIFG decides.

  Arguments:
    - chosen_id
    - candidates: [%{id/ :cell_id, lex_fit, rel_prior, activation, intent_bias}]
    - base_scores: %{id => score_before_deltas}
  Options:
    - :scope, :context_key
    - :lr (default cfg)
    - :margin_tau (default cfg)
  """

  def learn_lifg(_si, _chosen_id, candidates, base_scores, opts \\ [])
      when is_list(candidates) and is_map(base_scores) do
    case verified_target(candidates, opts[:supervision]) do
      nil ->
        # No independently verified label.
        # Prediction remains enabled, but learning is blocked.
        :ok

      target ->
        scope = opts[:scope] || "lifg_stage1"
        ctx = opts[:context_key] || context_key(:global)
        margin_tau = opts[:margin_tau] || cfg(:margin_tau, 0.12)
        lr = opts[:lr] || cfg(:lr, 0.05)

        target_id = candidate_id(target)

        deltas =
          predict_lifg(
            nil,
            candidates,
            scope: scope,
            context_key: ctx
          )

        scorep = fn candidate ->
          id = candidate_id(candidate)

          Map.get(base_scores, id, 0.0) +
            Map.get(deltas, id, 0.0)
        end

        target_features = feat(target)
        target_score = scorep.(target)

        {grad, loss_sum} =
          candidates
          |> Enum.reject(&(candidate_id(&1) == target_id))
          |> Enum.reduce({zero_vec(), 0.0}, fn candidate, {acc, loss_acc} ->
            competing_score = scorep.(candidate)

            loss = margin_tau - (target_score - competing_score)

            if loss > 0.0 do
              gradient =
                target_features
                |> vec_sub(feat(candidate))
                |> vec_scale(lr * loss)

              {vec_add(acc, gradient), loss_acc + loss}
            else
              {acc, loss_acc}
            end
          end)

        unless vec_zero?(grad) do
          Store.learn(
            scope,
            ctx,
            grad,
            loss_sum,
            feature_schema: cfg(:feature_schema, 1)
          )

          :telemetry.execute(
            [:brain, :cerebellum, :lifg, :learn],
            %{update_norm: l2_norm(grad), loss: loss_sum},
            %{scope: scope, supervision: :gold_label}
          )
        end

        :ok
    end
  end

  # ------- Feature plumbing ---------------------------------------------------

  defp verified_target(candidates, %{
         source: :gold_label,
         target_id: target_id
       })
       when is_binary(target_id) do
    Enum.find(candidates, fn candidate ->
      candidate_id(candidate) == target_id
    end)
  end

  defp verified_target(_candidates, _supervision), do: nil

  defp candidate_id(%{id: id}), do: id
  defp candidate_id(%{cell_id: id}), do: id
  defp candidate_id(_), do: nil

  defp feat(c) do
    [
      to_f(Map.get(c, :context_fit, 0.0)),
      to_f(Map.get(c, :lex_fit, 0.0)),
      to_f(Map.get(c, :rel_prior, 0.0)),
      to_f(Map.get(c, :activation, 0.0)),
      to_f(Map.get(c, :intent_bias, 0.0))
    ]
  end

  defp to_f(v) when is_number(v), do: v * 1.0
  defp to_f(_), do: 0.0

  # ------- Scoring & margins --------------------------------------------------

  defp rank_margin(base_scores, deltas) do
    totals =
      Enum.map(base_scores, fn {id, s} -> s + Map.get(deltas, id, 0.0) end)

    case Enum.sort(totals, :desc) do
      [a, b | _] -> a - b
      [_a] -> 9.99
      _ -> 0.0
    end
  end

  # ------- Config & math ------------------------------------------------------

  defp cfg(k, default), do: Application.get_env(:brain, :cerebellum, %{}) |> Map.get(k, default)

  defp zero_vec, do: Enum.map(cfg(:init_weights, [0.0, 0.05, 0.05, 0.03, 0.03]), fn _ -> 0.0 end)
  defp dot(w, x), do: Enum.zip(w, x) |> Enum.reduce(0.0, fn {a, b}, acc -> acc + a * b end)
  defp vec_add(a, b), do: Enum.zip(a, b) |> Enum.map(fn {x, y} -> x + y end)
  defp vec_sub(a, b), do: Enum.zip(a, b) |> Enum.map(fn {x, y} -> x - y end)
  defp vec_scale(v, s), do: Enum.map(v, &(&1 * s))
  defp l2_norm(v), do: :math.sqrt(Enum.reduce(v, 0.0, fn x, acc -> acc + x * x end))
  defp vec_zero?(v), do: Enum.all?(v, &(&1 == 0.0))
end
