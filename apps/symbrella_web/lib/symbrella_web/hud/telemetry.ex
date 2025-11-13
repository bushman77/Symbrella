defmodule SymbrellaWeb.HUD.Telemetry do
  @moduledoc """
  Global, zero-cost HUD telemetry aggregator.

  - Attaches once (idempotent) to Core/Brain telemetry events
  - Maintains small counters in ETS (fast, lock-free reads)
  - `snapshot/0` returns a compact map the HUD can render

  Usage (one-liner on LiveView mount or app start):
      :ok = SymbrellaWeb.HUD.Telemetry.ensure_attached()
  """

  alias Core.Telemetry

  @table :hud_telemetry_counters
  @attached_key {:hud, :telemetry, :attached}
  @handler_id :hud_telemetry_global

  # Events we care about
  @events [
    [:brain, :hippo, :attach],
    [:brain, :hippo, :recall],
    [:brain, :hippo, :write],
    [:brain, :hippo, :jaccard_filter],
    [:core, :recall, :synonyms, :lookup]
  ]

  @doc "Ensure ETS exists and telemetry is attached exactly once."
  @spec ensure_attached() :: :ok
  def ensure_attached do
    ensure_table()

    case :persistent_term.get(@attached_key, :no) do
      :yes ->
        :ok

      :no ->
        # Try to attach; tolerate already_exists if some other process beat us
        _ = Telemetry.attach_many(@handler_id, @events, &__MODULE__.handle/4, %{})
        :persistent_term.put(@attached_key, :yes)
        :ok
    end
  end

  @doc "Detach global handler (rarely needed; mostly for tests)."
  def detach do
    Telemetry.detach(@handler_id)
    :persistent_term.put(@attached_key, :no)
    :ok
  end

  @doc """
  Read a compact snapshot for the HUD.

  Returns:
    %{
      hippo: %{
        attach: %{count: integer()},
        recall: %{count: integer(), fallback_last?: boolean() | nil},
        write:  %{count: integer(), tokens_last: integer() | nil},
        jaccard: %{kept_sum: integer(), total_sum: integer(), min_last: float() | nil}
      },
      synonyms: %{
        lookups: %{count: integer(), cached_hits: integer(), took_ms_sum: integer()},
        avg_lookup_ms: float()
      }
    }
  """
  @spec snapshot() :: map()
  def snapshot do
    %{
      hippo: %{
        attach: %{
          count: get_int(:hippo_attach_count)
        },
        recall: %{
          count: get_int(:hippo_recall_count),
          fallback_last?: get_any(:hippo_recall_fallback_last)
        },
        write: %{
          count: get_int(:hippo_write_count),
          tokens_last: get_any(:hippo_write_tokens_last)
        },
        jaccard: %{
          kept_sum: get_int(:hippo_jaccard_kept_sum),
          total_sum: get_int(:hippo_jaccard_total_sum),
          min_last: get_any(:hippo_jaccard_min_last)
        }
      },
      synonyms: %{
        lookups: %{
          count: get_int(:syn_lookup_count),
          cached_hits: get_int(:syn_lookup_cached_hits),
          took_ms_sum: get_int(:syn_lookup_took_ms_sum)
        },
        avg_lookup_ms: avg(get_int(:syn_lookup_took_ms_sum), get_int(:syn_lookup_count))
      }
    }
  end

  # ─── Telemetry handler ────────────────────────────────────────────────────────

  @doc false
  def handle([:brain, :hippo, :attach], meas, _meta, _cfg) do
    inc(:hippo_attach_count, meas[:count] || 0)
    :ok
  end

  def handle([:brain, :hippo, :recall], meas, meta, _cfg) do
    inc(:hippo_recall_count, meas[:count] || 0)
    put(:hippo_recall_fallback_last, !!Map.get(meta, :fallback?, false))
    :ok
  end

  def handle([:brain, :hippo, :write], meas, meta, _cfg) do
    inc(:hippo_write_count, meas[:count] || 0)
    put(:hippo_write_tokens_last, Map.get(meta, :tokens))
    :ok
  end

  def handle([:brain, :hippo, :jaccard_filter], meas, meta, _cfg) do
    inc(:hippo_jaccard_kept_sum, meas[:kept] || 0)
    inc(:hippo_jaccard_total_sum, meas[:total] || 0)
    put(:hippo_jaccard_min_last, to_float(Map.get(meta, :min)))
    :ok
  end

  def handle([:core, :recall, :synonyms, :lookup], meas, meta, _cfg) do
    inc(:syn_lookup_count, meas[:count] || 0)
    inc(:syn_lookup_cached_hits, (Map.get(meta, :cached?) == true && 1) || 0)
    inc(:syn_lookup_took_ms_sum, Map.get(meta, :took_ms) || 0)
    :ok
  end

  def handle(_event, _meas, _meta, _cfg), do: :ok

  # ─── ETS helpers ──────────────────────────────────────────────────────────────

  defp ensure_table do
    case :ets.whereis(@table) do
      :undefined ->
        :ets.new(@table, [
          :named_table,
          :public,
          :set,
          read_concurrency: true,
          write_concurrency: true
        ])

      _tid ->
        :ok
    end
  end

  defp inc(key, by) when is_integer(by) do
    ensure_table()
    _ = :ets.update_counter(@table, key, {2, by}, {key, 0})
    :ok
  end

  defp put(key, val) do
    ensure_table()
    true = :ets.insert(@table, {key, val})
    :ok
  end

  defp get_int(key) do
    case :ets.lookup(@table, key) do
      [{^key, v}] when is_integer(v) -> v
      _ -> 0
    end
  end

  defp get_any(key) do
    case :ets.lookup(@table, key) do
      [{^key, v}] -> v
      _ -> nil
    end
  end

  defp to_float(x) when is_number(x), do: x * 1.0
  defp to_float(_), do: nil

  defp avg(_sum, 0), do: 0.0
  defp avg(sum, n) when is_integer(sum) and is_integer(n), do: sum / n
end
