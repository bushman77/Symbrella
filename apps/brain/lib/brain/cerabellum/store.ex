defmodule Brain.Cerebellum.Store do
  @moduledoc """
  Durable, lightweight parameter store for the Cerebellum forward model.

  • Hot path in ETS (public named table).
  • Periodic flush to Postgres via **Db** (your Repo module).
  • Lazy-start on first call; you can also eager-start in your boot path.
  • API: get_weights/2, learn/5, flush/0, stats/0.

  Keying:
    - `scope`: "lifg_stage1" (default) or other domains later.
    - `context_key`: stable 32-byte binary (see Brain.Cerebellum.context_key/1).
  """

  use GenServer
  require Logger

  alias Db
  alias Db.CerebellumModel

  @name __MODULE__
  @table :brain_cerebellum_models

  # ---- Public API ------------------------------------------------------------

  def ensure_started do
    case Process.whereis(@name) do
      nil ->
        {:ok, _pid} = GenServer.start_link(@name, %{}, name: @name)
        :ok
      _pid -> :ok
    end
  end

  def get_weights(scope, context_key) when is_binary(context_key) do
    ensure_started()
    case :ets.lookup(@table, {scope, context_key}) do
      [{{^scope, ^context_key}, rec}] ->
        rec.weights
      _ ->
        rec = load_or_init(scope, context_key)
        rec.weights
    end
  end

  @doc """
  Apply an accumulated gradient/update to weights for (scope, context_key).
  `grad` is a float list matching feature_dims.
  `loss` is an optional scalar for EMA tracking.
  Options:
    - :clip_norm (float)       -> override config(:max_update_norm)
    - :feature_schema (int)    -> version stamp for features
  """
  def learn(scope, context_key, grad, loss \\ 0.0, opts \\ []) when is_list(grad) do
    ensure_started()
    GenServer.call(@name, {:learn, scope, context_key, grad, loss, opts})
  end

  def flush do
    ensure_started()
    GenServer.call(@name, :flush)
  end

  @doc """
  Introspection stats for dashboards.
  Returns: %{size, pending, feature_dims, flush_interval_ms, flush_batch_size, max_update_norm, feature_schema}
  """
  def stats do
    ensure_started()
    GenServer.call(@name, :stats)
  end

  # ---- GenServer -------------------------------------------------------------

  @impl true
  def init(_) do
    create_table!()

    st = %{
      pending: %{},                    # {{scope, ctx} => rec}
      feature_dims: cfg(:feature_dims, 5),
      init_weights: cfg(:init_weights, [0.0, 0.05, 0.05, 0.03, 0.03]),
      flush_interval_ms: cfg(:flush_interval_ms, 15_000),
      flush_batch_size: cfg(:flush_batch_size, 100),
      max_update_norm: cfg(:max_update_norm, 1.0),
      feature_schema: cfg(:feature_schema, 1)
    }

    Process.send_after(self(), :flush, st.flush_interval_ms)
    {:ok, st}
  end

  @impl true
  def handle_call({:learn, scope, ctx, grad, loss, opts}, _from, st) do
    rec = ensure_record(scope, ctx, st)

    gradv = pad_or_trim(grad, st.feature_dims)
    new_w = clip_norm(vec_add(rec.weights, gradv), st.max_update_norm)

    new_rec = %{
      rec
      | weights: new_w,
        count_seen: rec.count_seen + 1,
        ema_error: ema(rec.ema_error, loss, 0.10),
        feature_schema: opts[:feature_schema] || rec.feature_schema,
        updated_at: now()
    }

    :ets.insert(@table, {{scope, ctx}, new_rec})
    pending = Map.put(st.pending, {scope, ctx}, new_rec)
    st2 = maybe_flush(%{st | pending: pending})
    {:reply, :ok, st2}
  end

  @impl true
  def handle_call(:flush, _from, st) do
    {:reply, :ok, do_flush(st)}
  end

  @impl true
  def handle_call(:stats, _from, st) do
    size =
      case :ets.info(@table, :size) do
        :undefined -> 0
        n when is_integer(n) -> n
        _ -> 0
      end

    stat = %{
      size: size,
      pending: map_size(st.pending),
      feature_dims: st.feature_dims,
      flush_interval_ms: st.flush_interval_ms,
      flush_batch_size: st.flush_batch_size,
      max_update_norm: st.max_update_norm,
      feature_schema: st.feature_schema
    }

    {:reply, stat, st}
  end

  @impl true
  def handle_info(:flush, st) do
    Process.send_after(self(), :flush, st.flush_interval_ms)
    {:noreply, do_flush(st)}
  end

  # ---- Internal --------------------------------------------------------------

  defp create_table! do
    _ =
      :ets.new(@table, [
        :set, :public, :named_table,
        read_concurrency: true,
        write_concurrency: true
      ])
  rescue
    ArgumentError -> :ok
  end

  defp ensure_record(scope, ctx, st) do
    case :ets.lookup(@table, {scope, ctx}) do
      [{{^scope, ^ctx}, rec}] ->
        rec =
          if length(rec.weights) != st.feature_dims do
            %{rec | weights: pad_or_trim(rec.weights, st.feature_dims)}
          else
            rec
          end

        :ets.insert(@table, {{scope, ctx}, rec})
        rec

      _ ->
        load_or_init(scope, ctx, st)
    end
  end

  defp load_or_init(scope, ctx, st \\ %{}) do
    model = Db.get_by(CerebellumModel, scope: scope, context_key: ctx)

    rec =
      case model do
        nil ->
          %{
            weights: pad_or_trim(st[:init_weights] || cfg(:init_weights, [0.0, 0.05, 0.05, 0.03, 0.03]), st[:feature_dims] || cfg(:feature_dims, 5)),
            count_seen: 0,
            ema_error: 0.0,
            feature_schema: st[:feature_schema] || cfg(:feature_schema, 1),
            updated_at: now()
          }

        %CerebellumModel{} = m ->
          %{
            weights: pad_or_trim(m.weights || [], st[:feature_dims] || cfg(:feature_dims, 5)),
            count_seen: m.count_seen || 0,
            ema_error: m.ema_error || 0.0,
            feature_schema: m.feature_schema || 1,
            updated_at: now()
          }
      end

    :ets.insert(@table, {{scope, ctx}, rec})
    rec
  end

  defp maybe_flush(st) do
    if map_size(st.pending) >= st.flush_batch_size do
      do_flush(st)
    else
      st
    end
  end

  defp do_flush(%{pending: pend} = st) when map_size(pend) == 0, do: st
  defp do_flush(%{pending: pend} = st) do
    now_dt = DateTime.utc_now() |> DateTime.truncate(:second)

    entries =
      for {{scope, ctx}, rec} <- pend do
        %{
          scope: scope,
          context_key: ctx,
          weights: rec.weights,
          count_seen: rec.count_seen,
          ema_error: rec.ema_error,
          feature_schema: rec.feature_schema,
          inserted_at: now_dt,
          updated_at: now_dt
        }
      end

    Db.insert_all(
      CerebellumModel,
      entries,
      on_conflict: {:replace, [:weights, :count_seen, :ema_error, :feature_schema, :updated_at]},
      conflict_target: [:scope, :context_key]
    )

    %{st | pending: %{}}
  end

  # ---- Math & utils ----------------------------------------------------------

  defp cfg(k, default), do: Application.get_env(:brain, :cerebellum, %{}) |> Map.get(k, default)
  defp now, do: System.system_time(:millisecond)

  defp ema(prev, x, alpha) when is_number(x), do: prev + alpha * (x - prev)
  defp ema(prev, _x, _a), do: prev

  defp pad_or_trim(list, n) do
    case length(list) do
      ^n -> list
      m when m > n -> Enum.take(list, n)
      m when m < n -> list ++ List.duplicate(0.0, n - m)
    end
  end

  defp vec_add(a, b), do: Enum.zip(a, b) |> Enum.map(fn {x, y} -> x + y end)

  defp clip_norm(v, max) do
    n = :math.sqrt(Enum.reduce(v, 0.0, fn x, acc -> acc + x * x end))
    if n == 0.0 or n <= max, do: v, else: Enum.map(v, &(&1 * (max / n)))
  end
end

