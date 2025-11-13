defmodule Brain.Cell.Safe do
  @moduledoc """
  Resilient GenServer call wrapper for Brain *cells* (region processes, micro-agents, etc.).

  Goals:
  • Contain timeouts/crashes so a single flaky cell never freezes the loop.
  • Uniform error taxonomy: `{:ok, reply}` | `{:error, reason}`.
  • Emit telemetry for latency + errors without introducing a hard dependency on metrics libs.
  • Optional tiny retry (with jitter) for transient transport errors.

  Telemetry events (native time units for duration):
    [:brain, :cell, :call]
    [:brain, :cell, :call, :error]

  Metadata:
    %{cell: cell, tag: tag(msg), timeout: timeout, retry: retried?, attempts: n, reason: reason?}

  Configure (optional) in `config/*.exs` under :brain, Brain.Cell.Safe:
    config :brain, Brain.Cell.Safe,
      default_timeout_ms: 150,
      default_retry?: true,
      default_retries: 1,
      warn_ms: 200
  """

  require Logger
  alias Brain.Cell.Telemetry, as: CellTelemetry

  @cfg Application.compile_env(:brain, __MODULE__, [])
  @default_timeout_ms Keyword.get(@cfg, :default_timeout_ms, 150)
  @default_retry? Keyword.get(@cfg, :default_retry?, true)
  @default_retries Keyword.get(@cfg, :default_retries, 1)

  @type reason ::
          :timeout
          | :noproc
          | :noconnection
          | {:exit, term()}
          | {:raise, term()}
          | {:error, term()}

  @doc """
  Safe call into a cell.

  Options:
    * :timeout      — ms (default #{@default_timeout_ms})
    * :retry?       — boolean (default #{@default_retry?})
    * :retries      — integer (default #{@default_retries}) — max extra attempts on transient errors
  """
  @spec call(GenServer.server(), term(), keyword()) :: {:ok, term()} | {:error, reason()}
  def call(cell, msg, opts \\ []) do
    # lazy, idempotent
    CellTelemetry.ensure_attached()

    timeout = opts[:timeout] || @default_timeout_ms
    retry? = opts[:retry?] || @default_retry?
    retries = opts[:retries] || @default_retries

    do_call(cell, msg, timeout, retry?, retries, 1)
  end

  # ---- Internal --------------------------------------------------------------

  defp do_call(cell, msg, timeout, retry?, retries_left, attempt) do
    start = System.monotonic_time()

    try do
      reply = GenServer.call(cell, msg, timeout)
      emit_ok(cell, msg, start, timeout, attempt)
      {:ok, reply}
    catch
      :exit, {:timeout, _} ->
        emit_err(cell, msg, start, timeout, attempt, :timeout)
        {:error, :timeout}

      :exit, {:noproc, _} = e ->
        transient_retry_or_error(
          cell,
          msg,
          start,
          timeout,
          attempt,
          :noproc,
          e,
          retry?,
          retries_left
        )

      :exit, {:noconnection, _} = e ->
        transient_retry_or_error(
          cell,
          msg,
          start,
          timeout,
          attempt,
          :noconnection,
          e,
          retry?,
          retries_left
        )

      :exit, reason ->
        emit_err(cell, msg, start, timeout, attempt, {:exit, reason})
        {:error, {:exit, reason}}

      kind, reason ->
        emit_err(cell, msg, start, timeout, attempt, {:raise, {kind, reason}})
        {:error, {:raise, {kind, reason}}}
    end
  end

  # NOTE: `_meta` retained for future tagging but intentionally unused.
  defp transient_retry_or_error(
         cell,
         msg,
         start,
         timeout,
         attempt,
         tag_reason,
         _meta,
         retry?,
         retries_left
       ) do
    if retry? and retries_left > 0 do
      emit_err(cell, msg, start, timeout, attempt, tag_reason, retried?: true)
      jitter_ms = 5 + :rand.uniform(25)
      Process.sleep(jitter_ms)
      do_call(cell, msg, timeout, true, retries_left - 1, attempt + 1)
    else
      emit_err(cell, msg, start, timeout, attempt, tag_reason)
      {:error, tag_reason}
    end
  end

  defp emit_ok(cell, msg, start, timeout, attempt) do
    :telemetry.execute(
      [:brain, :cell, :call],
      %{duration: dur(start)},
      %{cell: cell, tag: tag(msg), timeout: timeout, attempts: attempt, retry: attempt > 1}
    )
  end

  defp emit_err(cell, msg, start, timeout, attempt, reason, opts \\ []) do
    :telemetry.execute(
      [:brain, :cell, :call, :error],
      %{duration: dur(start)},
      %{
        cell: cell,
        tag: tag(msg),
        timeout: timeout,
        attempts: attempt,
        retry: Keyword.get(opts, :retried?, false),
        reason: reason
      }
    )
  end

  defp dur(start), do: System.monotonic_time() - start

  # Turn a request tuple into a compact atom for metrics (e.g., {:step, :lifg, data} -> :step)
  defp tag({t, _}) when is_atom(t), do: t
  defp tag({t, _, _}) when is_atom(t), do: t
  defp tag({t, _, _, _}) when is_atom(t), do: t
  defp tag(t) when is_atom(t), do: t
  defp tag(_), do: :unknown
end
