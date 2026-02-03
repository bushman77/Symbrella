# apps/brain/lib/brain/pipeline/lifg_stage1.ex
defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc """
  Stage-1 orchestrator wrapper for LIFG.

  Key goals:
  - NEVER expand optional structs at compile-time (no %Brain.PMTG.Query{}, no %Brain.LIFG.Out{}).
  - Be resilient to missing/disabled modules.
  - Normalize token shapes early so downstream code doesn't crash on binaries.
  - Best-effort episode writing (success + error), controlled by `:brain, :episodes_mode`.

  Expected return: updated SI map (or input SI on failure).
  """

  require Logger
  alias Brain.Episodes.Writer, as: EpWriter

  @default_tags ["auto", "lifg"]
  @max_err_len 240

  @spec run(map(), keyword()) :: map()
def run(%{} = si, opts \\ []) when is_list(opts) do
  sentence = Map.get(si, :sentence)

  # IMPORTANT: treat nil as "not provided" so it falls back to app env
  episodes_opt = Keyword.get(opts, :episodes, :__unset__)
  episodes_mode =
    case episodes_opt do
      :__unset__ -> Application.get_env(:brain, :episodes_mode, :async)
      nil        -> Application.get_env(:brain, :episodes_mode, :async)
      other      -> other
    end
    |> EpWriter.normalize_episode_mode()

  tags_opt = Keyword.get(opts, :episodes_tags, :__unset__)
  episodes_tags =
    case tags_opt do
      :__unset__ -> Application.get_env(:brain, :episodes_tags, [])
      nil        -> Application.get_env(:brain, :episodes_tags, [])
      other      -> other
    end
    |> EpWriter.normalize_episode_tags()

  with {:ok, %{} = si1} <- maybe_consult_pmtg(si, opts),
       {:ok, %{} = si2, out, audit} <- safe_disambiguate_stage1(si1, opts),
       {:ok, %{} = si_after} <- maybe_rescale(si2, audit, opts) do
    :telemetry.execute(@tele_stop, lifg_stop_meas(audit), %{winners: out.winners, boosts: out.boosts, inhibitions: out.inhibitions})

    maybe_store_episode(si_after, episodes_mode, episodes_tags)

    {:ok, si_after, out, audit}
  else
    {:error, reason} ->
      Logger.error("[LIFG] run failed: #{inspect(reason)}")

      maybe_store_episode(trace_error(si, reason), episodes_mode, ["auto", "lifg", "lifg_error"] ++ episodes_tags)
      {:error, reason}
  end
end

  def run(other, _opts), do: other

  # --- Optional PMTG consult (stub-safe) -------------------------------------

  # Keep this as a no-op by default. If/when you enable it,
  # it won't introduce compile-time struct dependencies.
  defp maybe_consult_pmtg(%{} = si, opts) do
    enabled? = Keyword.get(opts, :consult_pmtg, false)

    if enabled? do
      pmtg = :"Elixir.Brain.PMTG"

      if Code.ensure_loaded?(pmtg) and function_exported?(pmtg, :fetch_evidence, 3) do
        # Best-effort: pass a plain map; PMTG can ignore/shape it as needed.
        query = %{
          cues: cues_from_si(si),
          limit: Keyword.get(opts, :pmtg_limit, 5),
          scope: Keyword.get(opts, :scope, nil)
        }

        try do
          # Expecting something like: %{episodes: [...], lexicon: [...]}
          evidence = apply(pmtg, :fetch_evidence, [si, opts, query])

          if is_map(evidence) do
            put_in(si, [:evidence], Map.merge(Map.get(si, :evidence, %{}), evidence))
          else
            si
          end
        rescue
          e ->
            put_trace_error(si, "pmtg_exception=#{Exception.message(e)}")
        catch
          kind, reason ->
            put_trace_error(si, "pmtg_#{kind}=#{inspect(reason)}")
        end
      else
        si
      end
    else
      si
    end
  end

  # --- LIFG Stage1 safe call --------------------------------------------------

  defp safe_lifg_stage1(%{} = si, opts) do
    lifg = :"Elixir.Brain.LIFG"

    cond do
      not Code.ensure_loaded?(lifg) ->
        {:error, :lifg_not_loaded, put_trace_error(si, "lifg_not_loaded")}

      function_exported?(lifg, :disambiguate_stage1, 2) ->
        try do
          # We accept ANY return shape and coerce.
          case apply(lifg, :disambiguate_stage1, [si, opts]) do
            {:ok, %{} = si_after, _out, _audit} -> {:ok, si_after}
            {:ok, %{} = si_after, _audit} -> {:ok, si_after}
            {:ok, %{} = si_after} -> {:ok, si_after}
            %{} = si_after -> {:ok, si_after}
            {:error, reason} -> {:error, reason, put_trace_error(si, fmt_reason(reason))}
            other -> {:error, {:unexpected, other}, put_trace_error(si, "lifg_return=#{inspect(other)}")}
          end
        rescue
          e ->
            {:error, {:exception, e}, put_trace_error(si, fmt_exception(e))}
        catch
          kind, reason ->
            {:error, {kind, reason}, put_trace_error(si, "lifg_#{kind}=#{inspect(reason)}")}
        end

      true ->
        {:error, :lifg_missing_fun, put_trace_error(si, "lifg_missing_disambiguate_stage1/2")}
    end
  end

  # --- Normalization ----------------------------------------------------------

  defp normalize_si(%{} = si) do
    si
    |> normalize_tokens()
    |> ensure_trace_list()
  end

  defp ensure_trace_list(%{} = si) do
    case Map.get(si, :trace) do
      list when is_list(list) -> si
      _ -> Map.put(si, :trace, [])
    end
  end

  # Converts ["good","morning"] into token maps so nobody calls Map.get/2 on a binary.
  defp normalize_tokens(%{} = si) do
    tokens = Map.get(si, :tokens, [])

    cond do
      is_list(tokens) and Enum.all?(tokens, &is_binary/1) ->
        mapped =
          tokens
          |> Enum.with_index()
          |> Enum.map(fn {t, idx} ->
            %{
              text: t,
              surface: t,
              norm: String.downcase(t),
              lemma: String.downcase(t),
              token_index: idx,
              index: idx,
              n: 1,
              mw: false
            }
          end)

        Map.put(si, :tokens, mapped)

      true ->
        si
    end
  end

  defp cues_from_si(%{} = si) do
    toks = Map.get(si, :tokens, [])

    toks
    |> List.wrap()
    |> Enum.map(fn
      %{lemma: l} when is_binary(l) -> l
      %{norm: n} when is_binary(n) -> n
      %{text: t} when is_binary(t) -> t
      t when is_binary(t) -> t
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  # --- Trace helpers ----------------------------------------------------------

  defp put_trace_error(%{} = si, msg) when is_binary(msg) do
    entry = %{error: String.slice(msg, 0, @max_err_len), at_ms: now_ms()}
    trace = Map.get(si, :trace, [])
    Map.put(si, :trace, trace ++ [entry])
  end

  defp fmt_exception(e) do
    "exception=" <> Exception.message(e)
  end

  defp fmt_reason(reason) do
    inspect(reason) |> String.slice(0, @max_err_len)
  end

  defp now_ms, do: System.system_time(:millisecond)

  # --- Config ----------------------------------------------------------------

  defp episodes_mode do
    Application.get_env(:brain, :episodes_mode, :async)
    |> EpWriter.normalize_episode_mode()
  end

  defp episodes_tags do
    Application.get_env(:brain, :episodes_tags, @default_tags)
    |> List.wrap()
    |> Enum.map(&to_string/1)
  end
end
