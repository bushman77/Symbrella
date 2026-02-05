# apps/brain/lib/brain/pipeline/lifg_stage1.ex
defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc """
  LIFG Stage-1 pipeline wrapper (brain-side).

  Guardrails:
  - brain must not depend on core (db <- brain <- core <- web)
  - therefore do NOT pattern-match on `%Core.SemanticInput{}` here.
  - accept any map-like SI that carries `:tokens` (or "tokens") list.

  Returns:
    {:ok, %{si: si, cover: cover, choices: choices, boosts: boosts, inhibitions: inhibitions, audit: audit}}
  or
    {:error, exception}
  """

  require Logger
  alias Brain.LIFG.Stage1

  # -------------------------------------------------------------------
  # Public API
  # -------------------------------------------------------------------

  @doc """
  Primary entry used in multiple places (including Brain).

  Supports:
    - run(si_map, cfg_map_or_kw)
    - run(tokens_list, cfg_map_or_kw)  (treated as %{tokens: tokens})
  """
  @spec run(map() | list(), map() | keyword()) :: {:ok, map()} | {:error, any()}
  def run(si_or_tokens, cfg) when is_list(cfg), do: run(si_or_tokens, Map.new(cfg))

  def run(tokens, %{} = cfg) when is_list(tokens) do
    run(%{tokens: tokens}, cfg)
  end

  def run(%{} = si0, %{} = cfg) do
    tokens = fetch_tokens(si0)

    if is_list(tokens) do
      run_tokens(tokens, si0, cfg)
    else
      out = %{
        si: si0,
        cover: [],
        choices: [],
        boosts: [],
        inhibitions: [],
        audit: %{stage: :lifg_stage1, reason: :no_tokens}
      }

      {:ok, out}
    end
  rescue
    err ->
      st = __STACKTRACE__
      Logger.error(fn -> "[LIFGStage1] crash: " <> format_error(err, st) end)
      {:error, err}
  catch
    kind, reason ->
      st = __STACKTRACE__
      err = normalize_catch(kind, reason, st)
      Logger.error(fn -> "[LIFGStage1] crash: " <> format_error(err, st) end)
      {:error, err}
  end

  @doc """
  Back-compat entry: Brain.handle_call is currently invoking `run/4`.

  Accepts either:
    - an SI-like map/struct (map-shaped), OR
    - a tokens list

  `lifg_opts` should be a keyword list (or map) of LIFG options,
  and `state` is an opaque pipeline state map.
  """
  @spec run(map() | list(), any(), keyword() | map(), map()) :: {:ok, map()} | {:error, any()}
  def run(si_or_tokens, ctx_vec, lifg_opts, state) when is_list(si_or_tokens) do
    cfg = %{ctx_vec: ctx_vec, lifg_opts: lifg_opts, state: state}
    run(%{tokens: si_or_tokens}, cfg)
  end

  def run(%{} = si0, ctx_vec, lifg_opts, state) do
    cfg = %{ctx_vec: ctx_vec, lifg_opts: lifg_opts, state: state}
    run(si0, cfg)
  end

  @doc """
  Extract a compact `lifg_choices` list from a pipeline output.

  Accepts:
    - `{:ok, %{cover: cover, ...}}`
    - `%{cover: cover, ...}`
    - `{:ok, %{si: %{lifg_choices: ...}}}` etc (best-effort)
  """
  @spec extract_lifg_choices(any()) :: list(map())
  def extract_lifg_choices({:ok, %{} = out}), do: extract_lifg_choices(out)

  def extract_lifg_choices(%{} = out) do
    cond do
      is_list(Map.get(out, :lifg_choices)) ->
        Map.get(out, :lifg_choices)

      is_list(get_in(out, [:si, :lifg_choices])) ->
        get_in(out, [:si, :lifg_choices])

      true ->
        cover = Map.get(out, :cover) || get_in(out, [:si, :cover]) || []
        normalize_cover(cover)
    end
  end

  def extract_lifg_choices(_), do: []

  # -------------------------------------------------------------------
  # Internals
  # -------------------------------------------------------------------

  defp run_tokens(tokens, si0, cfg) do
    ctx_vec = Map.get(cfg, :ctx_vec) || Map.get(cfg, "ctx_vec")

    lifg_opts =
      Map.get(cfg, :lifg_opts) ||
        Map.get(cfg, "lifg_opts") ||
        Map.get(si0, :lifg_opts) ||
        Map.get(si0, "lifg_opts") ||
        []

    stage_opts = [
      ctx_vec: ctx_vec,
      lifg_opts: lifg_opts,
      state: Map.get(cfg, :state) || Map.get(cfg, "state") || %{}
    ]

    t0 = System.monotonic_time(:millisecond)
    res = Stage1.choose(tokens, ctx_vec, stage_opts)
    dt = System.monotonic_time(:millisecond) - t0

    log_summary(dt, res)

    case res do
      {:ok, %{} = out} ->
        {:ok, ensure_si(out, si0)}

      {:error, %{} = err} ->
        {:error, err}

      other ->
        {:error,
         RuntimeError.exception(
           "Unexpected return from Brain.LIFG.Stage1.choose/3: #{inspect(other)}"
         )}
    end
  end

  defp ensure_si(%{} = out, si0) do
    case Map.get(out, :si) do
      %{} -> out
      _ -> Map.put(out, :si, si0)
    end
  end

  defp fetch_tokens(%{} = si0) do
    cond do
      Map.has_key?(si0, :tokens) -> Map.get(si0, :tokens)
      Map.has_key?(si0, "tokens") -> Map.get(si0, "tokens")
      true -> nil
    end
  end

  defp normalize_cover(cover) when is_list(cover) do
    cover
    |> Enum.map(fn item ->
      id = Map.get(item, :id) || Map.get(item, "id")
      token_index = Map.get(item, :token_index) || Map.get(item, "token_index")
      span = Map.get(item, :span) || Map.get(item, "span")
      margin = Map.get(item, :margin) || Map.get(item, "margin")

      %{
        token_index: token_index,
        id: id,
        span: span,
        margin: margin
      }
    end)
    |> Enum.reject(fn m -> is_nil(m.id) end)
  end

  defp normalize_cover(_), do: []

  defp log_summary(dt_ms, {:ok, %{} = out}) do
    winners = out |> Map.get(:cover, []) |> length()
    boosts = out |> Map.get(:boosts, []) |> length()
    inhibs = out |> Map.get(:inhibitions, []) |> length()

    Logger.info(fn ->
      "[LIFG] #{dt_ms}ms winners=#{winners} boosts=#{boosts} inhibitions=#{inhibs} groups=nil ctx_dim=nil norm=nil scores=nil parallel=nil"
    end)
  end

  defp log_summary(dt_ms, {:error, err}) do
    Logger.error(fn -> "[LIFGStage1] #{dt_ms}ms error: " <> format_error(err, []) end)
  end

  defp log_summary(_dt_ms, _other), do: :ok

  defp normalize_catch(:exit, reason, _st),
    do: RuntimeError.exception("exit: #{inspect(reason)}")

  defp normalize_catch(:throw, reason, _st),
    do: RuntimeError.exception("throw: #{inspect(reason)}")

  defp normalize_catch(:error, reason, _st) when is_exception(reason),
    do: reason

  defp normalize_catch(kind, reason, _st),
    do: RuntimeError.exception("#{inspect(kind)}: #{inspect(reason)}")

  defp format_error({err, st}, _fallback_st) when is_list(st) do
    if is_exception(err), do: Exception.format(:error, err, st), else: inspect({err, st})
  end

  defp format_error(err, st) when is_list(st) do
    if is_exception(err), do: Exception.format(:error, err, st), else: inspect(err)
  end

  defp format_error(err, _st), do: inspect(err)
end
