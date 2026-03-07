# apps/brain/lib/brain/pipeline/lifg_stage1.ex
defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc """
  LIFG Stage-1 pipeline wrapper (brain-side).

  Guardrails:
    - brain must not depend on core (db <- brain <- core <- web)
    - therefore do NOT pattern-match on `%Core.SemanticInput{}` here.
    - accept any map-like SI that carries `:tokens` (or "tokens") list.

  IMPORTANT CONTRACT (Brain GenServer)
  -----------------------------------
  Brain.handle_call uses:

      {reply, state2} = Brain.Pipeline.LIFGStage1.run(si_or_cands, ctx_vec, opts, state)

  Therefore `run/4` MUST return `{reply, new_state}` where:
    - reply is `{:ok, out}` or `{:error, err}`
    - new_state is the updated Brain server state map (must keep :wm, :wm_cfg, etc.)
  """

  require Logger

  alias Brain.LIFG.{Post, Stage1}
  alias Brain.Utils.Safe
  alias Brain.WM.Gate, as: WMGate

  # -------------------------------------------------------------------
  # Public API (non-GenServer callers)
  # -------------------------------------------------------------------

  @doc """
  Primary entry used by non-GenServer callers.

  Supports:
    - run(si_map, cfg_map_or_kw)
    - run(tokens_list, cfg_map_or_kw)  (treated as %{tokens: tokens})

  NOTE: this does NOT update WM because it has no access to Brain state.
  """
  @spec run(map() | list(), map() | keyword()) :: {:ok, map()} | {:error, any()}
  def run(si_or_tokens, cfg) when is_list(cfg), do: run(si_or_tokens, Map.new(cfg))

  def run(tokens, %{} = cfg) when is_list(tokens) do
    run(%{tokens: tokens}, cfg)
  end

  def run(%{} = si0, %{} = cfg) do
    si_plain = Safe.to_plain(si0)
    tokens = fetch_tokens(si_plain)

    if is_list(tokens) do
      ctx_vec = fetch_ctx_vec(cfg)
      lifg_opts = fetch_lifg_opts(si_plain, cfg)

      # Non-GenServer entrypoint: run/4 with an empty state and NO WM gating.
      lifg_opts2 = Keyword.put_new(lifg_opts, :gate_into_wm, false)

      {reply, _state2} = run(si_plain, ctx_vec, lifg_opts2, %{})
      reply
    else
      {:ok, empty_out(si_plain, :no_tokens)}
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

  # -------------------------------------------------------------------
  # Brain GenServer contract entry
  # -------------------------------------------------------------------

  @doc """
  Brain GenServer contract entry.

  MUST return `{reply, new_state}` so Brain can keep its internal state.
  """
  @spec run(map() | list(), any(), keyword() | map(), map()) :: {term(), map()}
  def run(si_or_tokens, ctx_vec, lifg_opts, state)

  def run(tokens, ctx_vec, lifg_opts, state) when is_list(tokens) do
    run(%{tokens: tokens}, ctx_vec, lifg_opts, state)
  end

  def run(%{} = si0, ctx_vec, lifg_opts, state) when is_map(state) do
    si_plain = Safe.to_plain(si0)

    tokens =
      si_plain
      |> fetch_tokens()
      |> List.wrap()
      |> Enum.map(&Safe.to_plain/1)

    lifg_opts2 = normalize_lifg_opts(lifg_opts)

    # OPTION A default: when called inside Brain, gate_into_wm defaults to TRUE
    # unless explicitly disabled.
    lifg_opts3 = Keyword.put_new(lifg_opts2, :gate_into_wm, true)

    if tokens == [] do
      {{:ok, empty_out(si_plain, :no_tokens)}, state}
    else
      run_tokens(tokens, si_plain, ctx_vec, lifg_opts3, state)
    end
  rescue
    err ->
      st = __STACKTRACE__
      Logger.error(fn -> "[LIFGStage1] crash: " <> format_error(err, st) end)

      # CRITICAL: never corrupt Brain state
      {{:error, err}, state}
  catch
    kind, reason ->
      st = __STACKTRACE__
      err = normalize_catch(kind, reason, st)
      Logger.error(fn -> "[LIFGStage1] crash: " <> format_error(err, st) end)

      # CRITICAL: never corrupt Brain state
      {{:error, err}, state}
  end

  # -------------------------------------------------------------------
  # Helpers: extracting lifg choices
  # -------------------------------------------------------------------

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

  defp run_tokens(tokens, si0, ctx_vec, lifg_opts, state) do
# Stage1.run/2 is the correct bridge here: it needs the full SI map so it can
# use sentence/intent/active_cells/sense_candidates/candidates_by_token/etc.
t0 = System.monotonic_time(:millisecond)

si_for_stage1 =
  si0
  |> Map.put(:tokens, tokens)
  |> Map.put(:lifg_opts, lifg_opts)

res =
  Stage1.run(
    si_for_stage1,
    Keyword.merge(lifg_opts, ctx_vec: ctx_vec, state: state)
  )
    dt = System.monotonic_time(:millisecond) - t0

    case res do
      {:ok, %{} = out0} ->
        out1 =
          out0
          |> ensure_si(si0)
          |> ensure_cover(lifg_opts)

        # Put normalized lifg choices on SI so WM ingestion has a stable shape.
        si1 = ensure_si_has_lifg_choices(out1)
        out2 = Map.put(out1, :si, si1)

        # IMPORTANT: log AFTER cover is finalized
        log_summary(dt, {:ok, out2})

        state2 =
          if Keyword.get(lifg_opts, :gate_into_wm, false) do
            ingest_wm(state, si1)
          else
            state
          end

        {{:ok, out2}, state2}

      {:error, %{} = err} ->
        log_summary(dt, {:error, err})
        {{:error, err}, state}

      other ->
        raise RuntimeError,
              "Unexpected return from Brain.LIFG.Stage1.choose/3: #{inspect(other)}"
    end
  end

  defp ingest_wm(state, si) when is_map(state) and is_map(si) do
    allow_fb? = get_in(state, [:wm_cfg, :allow_fallback_into_wm?]) == true

    min_score =
      case get_in(state, [:wm_cfg, :gate_threshold]) do
        n when is_number(n) -> n * 1.0
        _ -> 0.0
      end

    WMGate.ingest_from_si(state, si,
      allow_fallback?: allow_fb?,
      allow_phrase_fallback?: allow_fb?,
      min_score: min_score
    )
  end

  defp ingest_wm(state, _si), do: state

defp ensure_si_has_lifg_choices(%{} = out) do
  si = Map.get(out, :si, %{}) |> Safe.to_plain()

  cover =
    out
    |> Map.get(:cover, [])
    |> List.wrap()
    |> Enum.map(&Safe.to_plain/1)

  choices =
    out
    |> Map.get(:choices, [])
    |> List.wrap()
    |> Enum.map(&Safe.to_plain/1)

  choice_by_key = lifg_choice_lookup(choices)

  lifg_choices =
    cover
    |> Enum.map(fn cov ->
      id = cov[:id] || cov["id"] || cov[:chosen_id] || cov["chosen_id"]
      tok_i = cov[:token_index] || cov["token_index"] || cov[:index] || cov["index"]
      span = cov[:span] || cov["span"]

      raw_choice =
        Map.get(choice_by_key, {tok_i, to_string(id)}, %{})

      prob =
        raw_choice[:prob] ||
          raw_choice["prob"] ||
          raw_choice[:score] ||
          raw_choice["score"] ||
          0.0

      margin =
        cov[:margin] ||
          cov["margin"] ||
          raw_choice[:margin] ||
          raw_choice["margin"] ||
          raw_choice[:prob_margin] ||
          raw_choice["prob_margin"] ||
          0.0

      %{
        id: to_string(id),
        chosen_id: to_string(id),
        token_index: tok_i,
        span: span,
        prob: as_float(prob),
        margin: as_float(margin),
        score: clamp01(as_float(prob)),
        source: :lifg
      }
    end)
    |> Enum.reject(fn ch ->
      ch.id == "" or is_nil(ch.token_index)
    end)

  Map.put(si, :lifg_choices, lifg_choices)
end

defp lifg_choice_lookup(choices) when is_list(choices) do
  Enum.reduce(choices, %{}, fn ch, acc ->
    tok_i = ch[:token_index] || ch["token_index"] || ch[:index] || ch["index"]
    id = ch[:chosen_id] || ch["chosen_id"] || ch[:id] || ch["id"]

    cond do
      is_nil(tok_i) or is_nil(id) ->
        acc

      true ->
        Map.put(acc, {tok_i, to_string(id)}, ch)
    end
  end)
end

  defp ensure_cover(%{} = out, lifg_opts) do
    cover = Map.get(out, :cover)

    if is_list(cover) and cover != [] do
      out
    else
      si = Map.get(out, :si, %{})
      choices = Map.get(out, :choices, [])

      post =
        Post.finalize(si, choices,
          reanalysis?:
            Keyword.get(lifg_opts, :reanalysis?, false) or
              Keyword.get(lifg_opts, :reanalysis, false),
          allow_overlaps?: Keyword.get(lifg_opts, :allow_overlaps?, false)
        )

      out
      |> Map.put(:cover, Map.get(post, :cover, []))
      |> Map.put_new(:boosts, [])
      |> Map.put_new(:inhibitions, [])
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

  defp fetch_ctx_vec(cfg) do
    v = Map.get(cfg, :ctx_vec) || Map.get(cfg, "ctx_vec")
    if is_list(v), do: v, else: []
  end

  defp fetch_lifg_opts(si0, cfg) do
    opts =
      Map.get(cfg, :lifg_opts) ||
        Map.get(cfg, "lifg_opts") ||
        Map.get(si0, :lifg_opts) ||
        Map.get(si0, "lifg_opts") ||
        []

    normalize_lifg_opts(opts)
  end

  defp normalize_lifg_opts(opts) when is_list(opts), do: opts
  defp normalize_lifg_opts(opts) when is_map(opts), do: Map.to_list(opts)
  defp normalize_lifg_opts(_), do: []

  defp empty_out(si, reason) do
    %{
      si: si,
      cover: [],
      choices: [],
      boosts: [],
      inhibitions: [],
      audit: %{stage: :lifg_stage1, reason: reason}
    }
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
    choices_count = out |> Map.get(:choices, []) |> length()
    audit = Map.get(out, :audit, %{})

    weak =
      case audit do
        %{} -> Map.get(audit, :weak_decisions, nil)
        _ -> nil
      end

    missing =
      case audit do
        %{} -> Map.get(audit, :missing_candidates, nil)
        _ -> nil
      end

    Logger.info(fn ->
      weak_s = if is_integer(weak), do: " weak=#{weak}", else: ""
      miss_s = if is_integer(missing), do: " missing=#{missing}", else: ""

      "[LIFG] #{dt_ms}ms choices=#{choices_count} winners=#{winners} boosts=#{boosts} inhibitions=#{inhibs}" <>
        weak_s <>
        miss_s <>
        " groups=nil ctx_dim=nil norm=nil scores=nil parallel=nil"
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

  defp as_float(v) when is_float(v), do: v
  defp as_float(v) when is_integer(v), do: v * 1.0

  defp as_float(v) when is_binary(v) do
    case Float.parse(v) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  defp clamp01(x) when is_number(x) do
    cond do
      x < 0.0 -> 0.0
      x > 1.0 -> 1.0
      true -> x * 1.0
    end
  end
end
