# apps/brain/lib/brain/pipeline/lifg_stage1.ex
defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc """
  LIFG Stage-1 pipeline wrapper (brain-side).

  Guardrails:
  - brain must not depend on core (db <- brain <- core <- web)
  - do NOT pattern-match on `%Core.SemanticInput{}` here.
  - accept any map-like SI that carries `:tokens` (or "tokens") list.

  Contract (important — Brain.handle_call relies on this):
    run/4 returns {{:ok | :error, payload}, state}

  Payload shape on success:
    %{
      si: map(),
      choices: list(),
      cover: list(),
      boosts: list(),
      inhibitions: list(),
      audit: map()
    }
  """

  require Logger

  alias Brain.LIFG.{Post, Stage1}

  @type out_t :: %{
          si: map(),
          choices: list(),
          cover: list(),
          boosts: list(),
          inhibitions: list(),
          audit: map()
        }

  # -------------------------------------------------------------------
  # Public API (run/2)
  # -------------------------------------------------------------------

  @spec run(map() | list(), map() | keyword()) :: {:ok, out_t()} | {:error, any()}
  def run(si_or_tokens, cfg) when is_list(cfg), do: run(si_or_tokens, Map.new(cfg))

  def run(tokens, %{} = cfg) when is_list(tokens) do
    run(%{tokens: tokens}, cfg)
  end

  def run(%{} = si0, %{} = cfg) do
    tokens = fetch_tokens(si0)

    if is_list(tokens) do
      run_tokens(tokens, si0, cfg)
    else
      {:ok,
       %{
         si: si0,
         cover: [],
         choices: [],
         boosts: [],
         inhibitions: [],
         audit: %{stage: :lifg_stage1, reason: :no_tokens}
       }}
    end
  end

  # -------------------------------------------------------------------
  # Public API (run/4) — back-compat for Brain.handle_call
  # -------------------------------------------------------------------

  @spec run(map() | list(), any(), keyword() | map(), map()) ::
          {{:ok, out_t()} | {:error, any()}, map()}
  def run(si_or_tokens, ctx_vec, lifg_opts, state) when is_list(si_or_tokens) do
    cfg = %{ctx_vec: ctx_vec, lifg_opts: lifg_opts, state: state}

    case run(%{tokens: si_or_tokens}, cfg) do
      {:ok, out} -> {{:ok, out}, state}
      {:error, err} -> {{:error, err}, state}
    end
  end

  def run(%{} = si0, ctx_vec, lifg_opts, state) do
    cfg = %{ctx_vec: ctx_vec, lifg_opts: lifg_opts, state: state}

    case run(si0, cfg) do
      {:ok, out} -> {{:ok, out}, state}
      {:error, err} -> {{:error, err}, state}
    end
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

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
  # Core implementation
  # -------------------------------------------------------------------

  defp run_tokens(tokens, si0, cfg) do
    lifg_opts =
      Map.get(cfg, :lifg_opts) ||
        Map.get(cfg, "lifg_opts") ||
        Map.get(si0, :lifg_opts) ||
        Map.get(si0, "lifg_opts") ||
        []

    # Stage1.run/2 expects an SI-like map (it will source candidates from:
    # - si.sense_candidates / si.candidates_by_token (if present)
    # - si.active_cells (common in prod)
    si_for_stage =
      si0
      |> ensure_tokens(tokens)
      |> Brain.Utils.Safe.to_plain()

    t0 = System.monotonic_time(:millisecond)
    res = Stage1.run(si_for_stage, lifg_opts)
    dt = System.monotonic_time(:millisecond) - t0

    case res do
      {:ok, %{si: si1, choices: choices, audit: audit}} ->
        # Compute cover AFTER Stage1, before logging.
        post =
          Post.finalize(si1, choices,
            reanalysis?:
              Keyword.get(lifg_opts, :reanalysis?, false) or
                Keyword.get(lifg_opts, :reanalysis, false),
            allow_overlaps?: Keyword.get(lifg_opts, :allow_overlaps?, false)
          )

        out = %{
          si: si1,
          choices: choices,
          cover: Map.get(post, :cover, []),
          boosts: [],
          inhibitions: [],
          audit: audit
        }

        log_summary(dt, {:ok, out})
        {:ok, out}

      {:error, err} ->
        log_summary(dt, {:error, err})
        {:error, err}

      other ->
        err =
          RuntimeError.exception(
            "Unexpected return from Brain.LIFG.Stage1.run/2: #{inspect(other)}"
          )

        log_summary(dt, {:error, err})
        {:error, err}
    end
  end

  defp ensure_tokens(%{} = si, tokens) when is_list(tokens) do
    si
    |> Map.put(:tokens, tokens)
    |> Map.put("tokens", tokens)
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
      %{
        token_index: Map.get(item, :token_index) || Map.get(item, "token_index"),
        id: Map.get(item, :id) || Map.get(item, "id"),
        span: Map.get(item, :span) || Map.get(item, "span"),
        margin: Map.get(item, :margin) || Map.get(item, "margin")
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
    weak = if is_map(audit), do: Map.get(audit, :weak_decisions), else: nil
    missing = if is_map(audit), do: Map.get(audit, :missing_candidates), else: nil

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
    Logger.error(fn -> "[LIFGStage1] #{dt_ms}ms error: " <> Exception.message(err) end)
  end

  defp log_summary(_dt_ms, _other), do: :ok
end
