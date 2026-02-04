# apps/brain/lib/brain/pipeline/lifg_stage1.ex
defmodule Brain.Pipeline.LIFGStage1 do
  @moduledoc """
  Brain-side LIFG Stage-1 pipeline wrapper.

  Guardrails:
  - Do NOT compile-time depend on Core structs (e.g. Core.SemanticInput).
  - Accepts "SI-like" maps (Core may pass structs at runtime; Brain treats them as maps).

  Expected input (minimum):
    - map with :sentence (binary) and/or :tokens (list)
  """

  require Logger

  alias Brain.LIFG

  @type si_like :: map()

  @spec run(si_like(), any(), keyword(), map()) ::
          {:ok, map()} | {:error, any()}
  def run(si_like, ctx_vec \\ nil, opts \\ [], state \\ %{})
      when is_map(si_like) and is_list(opts) and is_map(state) do
    lifg_opts = Keyword.get(opts, :lifg_opts, [])
    scores_mode = Keyword.get(lifg_opts, :scores_mode)

    t0 = System.monotonic_time()

    # Keep everything map-shaped inside Brain to avoid Core struct coupling.
    si =
      si_like
      |> Map.put_new(:trace, [])
      |> Map.put_new(:active_cells, [])
      |> Map.put_new(:sense_candidates, %{})
      |> Map.put_new(:intent_bias, %{})
      |> Map.put_new(:tokens, [])
      |> Map.put_new(:sentence, "")
      |> Map.put(:source, Map.get(si_like, :source, :prod))
      |> Map.put(:frame, Map.get(si_like, :frame, %{seq: nil, run_id: nil, ts_ms: nil}))
      |> Map.put(:frame_seq, Map.get(si_like, :frame_seq, nil))
      |> Map.put(:frame_run_id, Map.get(si_like, :frame_run_id, nil))
      |> Map.put(:frame_ts_ms, Map.get(si_like, :frame_ts_ms, nil))
      |> Map.put(:lifg_opts, lifg_opts)

    Logger.info(fn ->
      "[LIFG] 0ms groups=nil ctx_dim=nil norm=nil scores=#{inspect(scores_mode)} parallel=nil"
    end)

    res = safe_disambiguate_stage1(si, ctx_vec, lifg_opts, state)

    dt_ms =
      System.monotonic_time()
      |> Kernel.-(t0)
      |> System.convert_time_unit(:native, :millisecond)

    case res do
      {:ok, %{cover: cover} = out} ->
        Logger.info(fn ->
          "[LIFG] #{dt_ms}ms winners=#{length(cover)} boosts=#{length(Map.get(out, :boosts, []))} " <>
            "inhibitions=#{length(Map.get(out, :inhibitions, []))} groups=nil ctx_dim=nil norm=nil scores=nil parallel=nil"
        end)

        {:ok, out}

      {:error, {err, st}} ->
        Logger.error(fn -> "[LIFGStage1] crash: #{Exception.format(:error, err, st)}" end)
        {:error, err}

      {:error, err} ->
        Logger.error(fn -> "[LIFGStage1] crash: #{Exception.message(err)}" end)
        {:error, err}
    end
  end

  # ───────────────────────────────────────────────────────────────────
  # Internal
  # ───────────────────────────────────────────────────────────────────

  defp safe_disambiguate_stage1(si, ctx_vec, lifg_opts, state) do
    try do
      # Pass ctx_vec/state via opts to avoid changing the SI shape.
      lifg_opts2 =
        lifg_opts
        |> Keyword.put_new(:ctx_vec, ctx_vec)
        |> Keyword.put_new(:state, state)

      case LIFG.disambiguate_stage1(si, lifg_opts2) do
        {:ok, cover, choices, boosts, inhibitions, audit} ->
          {:ok,
           %{
             si: si,
             cover: cover,
             choices: choices,
             boosts: boosts,
             inhibitions: inhibitions,
             audit: audit
           }}

        other ->
          {:ok, %{si: si, cover: [], choices: [], boosts: [], inhibitions: [], audit: %{other: other}}}
      end
    rescue
      e ->
        {:error, {e, __STACKTRACE__}}
    end
  end

  @doc """
  Normalize any of the pipeline return shapes into a flat list of LIFG "cover" choices.

  Returns list of:
    %{id, span, token_index, margin}

  Accepted shapes:
    * {:ok, %{cover: cover}} (preferred)
    * {:ok, %{cover: cover, si: _}}
    * {:ok, cover, _choices, _boosts, _inhibitions, _audit} (legacy)
    * {:ok, %{choices: choices}} (fallback)
    * {:error, _} / anything else -> []
  """
  @spec extract_lifg_choices(any()) :: [map()]
  def extract_lifg_choices(result) do
    cond do
      match?({:ok, %{cover: _}}, result) ->
        {:ok, %{cover: cover}} = result
        normalize_cover_list(cover)

      match?({:ok, %{cover: _, si: _}}, result) ->
        {:ok, %{cover: cover}} = result
        normalize_cover_list(cover)

      match?({:ok, _cover, _choices, _boosts, _inhibitions, _audit}, result) ->
        {:ok, cover, _choices, _boosts, _inhibitions, _audit} = result
        normalize_cover_list(cover)

      match?({:ok, %{choices: _}}, result) ->
        {:ok, %{choices: choices}} = result

        choices
        |> List.wrap()
        |> Enum.map(&normalize_choice/1)
        |> Enum.reject(&is_nil/1)

      match?({:error, _}, result) ->
        []

      true ->
        []
    end
  end

  defp normalize_cover_list(list) when is_list(list) do
    list
    |> Enum.map(&normalize_choice/1)
    |> Enum.reject(&is_nil/1)
  end

  defp normalize_cover_list(_), do: []

  defp normalize_choice(%{id: id} = c) when is_binary(id) do
    %{
      id: id,
      span: Map.get(c, :span) || Map.get(c, "span") || {0, 0},
      token_index: Map.get(c, :token_index) || Map.get(c, "token_index") || 0,
      margin: Map.get(c, :margin) || Map.get(c, "margin") || 0.0
    }
  end

  defp normalize_choice(%{"id" => id} = c) when is_binary(id) do
    %{
      id: id,
      span: Map.get(c, :span) || Map.get(c, "span") || {0, 0},
      token_index: Map.get(c, :token_index) || Map.get(c, "token_index") || 0,
      margin: Map.get(c, :margin) || Map.get(c, "margin") || 0.0
    }
  end

  defp normalize_choice(_), do: nil
end
