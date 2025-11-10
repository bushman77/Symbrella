defmodule Brain.LIFG do
  @moduledoc """
  Left Inferior Frontal Gyrus (LIFG) — Competitive Sense Selection.

  Entry points:
    • Legacy-compatible API (pure): `disambiguate_stage1/1,2`
      - Calls the new `Brain.LIFG.Stage1.run/2` engine and preserves the legacy event shape.
    • Full pipeline: `run/2` (ATL finalize → Stage-1 → optional pMTG, optional ACC gate → Post.finalize).

  Optional ACC hook:
    If `Brain.ACC` is available, `run/2` will compute a conflict score
    and only apply pMTG if `conflict >= acc_conflict_tau`. When ACC is absent,
    behavior is unchanged (pMTG applies according to opts).

  Central config (`config/config.exs`):

      config :brain,
        lifg_stage1_weights: %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10},
        lifg_stage1_scores_mode: :all,  # or :top2 | :none
        lifg_min_margin: 0.05,
        lifg_stage1_mwe_fallback: true,
        pmtg_mode: :boost,
        pmtg_margin_threshold: 0.15,
        pmtg_window_keep: 50,
        acc_conflict_tau: 0.50
  """

  use Brain, region: :lifg
  require Logger

  alias Brain.Utils.Safe
  alias Brain.LIFG.Input
  alias Brain.LIFG.Post

  # Slim helpers (lived in separate files)
  alias Brain.LIFG.{
    MWE,
    Choices,          # ensure apps/brain/lib/brain/lifg/choices.ex is present
    Legacy,           # ensure apps/brain/lib/brain/lifg/legacy.ex is present
    Stage1Bridge,
    ACCGate,
    FallbackRerun,
    Recorder,
    Config
  }

  # ---- Stable registered name / lifecycle helpers ---------------------------

  @doc "Stable registered name used for calls/whereis (pairs with UI call_target/1)."
  def name, do: __MODULE__

  @doc "Best-effort bring-up for dev/hotreload; safe if already running."
  def ensure_started do
    case GenServer.whereis(name()) do
      nil ->
        case start_link(%{}) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _}} -> :ok
          _ -> :ok
        end

      _pid ->
        :ok
    end
  rescue
    _ -> :ok
  end

  @doc "Return a minimal snapshot of the server state if running; else %{}."
  def get_state(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil -> %{}
      _ ->
        try do
          GenServer.call(server, :get_state)
        catch
          :exit, _ -> %{}
        end
    end
  end

  def start_link(opts) when is_list(opts) or is_map(opts) do
    GenServer.start_link(__MODULE__, Map.new(opts), name: name())
  end

  # ── Server bootstrap & runtime config ────────────────────────────────

  @impl true
  def init(opts) do
    eff = opts |> Map.new() |> Config.effective_opts()
    {:ok, %{region: :lifg, opts: eff, last: nil}}
  end

  @doc "Returns effective options (env + overrides), even if the server hasn't started."
  def status(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil -> Config.effective_opts(%{})
      _pid ->
        try do
          GenServer.call(server, :status)
        catch
          :exit, _ -> Config.effective_opts(%{})
        end
    end
  end

  @doc """
  Returns the most recent LIFG decision snapshot recorded by this server, or :empty.
  """
  @spec last(module()) :: map() | :empty
  def last(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil -> :empty
      _pid ->
        try do
          GenServer.call(server, :last)
        catch
          :exit, _ -> :empty
        end
    end
  end

  def reload_config(new_opts, server \\ __MODULE__) when is_list(new_opts) or is_map(new_opts) do
    GenServer.cast(server, {:reload_config, Map.new(new_opts)})
  end

  @impl true
  def handle_call(:status, _from, state) do
    eff = Config.effective_opts(state.opts || %{})
    {:reply, eff, %{state | opts: eff}}
  end

  @impl true
  def handle_call(:last, _from, state), do: {:reply, state.last || :empty, state}

  @impl true
  def handle_call(:get_state, _from, state),
    do: {:reply, %{region: :lifg, opts: state.opts, last: state.last}, state}

  @impl true
  def handle_cast({:reload_config, new_opts}, state) do
    merged = Map.merge(state.opts || %{}, Map.new(new_opts))
    {:noreply, %{state | opts: Config.effective_opts(merged)}}
  end

  @impl true
  def handle_cast({:record_last, payload}, state) when is_map(payload) do
    {:noreply, %{state | last: payload}}
  end

  # ── Stage-1 (legacy-compatible event shape) ──────────────────────────

  @doc """
  Pure, stateless Stage-1 wrapper over `Brain.LIFG.Stage1.run/2`.
  Preserves legacy event shape with `:choices/:boosts/:inhibitions`.
  """
  @spec disambiguate_stage1(map()) :: map()
  def disambiguate_stage1(%{} = si), do: disambiguate_stage1(si, [])

  @spec disambiguate_stage1(map(), keyword()) :: map()
  def disambiguate_stage1(%{} = si, opts) do
    si_plain = Safe.to_plain(si)
    slate    = Input.slate_for(si_plain)

    si_for_stage =
      si_plain
      |> Map.put(:sense_candidates, slate)
      |> Map.delete(:candidates_by_token)
      |> Map.put_new(:trace, [])
      |> MWE.ensure_mwe_candidates(opts)
      |> MWE.absorb_unigrams_into_mwe(opts)
      |> MWE.backfill_unigrams_from_active_cells(opts)

    eff_opts =
      si_for_stage
      |> Map.get(:lifg_opts, [])
      |> flatten_lifg_opts()
      |> Keyword.merge(flatten_lifg_opts(opts))

    scores_mode =
      Keyword.get(eff_opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

    margin_thr = Keyword.get(eff_opts, :margin_threshold, 0.15)

    min_margin =
      Keyword.get(eff_opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.05))

    stage_weights =
      Config.lifg_weights()
      |> Map.merge(Map.new(Keyword.get(eff_opts, :weights, [])))

    res =
      Stage1Bridge.safe_call(
        si_for_stage,
        [
          weights: stage_weights,
          scores: scores_mode,
          margin_threshold: margin_thr,
          chargram_event: [:brain, :lifg, :chargram_violation],
          boundary_event: [:brain, :lifg, :boundary_drop]
        ],
        eff_opts
      )

    case Stage1Bridge.normalize_result(res) do
      {:ok, si_after, raw_choices, audit} ->
        choices =
          raw_choices
          |> Enum.map(&Safe.to_plain/1)
          |> Choices.augment(si_after, min_margin)

        {boosts_out, inhibitions_out} =
          Legacy.boosts_inhibitions(choices, margin_thr, scores_mode, eff_opts)

        _ =
          Recorder.maybe_record_last(:stage1, si_after, choices, audit, %{
            scores_mode: scores_mode,
            margin_threshold: margin_thr,
            weights: stage_weights
          })

        evt = %{
          stage: :lifg_stage1,
          choices: choices,
          boosts: boosts_out,
          inhibitions: inhibitions_out,
          opts: Enum.into(eff_opts, %{})
        }

        Map.update(si_after, :trace, [evt], fn tr -> [evt | tr] end)

      {:error, reason} ->
        Logger.error("LIFG Stage1 run failed: #{inspect(reason)}")
        _ = Recorder.maybe_record_last(:stage1_error, si_for_stage, [], %{}, %{error: inspect(reason)})

        evt = %{
          stage: :lifg_stage1,
          choices: [],
          boosts: [],
          inhibitions: [],
          opts: Enum.into(Keyword.put(eff_opts, :error, inspect(reason)), %{})
        }

        si_for_stage
        |> Map.update(:trace, [evt], fn tr -> [evt | tr] end)
        |> Map.put(:lifg_error, inspect(reason))
    end
  end

  # ── Full pipeline (optional ATL + ACC + pMTG) ────────────────────────

  @doc """
  Full LIFG pipeline:
    1) ATL finalize → slate (if available)
    2) Attach si.sense_candidates
    3) Stage-1 disambiguation (probabilities & margins)
    4) Fallback-rerun if winners are `|phrase|fallback` and WM disallows fallbacks
    5) ACC assess (optional; computes :conflict and appends trace)
    6) pMTG consult (sync rerun or async boost/none), gated by ACC when available
    7) Post.finalize: non-overlap cover and optional reanalysis
  """
  @spec run(map(), keyword()) ::
          {:ok, %{si: map(), choices: list(), slate: map(), cover: list(), flips: non_neg_integer()}}
          | {:error, term()}
  def run(si, opts \\ []) when is_map(si) and is_list(opts) do
    try do
      # 1) ATL finalize (optional)
      {si1, slate} =
        if Code.ensure_loaded?(Brain.ATL) and function_exported?(Brain.ATL, :finalize, 2) do
          case Brain.ATL.finalize(si, opts) do
            {s, sl} when is_map(s) and is_map(sl) -> {s, sl}
            _ -> {si, %{}}
          end
        else
          {si, %{}}
        end

      # 2) Attach slate → sense_candidates (optional)
      si2 =
        if Code.ensure_loaded?(Brain.ATL) and function_exported?(Brain.ATL, :attach_sense_candidates, 3) do
          case Brain.ATL.attach_sense_candidates(
                 si1,
                 slate,
                 top_k: Keyword.get(opts, :top_k, 3),
                 margin_window: Keyword.get(opts, :margin_window, 0.05)
               ) do
            %{} = s -> s
            _ -> si1
          end
        else
          si1
        end

      # 2b) Ensure MWE candidates exist for MWE tokens that lack them
      si2a = MWE.ensure_mwe_candidates(si2, opts)

      scores_mode =
        Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

      margin_thr = Keyword.get(opts, :margin_threshold, 0.15)

      min_margin =
        Keyword.get(opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.05))

     

weights_for_stage1 =
  Keyword.get(opts, :weights, %{
    lex_fit: 0.5,
    rel_prior: 0.25,
    activation: 0.15,
    intent_bias: 0.10
  })
 
      # 3) Stage-1 disambiguation
      stage1_result =
        Stage1Bridge.safe_call(
          si2a,
          [
            weights: weights_for_stage1,
            scores: scores_mode,
            margin_threshold: margin_thr,
            chargram_event: [:brain, :lifg, :chargram_violation],
            boundary_event: [:brain, :lifg, :boundary_drop]
          ],
          opts
        )

      case Stage1Bridge.normalize_result(stage1_result) do
        {:ok, si3, raw_choices, audit} ->
          choices0 =
            raw_choices
            |> Enum.map(&Safe.to_plain/1)
            |> Choices.augment(si3, min_margin)

          # 4) Fallback-rerun (P-201)
          {si3a, choices} =
            FallbackRerun.maybe_rerun(
              si3,
              choices0,
              weights_for_stage1,
              scores_mode,
              margin_thr,
              opts
            )

          # 5) ACC (optional). If present, record conflict & gate pMTG by threshold.
          {si3b, acc_conflict, acc_present?} = ACCGate.assess(si3a, choices, opts)

          acc_conf_tau =
            Keyword.get(
              opts,
              :acc_conflict_tau,
              Application.get_env(:brain, :acc_conflict_tau, 0.50)
            )

          # 6) pMTG consult (respect ACC gate when ACC is present)
          pmtg_mode = Keyword.get(opts, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost))

          pmtg_apply? =
            Keyword.get(opts, :pmtg_apply?, true) and
              (not acc_present? or acc_conflict >= acc_conf_tau)

          {final_si, final_choices} =
            if Code.ensure_loaded?(Brain.PMTG) and pmtg_apply? do
              case pmtg_mode do
                :rerun ->
                  case Brain.PMTG.consult_sync(
                         choices,
                         Safe.get(si3b, :tokens, []),
                         already_needy: false,
                         margin_threshold: margin_thr,
                         limit: Keyword.get(opts, :limit, 5),
                         mode: :rerun,
                         rerun_only_if_hits: Keyword.get(opts, :rerun_only_if_hits, true),
                         rerun_weights_bump:
                           Keyword.get(opts, :rerun_weights_bump, %{
                             lex_fit: 0.05,
                             rel_prior: 0.05
                           })
                       ) do
                    {:ok, %{si: si_after, choices: merged}} -> {si_after, merged}
                    _ -> {si3b, choices}
                  end

                _other ->
                  _ =
                    Brain.PMTG.consult(
                      choices,
                      Safe.get(si3b, :tokens, []),
                      margin_threshold: margin_thr,
                      limit: Keyword.get(opts, :limit, 5),
                      mode: pmtg_mode
                    )

                  {si3b, choices}
              end
            else
              {si3b, choices}
            end

          # 7) Post.finalize — non-overlap cover + optional reanalysis
          post_out =
            Post.finalize(
              final_si,
              final_choices,
              reanalysis?: Keyword.get(opts, :reanalysis?, false),
              fail_fun: Keyword.get(opts, :fail_fun, fn _ -> false end),
              allow_overlaps?: Keyword.get(opts, :allow_overlaps?, false)
            )

          Recorder.maybe_record_last(:run, post_out.si, post_out.choices, audit, %{
            scores_mode: scores_mode,
            margin_threshold: margin_thr,
            pmtg_mode: pmtg_mode,
            cover_count: length(post_out.cover),
            flips: post_out.flips
          })

          {:ok,
           %{
             si: post_out.si,
             choices: post_out.choices,
             slate: slate,
             cover: post_out.cover,
             flips: post_out.flips
           }}

        {:error, reason} ->
          Recorder.maybe_record_last(:run_error, si2a, [], %{}, %{error: inspect(reason)})
          {:error, {:stage1, reason}}
      end
    rescue
      e ->
        Logger.error("LIFG full run failed: #{inspect(e)}")
        Recorder.maybe_record_last(:run_exception, si, [], %{}, %{error: inspect(e)})
        {:error, e}
    end
  end

  # ── Math helpers (public, used elsewhere) ────────────────────────────

  @doc "Stable softmax. Uniform if inputs are all equal. Sums to 1.0."
  @spec normalize_scores([number()]) :: [float()]
  def normalize_scores([]), do: []

  def normalize_scores(xs) when is_list(xs) do
    m = Enum.max(xs, fn -> 0.0 end)
    exs = Enum.map(xs, fn x -> :math.exp(x * 1.0 - m) end)
    z = Enum.sum(exs)
    if z <= 0.0 do
      n = length(xs)
      if n == 0, do: [], else: List.duplicate(1.0 / n, n)
    else
      Enum.map(exs, &(&1 / z))
    end
  end

  @doc "Cosine similarity (nil/zero-safe). Returns 0.0 if either vector has ~0 norm."
  @spec cosine([number()] | nil, [number()] | nil) :: float()
  def cosine(a, b) when is_list(a) and is_list(b) do
    {sa, sb} =
      {Enum.reduce(a, 0.0, fn x, acc -> acc + x * x end),
       Enum.reduce(b, 0.0, fn x, acc -> acc + x * x end)}
    na = :math.sqrt(Kernel.max(sa, 0.0))
    nb = :math.sqrt(Kernel.max(sb, 0.0))
    if na <= 1.0e-15 or nb <= 1.0e-15, do: 0.0,
      else: (Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)) / (na * nb)
  end

  def cosine(_, _), do: 0.0

  @doc "Return p(top1) from a LIFG choice (max over normalized scores)."
  def top1_prob(choice) when is_map(choice) do
    choice |> Safe.get(:scores, %{}) |> Map.values() |> Enum.max(fn -> 0.0 end)
  end

  def top1_prob(_), do: 0.0

  @doc """
  Low-confidence predicate used by pMTG gate.
  Defaults: tau_confident=0.20, p_min=0.65. Returns true when pMTG should fire.
  """
  def low_confidence?(choice, opts) when is_map(choice) do
    tau = Keyword.get(opts, :tau_confident, 0.20)
    p_min = Keyword.get(opts, :p_min, 0.65)
    m = Safe.get(choice, :margin, 0.0) || 0.0
    p1 = top1_prob(choice)
    alts? = (Safe.get(choice, :alt_ids, []) || []) != []
    m < tau or p1 < p_min or alts?
  end

  def low_confidence?(_choice, _opts), do: true

  # ── Small internal helpers ───────────────────────────────────────────

  # Flatten :lifg_opts embedded as kw/map into a keyword list
  defp flatten_lifg_opts(nil), do: []
  defp flatten_lifg_opts(%{} = m), do: Map.to_list(m)
  defp flatten_lifg_opts(kw) when is_list(kw), do: kw
end

