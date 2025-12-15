# apps/brain/lib/brain/lifg.ex
defmodule Brain.LIFG do
  @moduledoc """
  Left Inferior Frontal Gyrus (LIFG) — Competitive Sense Selection.

  Entry points:
    • Legacy-compatible API (pure): `disambiguate_stage1/1,2`
      - Calls the new `Brain.LIFG.Stage1.run/2` engine and preserves the legacy event shape.
    • Full pipeline: `run/2`
      (ATL finalize → Stage-1 → optional fallback-rerun → optional ACC gate → optional pMTG → Post.finalize)

  Optional ACC hook:
    If `Brain.ACC` is available, `run/2` will compute a conflict score
    and only apply pMTG if `conflict >= acc_conflict_tau`. When ACC is absent,
    behavior is unchanged (pMTG applies according to opts).

  Assistant identity (Option B):
    If a token matches `Brain.Config.assistant/0` (name/norm/aliases), LIFG injects a
    system-entity candidate into `si.sense_candidates[token_index]` so the assistant
    name is handled as an internal entity (not a dictionary lookup).
  """

  use Brain, region: :lifg
  require Logger

  alias Brain.Utils.Safe
  alias Brain.Config, as: BrainConfig
  alias Brain.LIFG.{Explanation, Post}

  alias Brain.LIFG.{
    ACCGate,
    Choices,
    Config,
    FallbackRerun,
    Legacy,
    MWE,
    Recorder,
    Stage1Bridge
  }

  # ── Server façade ──────────────────────────────────────────────────────────

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
      nil ->
        %{}

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

  @impl true
  def init(opts) do
    eff = opts |> Map.new() |> Config.effective_opts()
    {:ok, %{region: :lifg, opts: eff, last: nil}}
  end

  @doc "Returns effective options (env + overrides), even if the server hasn't started."
  def status(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil ->
        Config.effective_opts(%{})

      _pid ->
        try do
          GenServer.call(server, :status)
        catch
          :exit, _ -> Config.effective_opts(%{})
        end
    end
  end

  @doc "Returns the most recent LIFG decision snapshot recorded by this server, or :empty."
  @spec last(module()) :: map() | :empty
  def last(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil ->
        :empty

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
    enriched = attach_explanation(payload)
    {:noreply, %{state | last: enriched}}
  end

  # ── Stage-1 (legacy-compatible event shape) ────────────────────────────────

  @doc """
  Pure, stateless Stage-1 wrapper over `Brain.LIFG.Stage1.run/2`.
  Preserves legacy event shape with `:choices/:boosts/:inhibitions`.
  """
  @spec disambiguate_stage1(map()) :: map()
  def disambiguate_stage1(%{} = si), do: disambiguate_stage1(si, [])

  @spec disambiguate_stage1(map(), keyword()) :: map()
  def disambiguate_stage1(%{} = si, opts) when is_list(opts) do
    si_plain = Safe.to_plain(si)

    # Base slate from caller shapes (incl. active_cells)
    slate0 =
      si_plain
      |> slate_from_si()
      |> ensure_map()

    si_for_stage0 =
      si_plain
      |> Map.put(:sense_candidates, slate0)
      |> Map.delete(:candidates_by_token)
      |> Map.put_new(:trace, [])

    si_for_stage1 =
      si_for_stage0
      |> ensure_assistant_candidates()
      |> MWE.ensure_mwe_candidates(opts)
      |> MWE.absorb_unigrams_into_mwe(opts)
      |> MWE.backfill_unigrams_from_active_cells(opts)

    # Ensure MWE helpers cannot wipe out the base per-token competition
    slate_final =
      si_for_stage1
      |> Map.get(:sense_candidates)
      |> ensure_map()
      |> merge_slate_maps(slate0)

    si_for_stage = Map.put(si_for_stage1, :sense_candidates, slate_final)

    eff_opts = stage1_effective_opts(si_for_stage, opts)
    cfg = stage1_config(eff_opts)

    case run_stage1(si_for_stage, cfg, eff_opts) do
      {:ok, si_after0, raw_choices, audit} ->
        # Reattach merged slate we know contains per-token competitors
        si_after =
          si_after0
          |> Safe.to_plain()
          |> Map.put(:sense_candidates, slate_final)

        choices =
          raw_choices
          |> Enum.map(&Safe.to_plain/1)
          |> normalize_choice_token_indices(si_after)
          |> maybe_backfill_probs_and_margin(cfg.scores_mode, si_after, cfg.stage_weights, eff_opts)
          |> Choices.augment(si_after, cfg.min_margin)
          |> normalize_choice_token_indices(si_after)
          |> attach_slate_alt_ids(si_after)

        {boosts_out, inhibitions_out} =
          control_signals(choices, si_after, cfg.scores_mode, cfg.margin_thr, eff_opts)

        _ =
          Recorder.maybe_record_last(__MODULE__, :stage1, si_after, choices, audit, %{
            scores_mode: cfg.scores_mode,
            margin_threshold: cfg.margin_thr,
            weights: cfg.stage_weights
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

        _ =
          Recorder.maybe_record_last(__MODULE__, :stage1_error, si_for_stage, [], %{}, %{
            error: inspect(reason)
          })

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

  # ── Full pipeline (optional ATL + ACC + pMTG) ──────────────────────────────

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
      {si1, slate} =
        if Code.ensure_loaded?(Brain.ATL) and function_exported?(Brain.ATL, :finalize, 2) do
          case Brain.ATL.finalize(si, opts) do
            {s, sl} when is_map(s) and is_map(sl) -> {s, sl}
            _ -> {si, %{}}
          end
        else
          {si, %{}}
        end

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

      si2 =
        si2
        |> Map.put(:atl_slate, slate)
        |> ensure_assistant_candidates()
        |> MWE.ensure_mwe_candidates(opts)
        |> MWE.absorb_unigrams_into_mwe(opts)
        |> MWE.backfill_unigrams_from_active_cells(opts)

      eff_opts = stage1_effective_opts(si2, opts)
      cfg = stage1_config(eff_opts)

      weights_for_stage1 = cfg.stage_weights

      stage1_result =
        run_stage1(si2, cfg, eff_opts)

      case stage1_result do
        {:ok, si3a0, raw_choices, audit} ->
          # Keep slate if Stage1 drops it
          si3a = reattach_sense_candidates(si3a0, si2)

          choices0 =
            raw_choices
            |> Enum.map(&Safe.to_plain/1)
            |> normalize_choice_token_indices(si3a)
            |> maybe_backfill_probs_and_margin(cfg.scores_mode, si3a, weights_for_stage1, eff_opts)
            |> Choices.augment(si3a, cfg.min_margin)
            |> normalize_choice_token_indices(si3a)
            |> attach_slate_alt_ids(si3a)

          {si3b, choices} =
            FallbackRerun.maybe_rerun(
              si3a,
              choices0,
              weights_for_stage1,
              cfg.scores_mode,
              cfg.margin_thr,
              eff_opts
            )

          {si3c, acc_conflict, acc_present?} = ACCGate.assess(si3b, choices, eff_opts)

          acc_conf_tau =
            Keyword.get(eff_opts, :acc_conflict_tau, Application.get_env(:brain, :acc_conflict_tau, 0.50))

          needy =
            Enum.filter(choices, fn ch ->
              m = (ch[:margin] || 1.0) * 1.0
              alts? = (ch[:alt_ids] || []) != []
              p1 = top1_prob(ch)

              (m < cfg.margin_thr or p1 < Application.get_env(:brain, :acc_p_min, 0.65)) and alts?
            end)

          pmtg_mode = Keyword.get(eff_opts, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost))

          pmtg_apply? =
            Keyword.get(eff_opts, :pmtg_apply?, true) and (not acc_present? or acc_conflict >= acc_conf_tau)

          :telemetry.execute(
            [:brain, :lifg, :pmtg_decision],
            %{needy: length(needy)},
            %{
              apply?: pmtg_apply?,
              mode: pmtg_mode,
              acc_present?: acc_present?,
              acc_conflict: acc_conflict * 1.0,
              acc_tau: acc_conf_tau * 1.0
            }
          )

          {final_si, final_choices} =
            if Code.ensure_loaded?(Brain.PMTG) and pmtg_apply? do
              case pmtg_mode do
                :rerun ->
                  case Brain.PMTG.consult_sync(
                         choices,
                         Safe.get(si3c, :tokens, []),
                         already_needy: false,
                         margin_threshold: cfg.margin_thr,
                         limit: Keyword.get(eff_opts, :limit, 5),
                         mode: :rerun,
                         rerun_only_if_hits: Keyword.get(eff_opts, :rerun_only_if_hits, true),
                         rerun_weights_bump:
                           Keyword.get(eff_opts, :rerun_weights_bump, %{lex_fit: 0.05, rel_prior: 0.05})
                       ) do
                    {:ok, %{si: si_after, choices: merged}} -> {si_after, merged}
                    _ -> {si3c, choices}
                  end

                _other ->
                  _ =
                    Brain.PMTG.consult(
                      choices,
                      Safe.get(si3c, :tokens, []),
                      margin_threshold: cfg.margin_thr,
                      limit: Keyword.get(eff_opts, :limit, 5),
                      mode: pmtg_mode
                    )

                  {si3c, choices}
              end
            else
              {si3c, choices}
            end

          post_out =
            Post.finalize(
              final_si,
              final_choices,
              reanalysis?: Keyword.get(eff_opts, :reanalysis?, false),
              fail_fun: Keyword.get(eff_opts, :fail_fun, fn _ -> false end),
              allow_overlaps?: Keyword.get(eff_opts, :allow_overlaps?, false)
            )

          _ =
            Recorder.maybe_record_last(__MODULE__, :run, post_out.si, post_out.choices, audit, %{
              scores_mode: cfg.scores_mode,
              margin_threshold: cfg.margin_thr,
              pmtg_mode: pmtg_mode,
              pmtg_applied?: pmtg_apply?,
              acc_present?: acc_present?,
              acc_conflict: acc_conflict,
              acc_conflict_tau: acc_conf_tau,
              needy_count: length(needy),
              cover_count: length(post_out.cover),
              flips: post_out.flips
            })

          {:ok,
           %{si: post_out.si, choices: post_out.choices, slate: slate, cover: post_out.cover, flips: post_out.flips}}

        {:error, reason} ->
          _ =
            Recorder.maybe_record_last(__MODULE__, :run_error, si2, [], %{}, %{
              error: inspect(reason)
            })

          {:error, {:stage1, reason}}
      end
    rescue
      e ->
        Logger.error("LIFG full run failed: #{inspect(e)}")
        _ = Recorder.maybe_record_last(__MODULE__, :run_exception, si, [], %{}, %{error: inspect(e)})
        {:error, e}
    end
  end

  # ── Math helpers (public, used elsewhere) ──────────────────────────────────

  @doc "Stable softmax. Uniform if inputs are all equal. Sums to 1.0."
  @spec normalize_scores([number()]) :: [float()]
  def normalize_scores([]), do: []

  def normalize_scores(xs) when is_list(xs) do
    m = Enum.max(xs, fn -> 0.0 end)
    exs = Enum.map(xs, fn x -> :math.exp(x * 1.0 - m) end)
    z = Enum.sum(exs)

    cond do
      z <= 0.0 ->
        n = length(xs)
        if n == 0, do: [], else: List.duplicate(1.0 / n, n)

      true ->
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

    if na <= 1.0e-15 or nb <= 1.0e-15 do
      0.0
    else
      (Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)) / (na * nb)
    end
  end

  def cosine(_, _), do: 0.0

  @doc "Return p(top1) from a LIFG choice (max over normalized scores or probs)."
  @spec top1_prob(map()) :: float()
  def top1_prob(choice) when is_map(choice) do
    m0 = Safe.get(choice, :scores, %{}) || %{}
    m = if is_map(m0) and map_size(m0) > 0, do: m0, else: (Safe.get(choice, :probs, %{}) || %{})
    m |> Map.values() |> Enum.max(fn -> 0.0 end)
  end

  def top1_prob(_), do: 0.0

  @doc """
  Low-confidence predicate used by pMTG gate.
  Defaults: tau_confident=0.20, p_min=0.65. Returns true when pMTG should fire.
  """
  @spec low_confidence?(map(), keyword()) :: boolean()
  def low_confidence?(choice, opts) when is_map(choice) do
    tau = Keyword.get(opts, :tau_confident, 0.20)
    p_min = Keyword.get(opts, :p_min, 0.65)

    m = (Safe.get(choice, :margin, 0.0) || 0.0) * 1.0
    p1 = top1_prob(choice)
    alts? = (Safe.get(choice, :alt_ids, []) || []) != []

    alts? and (m < tau or p1 < p_min)
  end

  def low_confidence?(_choice, _opts), do: true

  # ── Stage-1 core execution helpers ─────────────────────────────────────────

  defp stage1_effective_opts(si_for_stage, opts) do
    si_for_stage
    |> Map.get(:lifg_opts, [])
    |> flatten_lifg_opts()
    |> Keyword.merge(flatten_lifg_opts(opts))
  end

  defp stage1_config(eff_opts) do
    scores_mode =
      Keyword.get(
        eff_opts,
        :scores,
        Application.get_env(:brain, :lifg_stage1_scores_mode, :all)
      )

    margin_thr = Keyword.get(eff_opts, :margin_threshold, 0.15)

    min_margin =
      Keyword.get(
        eff_opts,
        :min_margin,
        Application.get_env(:brain, :lifg_min_margin, 0.05)
      )

    stage_weights =
      Config.lifg_weights()
      |> Map.merge(Map.new(Keyword.get(eff_opts, :weights, [])))

    %{
      scores_mode: scores_mode,
      margin_thr: margin_thr,
      min_margin: min_margin,
      stage_weights: stage_weights
    }
  end

  defp stage1_bridge_opts(scores_mode, stage_weights, margin_thr) do
    [
      weights: stage_weights,
      scores: scores_mode,
      margin_threshold: margin_thr,
      chargram_event: [:brain, :lifg, :chargram_violation],
      boundary_event: [:brain, :lifg, :boundary_drop]
    ]
  end

  defp run_stage1(si_for_stage, cfg, eff_opts) when is_map(si_for_stage) and is_map(cfg) and is_list(eff_opts) do
    res =
      Stage1Bridge.safe_call(
        si_for_stage,
        stage1_bridge_opts(cfg.scores_mode, cfg.stage_weights, cfg.margin_thr),
        eff_opts
      )

    case Stage1Bridge.normalize_result(res) do
      {:ok, si_after, raw_choices, audit} ->
        {:ok, si_after, raw_choices, audit}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Reattach a non-empty slate if Stage1 drops :sense_candidates.
  defp reattach_sense_candidates(si_after0, si_for_stage)
       when is_map(si_after0) and is_map(si_for_stage) do
    si_after = Safe.to_plain(si_after0)
    slate = slate_from_si(si_for_stage)

    cond do
      is_map(Map.get(si_after, :sense_candidates)) and map_size(Map.get(si_after, :sense_candidates)) > 0 ->
        si_after

      is_map(slate) and map_size(slate) > 0 ->
        Map.put(si_after, :sense_candidates, slate)

      true ->
        si_after
    end
  end

  # ── Choice normalization & slate recovery ──────────────────────────────────

  defp ensure_map(m) when is_map(m), do: m
  defp ensure_map(_), do: %{}

  defp merge_slate_maps(a, b) do
    a1 = if is_map(a), do: normalize_slate_map(a), else: %{}
    b1 = if is_map(b), do: normalize_slate_map(b), else: %{}

    Map.merge(a1, b1, fn _idx, va, vb ->
      (List.wrap(va) ++ List.wrap(vb))
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.uniq_by(fn c ->
        normalize_signal_id(
          c[:id] || c["id"] || c[:chosen_id] || c["chosen_id"] || c[:lemma] || c["lemma"]
        )
      end)
    end)
  end

  # Ensures every Stage-1 choice has a stable integer :token_index.
  defp normalize_choice_token_indices(choices, si) when is_list(choices) and is_map(si) do
    slate = slate_from_si(si)

    Enum.map(choices, fn
      ch when is_map(ch) ->
        chp = Safe.to_plain(ch)

        idx =
          choice_index_from_choice(chp) ||
            token_index_from_choice_token(Map.get(chp, :token) || Map.get(chp, "token")) ||
            token_index_from_slate(slate, choice_winner_id(chp))

        Map.put(chp, :token_index, if(is_integer(idx) and idx >= 0, do: idx, else: 0))

      other ->
        other
    end)
  end

  defp normalize_choice_token_indices(other, _si), do: other

  defp choice_index_from_choice(chp) when is_map(chp) do
    parse_nonneg_int(
      Map.get(chp, :token_index) || Map.get(chp, "token_index") ||
        Map.get(chp, :index) || Map.get(chp, "index")
    )
  end

  defp choice_index_from_choice(_), do: nil

  defp token_index_from_choice_token(tok) when is_map(tok) do
    parse_nonneg_int(
      Map.get(tok, :index) || Map.get(tok, "index") ||
        Map.get(tok, :token_index) || Map.get(tok, "token_index")
    )
  end

  defp token_index_from_choice_token(_), do: nil

  defp token_index_from_slate(slate, wid0) when is_map(slate) do
    wid = normalize_signal_id(wid0)

    if is_binary(wid) do
      Enum.find_value(slate, fn {k, senses} ->
        idx = parse_nonneg_int(k)

        if is_integer(idx) do
          hit? =
            senses
            |> List.wrap()
            |> Enum.any?(fn s ->
              c = Safe.to_plain(s)

              cid =
                normalize_signal_id(
                  c[:id] || c["id"] || c[:chosen_id] || c["chosen_id"] || c[:lemma] || c["lemma"]
                )

              cid == wid
            end)

          if hit?, do: idx, else: nil
        else
          nil
        end
      end)
    else
      nil
    end
  end

  defp token_index_from_slate(_, _), do: nil

  defp parse_nonneg_int(i) when is_integer(i) and i >= 0, do: i

  defp parse_nonneg_int(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> nil
    end
  end

  defp parse_nonneg_int(_), do: nil

  # ── Slate extraction (Stage-1 input contract) ──────────────────────────────

  defp slate_from_si(si) when is_map(si) do
    cond do
      is_map(m = Map.get(si, :sense_candidates)) -> m
      is_map(m = Map.get(si, "sense_candidates")) -> m

      is_map(m = Map.get(si, :atl_slate)) -> m
      is_map(m = Map.get(si, "atl_slate")) -> m

      is_map(m = Map.get(si, :candidates_by_token)) -> normalize_slate_map(m)
      is_map(m = Map.get(si, "candidates_by_token")) -> normalize_slate_map(m)

      is_list(l = Map.get(si, :candidates)) -> group_flat_candidates(l)
      is_list(l = Map.get(si, "candidates")) -> group_flat_candidates(l)

      is_list(l = Map.get(si, :cands)) -> group_flat_candidates(l)
      is_list(l = Map.get(si, "cands")) -> group_flat_candidates(l)

      # SemanticInput path
      is_list(l = Map.get(si, :active_cells)) -> group_flat_candidates(l)
      is_list(l = Map.get(si, "active_cells")) -> group_flat_candidates(l)

      true ->
        %{}
    end
  end

  defp slate_from_si(_), do: %{}

  defp normalize_slate_map(m) when is_map(m) do
    Enum.reduce(m, %{}, fn {k, v}, acc ->
      idx = parse_nonneg_int(k) || 0

      bucket =
        v
        |> List.wrap()
        |> Enum.map(&normalize_candidate/1)
        |> Enum.reject(&is_nil/1)

      Map.put(acc, idx, bucket)
    end)
  end

  defp normalize_slate_map(_), do: %{}

  defp group_flat_candidates(list) when is_list(list) do
    list
    |> Enum.map(&normalize_candidate/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.group_by(fn c -> Map.get(c, :token_index, 0) end)
  end

  defp group_flat_candidates(_), do: %{}

# apps/brain/lib/brain/lifg.ex
# Replace your current normalize_candidate/1 with this version.

defp normalize_candidate(c) when is_map(c) do
  c0 = Safe.to_plain(c)
  p0 = Safe.to_plain(Map.get(c0, :payload) || Map.get(c0, "payload") || %{})

  idx =
    parse_nonneg_int(
      Map.get(c0, :token_index) || Map.get(c0, "token_index") ||
        Map.get(c0, :index) || Map.get(c0, "index") ||
        Map.get(p0, :token_index) || Map.get(p0, "token_index") ||
        Map.get(p0, :index) || Map.get(p0, "index")
    ) || 0

  id =
    Map.get(c0, :id) || Map.get(c0, "id") ||
      Map.get(p0, :id) || Map.get(p0, "id") ||
      Map.get(c0, :chosen_id) || Map.get(c0, "chosen_id") ||
      Map.get(p0, :chosen_id) || Map.get(p0, "chosen_id") ||
      Map.get(c0, :lemma) || Map.get(c0, "lemma") ||
      Map.get(p0, :lemma) || Map.get(p0, "lemma")

  feats0 =
    Map.get(c0, :features) || Map.get(c0, "features") ||
      Map.get(p0, :features) || Map.get(p0, "features")

  feats =
    cond do
      is_map(feats0) and map_size(feats0) > 0 ->
        feats0

      true ->
        # Preserve the common Stage1 feature keys even when callers don't provide :features.
        # This prevents “empty features map” issues during scores=:none backfill.
        Map.merge(
          Map.take(c0, [
            :lex_fit, :rel_prior, :activation, :intent_bias, :embedding,
            :pos, :lemma, :norm, :score, :rank
          ]),
          Map.take(p0, [
            :lex_fit, :rel_prior, :activation, :intent_bias, :embedding,
            :pos, :lemma, :norm, :score, :rank
          ])
        )
    end

  c0
  |> Map.put(:token_index, idx)
  |> Map.put(:id, if(is_nil(id), do: nil, else: to_string(id)))
  |> Map.put(:features, feats)
end

defp normalize_candidate(_), do: nil

  # ── Winner extraction & competition detection ──────────────────────────────

  defp choice_winner_id(ch) when is_map(ch) do
    raw =
      Map.get(ch, :chosen_id) || Map.get(ch, "chosen_id") ||
        Map.get(ch, :chosen) || Map.get(ch, "chosen") ||
        Map.get(ch, :winner) || Map.get(ch, "winner") ||
        Map.get(ch, :winner_id) || Map.get(ch, "winner_id") ||
        Map.get(ch, :best_id) || Map.get(ch, "best_id") ||
        Map.get(ch, :id) || Map.get(ch, "id")

    case raw do
      %{} = m ->
        m[:id] || m["id"] || m[:chosen_id] || m["chosen_id"] || m[:lemma] || m["lemma"]

      other ->
        other
    end
  end

  defp choice_winner_id(_), do: nil

  defp any_slate_competition?(choices) when is_list(choices) do
    Enum.any?(choices, fn ch0 ->
      ch = Safe.to_plain(ch0)

      slate_alts =
        ch[:slate_alt_ids] || ch["slate_alt_ids"] || ch[:alt_ids] || ch["alt_ids"] || []

      probs0 = ch[:probs] || ch["probs"]
      scores0 = ch[:scores] || ch["scores"]

      (is_list(slate_alts) and length(slate_alts) > 0) or
        (is_map(probs0) and map_size(probs0) > 1) or
        (is_map(scores0) and map_size(scores0) > 1)
    end)
  end

  defp any_slate_competition?(_), do: false

  # Attach :slate_alt_ids derived from si_after.sense_candidates.
  defp attach_slate_alt_ids(choices, si_after) when is_list(choices) and is_map(si_after) do
    slate = slate_from_si(si_after)

    if map_size(slate) == 0 do
      choices
    else
      Enum.map(choices, fn
        ch when is_map(ch) ->
          chp = Safe.to_plain(ch)
          idx = normalize_idx(choice_token_index(chp))
          chosen = normalize_signal_id(choice_winner_id(chp))

          alts =
            bucket_for_slate(slate, idx)
            |> List.wrap()
            |> Enum.map(&Safe.to_plain/1)
            |> Enum.map(fn s ->
              s[:id] || s["id"] || s[:chosen_id] || s["chosen_id"] || s[:lemma] || s["lemma"]
            end)
            |> Enum.reject(&is_nil/1)
            |> Enum.map(&to_string/1)
            |> Enum.reject(fn id -> is_binary(chosen) and id == chosen end)
            |> Enum.uniq()

          if alts == [] do
            chp
          else
            Map.put(chp, :slate_alt_ids, alts)
          end

        other ->
          other
      end)
    end
  end

  defp attach_slate_alt_ids(choices, _si_after) when is_list(choices), do: choices

  defp bucket_for_slate(slate, idx) when is_map(slate) do
    cond do
      Map.has_key?(slate, idx) ->
        List.wrap(Map.get(slate, idx))

      is_integer(idx) and Map.has_key?(slate, Integer.to_string(idx)) ->
        List.wrap(Map.get(slate, Integer.to_string(idx)))

      true ->
        []
    end
  end

  defp bucket_for_slate(_slate, _idx), do: []

  # ── Prob/margin backfill for scores=:none ──────────────────────────────────

  defp maybe_backfill_probs_and_margin(choices, scores_mode, si_after, weights, eff_opts)
       when is_list(choices) and is_map(si_after) and is_map(weights) and is_list(eff_opts) do
    choices1 =
      if scores_mode == :none do
        backfill_probs_and_margin_from_slate(choices, si_after, weights, eff_opts)
      else
        choices
      end

    Enum.map(choices1, &ensure_choice_probs_and_margin/1)
  end

  defp ensure_choice_probs_and_margin(ch0) when is_map(ch0) do
    ch = Safe.to_plain(ch0)

    chosen =
      ch
      |> choice_winner_id()
      |> normalize_signal_id()

    probs0 = Map.get(ch, :probs) || Map.get(ch, "probs")
    scores0 = Map.get(ch, :scores) || Map.get(ch, "scores")

    probs =
      cond do
        is_map(probs0) and map_size(probs0) > 0 ->
          normalize_prob_map(probs0)

        is_map(scores0) and map_size(scores0) > 0 ->
          probs_from_scores(scores0)

        is_binary(chosen) ->
          %{chosen => 1.0}

        true ->
          %{}
      end

    probs =
      if probs == %{} and is_binary(chosen) do
        %{chosen => 1.0}
      else
        probs
      end

    {p1, p2} =
      probs
      |> Map.values()
      |> Enum.sort(:desc)
      |> case do
        [a, b | _] -> {a, b}
        [a] -> {a, 0.0}
        _ -> {0.0, 0.0}
      end

    margin =
      case Map.get(ch, :margin) || Map.get(ch, "margin") do
        m when is_number(m) and m >= 0.0 -> m * 1.0
        _ -> max(p1 - p2, 0.0)
      end

    score0 = Map.get(ch, :score) || Map.get(ch, "score")

    ch
    |> Map.put(:probs, probs)
    |> Map.put(:margin, margin)
    |> Map.put(:p_top1, p1)
    |> Map.put(:score, (if is_number(score0), do: score0 * 1.0, else: p1))
  end

  defp ensure_choice_probs_and_margin(other), do: other

  defp normalize_prob_map(m) when is_map(m) do
    items = Enum.map(m, fn {k, v} -> {to_string(k), max(num(v), 0.0)} end)
    z = Enum.reduce(items, 0.0, fn {_k, v}, acc -> acc + v end)

    cond do
      z <= 0.0 ->
        n = max(length(items), 1)
        Enum.into(items, %{}, fn {k, _} -> {k, 1.0 / n} end)

      true ->
        Enum.into(items, %{}, fn {k, v} -> {k, v / z} end)
    end
  end

  defp normalize_prob_map(_), do: %{}

  defp probs_from_scores(scores) when is_map(scores) do
    items = Enum.map(scores, fn {k, v} -> {to_string(k), num(v)} end)
    xs = Enum.map(items, fn {_k, v} -> v end)
    ps = normalize_scores(xs)

    items
    |> Enum.zip(ps)
    |> Enum.into(%{}, fn {{k, _v}, p} -> {k, p} end)
  end

  # ── Control signals ────────────────────────────────────────────────────────

  defp control_signals(choices, si_after, scores_mode, margin_thr, eff_opts) do
    {b0, i0} =
      case scores_mode do
        :none ->
          fb1 = fallback_feedback_from_choices(choices, margin_thr, eff_opts)

# In scores=:none, choices often arrive “thin” (no probs/scores/alt_ids yet),
# so detect/derive competition from the slate itself.
fb2 =
  if map_size(slate_from_si(si_after)) > 0 do
    fallback_feedback_from_slate(si_after, choices, margin_thr, eff_opts)
  else
    %{boosts: [], inhibitions: []}
  end

          boosts =
            (List.wrap(fb1.boosts) ++ List.wrap(fb2.boosts))
            |> Enum.uniq_by(fn b -> {b[:token_index], b[:id]} end)

          inhibitions =
            (List.wrap(fb1.inhibitions) ++ List.wrap(fb2.inhibitions))
            |> Enum.uniq_by(fn i -> {i[:token_index], i[:id]} end)

          {boosts, inhibitions}

        _ ->
          Legacy.boosts_inhibitions(choices, margin_thr, scores_mode, eff_opts)
      end

    boosts = ensure_winner_boosts(choices, b0, eff_opts)
    inhibs = ensure_competitor_inhibitions(choices, i0, margin_thr, eff_opts)

    {boosts, inhibs}
  end

  defp ensure_winner_boosts(choices, boosts, eff_opts) do
    boost_amt = Keyword.get(eff_opts, :boost_amount, 0.05) * 1.0

    boosts1 =
      normalize_signal_entries(
        List.wrap(boosts),
        choices,
        :boost,
        boost_amt
      )

    have = MapSet.new(Enum.map(boosts1, fn b -> {b[:token_index], b[:id]} end))

    add =
      Enum.flat_map(choices, fn ch ->
        idx = normalize_idx(choice_token_index(ch))
        wid = normalize_signal_id(choice_winner_id(ch))

        if is_integer(idx) and idx >= 0 and is_binary(wid) and not MapSet.member?(have, {idx, wid}) do
          [%{token_index: idx, id: wid, amount: boost_amt}]
        else
          []
        end
      end)

    (boosts1 ++ add)
    |> Enum.uniq_by(fn b -> {b[:token_index], b[:id]} end)
  end

defp ensure_competitor_inhibitions(choices, inhibitions, _margin_thr, eff_opts) do
  inhib_amt = Keyword.get(eff_opts, :inhib_amount, 0.02) * 1.0

  inhibitions1 =
    normalize_signal_entries(
      List.wrap(inhibitions),
      choices,
      :inhib,
      inhib_amt
    )

  have = MapSet.new(Enum.map(inhibitions1, fn i -> {i[:token_index], i[:id]} end))

  add =
    Enum.flat_map(choices, fn ch0 ->
      ch = Safe.to_plain(ch0)
      idx = normalize_idx(choice_token_index(ch))
      wid = normalize_signal_id(choice_winner_id(ch))

      if is_integer(idx) and idx >= 0 do
        competitor_ids(ch)
        |> Enum.reject(&is_nil/1)
        |> Enum.map(&to_string/1)
        |> Enum.reject(fn cid -> is_binary(wid) and cid == wid end)
        |> Enum.uniq()
        |> Enum.flat_map(fn cid ->
          if MapSet.member?(have, {idx, cid}) do
            []
          else
            [%{token_index: idx, id: cid, amount: inhib_amt}]
          end
        end)
      else
        []
      end
    end)

  (inhibitions1 ++ add)
  |> Enum.uniq_by(fn i -> {i[:token_index], i[:id]} end)
end

  defp normalize_signal_entries(entries, choices, kind, default_amt)
       when is_list(entries) and is_list(choices) and kind in [:boost, :inhib] do
    Enum.flat_map(entries, fn entry ->
      case entry do
        {id, _any} ->
          expand_signal_by_id(id, choices, kind, default_amt)

        m when is_map(m) ->
          id = Map.get(m, :id) || Map.get(m, :winner_id) || Map.get(m, :chosen_id)
          idx = Map.get(m, :token_index)
          amt = normalize_amount(Map.get(m, :amount), default_amt)

          cond do
            is_integer(idx) and idx >= 0 and is_binary(id) ->
              [%{token_index: idx, id: id, amount: amt}]

            is_binary(id) ->
              expand_signal_by_id(id, choices, kind, amt)

            true ->
              []
          end

        kw when is_list(kw) ->
          if Keyword.keyword?(kw) do
            m = Enum.into(kw, %{})
            id = Map.get(m, :id) || Map.get(m, :winner_id) || Map.get(m, :chosen_id)
            idx = Map.get(m, :token_index)
            amt = normalize_amount(Map.get(m, :amount), default_amt)

            cond do
              is_integer(idx) and idx >= 0 and is_binary(id) ->
                [%{token_index: idx, id: id, amount: amt}]

              is_binary(id) ->
                expand_signal_by_id(id, choices, kind, amt)

              true ->
                []
            end
          else
            []
          end

        _ ->
          []
      end
    end)
  end

  defp normalize_signal_entries(_, _, _, _), do: []

  defp expand_signal_by_id(id0, choices, kind, amt) do
    id = normalize_signal_id(id0)

    if is_binary(id) do
      Enum.flat_map(choices, fn ch ->
        idx = normalize_idx(choice_token_index(ch))
        wid = normalize_signal_id(choice_winner_id(ch))

        case kind do
          :boost ->
            if is_integer(idx) and idx >= 0 and wid == id do
              [%{token_index: idx, id: id, amount: amt}]
            else
              []
            end

          :inhib ->
            cids =
              competitor_ids(ch)
              |> Enum.reject(&is_nil/1)
              |> Enum.map(&normalize_signal_id/1)
              |> Enum.reject(&is_nil/1)
              |> Enum.uniq()

            if is_integer(idx) and idx >= 0 and id in cids and id != wid do
              [%{token_index: idx, id: id, amount: amt}]
            else
              []
            end
        end
      end)
    else
      []
    end
  end

  defp normalize_signal_id(nil), do: nil

  defp normalize_signal_id(v) when is_binary(v) do
    s = String.trim(v)
    if s == "", do: nil, else: s
  end

  defp normalize_signal_id(v) when is_atom(v), do: v |> Atom.to_string() |> normalize_signal_id()
  defp normalize_signal_id(v), do: v |> to_string() |> normalize_signal_id()

  defp normalize_amount(v, _default_amt) when is_number(v), do: v * 1.0
  defp normalize_amount(_v, default_amt), do: default_amt * 1.0

  defp choice_token_index(ch) when is_map(ch) do
    Map.get(ch, :token_index) ||
      Map.get(ch, "token_index") ||
      Map.get(ch, :index) ||
      Map.get(ch, "index") ||
      0
  end

  defp choice_token_index(_), do: 0

  defp competitor_ids(ch) when is_map(ch) do
    alts =
      List.wrap(Map.get(ch, :alt_ids) || Map.get(ch, "alt_ids") || [])
      |> Enum.map(&to_string/1)

    slate_alts =
      List.wrap(Map.get(ch, :slate_alt_ids) || Map.get(ch, "slate_alt_ids") || [])
      |> Enum.map(&to_string/1)

    score_keys =
      case (Map.get(ch, :scores) || Map.get(ch, "scores") || %{}) do
        m when is_map(m) -> Map.keys(m) |> Enum.map(&to_string/1)
        l when is_list(l) -> l |> safe_kv_list_to_map() |> Map.keys() |> Enum.map(&to_string/1)
        _ -> []
      end

    prob_keys =
      case (Map.get(ch, :probs) || Map.get(ch, "probs") || %{}) do
        m when is_map(m) -> Map.keys(m) |> Enum.map(&to_string/1)
        l when is_list(l) -> l |> safe_kv_list_to_map() |> Map.keys() |> Enum.map(&to_string/1)
        _ -> []
      end

    (alts ++ slate_alts ++ score_keys ++ prob_keys)
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.uniq()
  end

  defp competitor_ids(_), do: []

  defp safe_kv_list_to_map(list) when is_list(list) do
    cond do
      Keyword.keyword?(list) -> Enum.into(list, %{})
      Enum.all?(list, &match?({_, _}, &1)) -> Enum.into(list, %{})
      true -> %{}
    end
  rescue
    _ -> %{}
  end

  defp safe_kv_list_to_map(_), do: %{}

  # ── Backfill from slate (scores=:none) ──────────────────────────────────────

  defp backfill_probs_and_margin_from_slate(choices, si_after, weights, eff_opts)
       when is_list(choices) and is_map(si_after) and is_map(weights) and is_list(eff_opts) do
    normalize = Keyword.get(eff_opts, :normalize, :softmax)
    slate = slate_from_si(si_after)

    Enum.map(choices, fn ch0 ->
      ch = Safe.to_plain(ch0)

      probs0 = Map.get(ch, :probs) || %{}
      has_full_probs? = is_map(probs0) and map_size(probs0) > 1
      has_margin? = is_number(Map.get(ch, :margin)) and (Map.get(ch, :margin) || 0.0) > 0.0

      if has_full_probs? and has_margin? do
        ch
      else
        tidx =
          normalize_idx(
            Map.get(ch, :token_index) ||
              Map.get(ch, :index) ||
              token_index_from_choice_token(Map.get(ch, :token) || Map.get(ch, "token")) ||
              token_index_from_slate(slate, choice_winner_id(ch)) ||
              0
          )

        bucket = bucket_for_slate(slate, tidx)

        scored =
          bucket
          |> Enum.map(fn cand ->
            c = Safe.to_plain(cand)
            id = c[:id] || c["id"] || c[:chosen_id] || c["chosen_id"] || "?"
            feats = c[:features] || c["features"] || c
            {to_string(id), candidate_score(feats, weights)}
          end)
          |> Enum.reject(fn {id, _} -> id in [nil, ""] end)

        probs =
          case {normalize, scored} do
            {:softmax, list} when is_list(list) and length(list) >= 1 -> softmax_map(list)
            {_other, list} when is_list(list) and length(list) >= 1 -> linear_norm_map(list)
            _ -> %{}
          end

        {p1, p2} =
          probs
          |> Map.values()
          |> Enum.sort(:desc)
          |> case do
            [a, b | _] -> {a, b}
            [a] -> {a, 0.0}
            _ -> {0.0, 0.0}
          end

        margin = max(p1 - p2, 0.0)

        ch
        |> Map.put(:probs, probs)
        |> Map.put(:margin, margin)
        |> Map.put(:p_top1, p1)
      end
    end)
  end

  defp backfill_probs_and_margin_from_slate(choices, _si_after, _weights, _eff_opts), do: choices

  defp candidate_score(feats, weights) when is_map(feats) and is_map(weights) do
    w_lex = Map.get(weights, :lex_fit, 0.0) * 1.0
    w_rel = Map.get(weights, :rel_prior, 0.0) * 1.0
    w_act = Map.get(weights, :activation, 0.0) * 1.0
    w_int = Map.get(weights, :intent_bias, 0.0) * 1.0

    lex =
      num(
        Map.get(feats, :lex_fit) ||
          Map.get(feats, "lex_fit") ||
          get_in(feats, [:features, :lex_fit]) || 0.0
      )

    rel =
      num(
        Map.get(feats, :rel_prior) ||
          Map.get(feats, "rel_prior") ||
          get_in(feats, [:features, :rel_prior]) || 0.0
      )

    act =
      num(
        Map.get(feats, :activation) ||
          Map.get(feats, "activation") ||
          get_in(feats, [:features, :activation]) || 0.0
      )

    ib =
      num(
        Map.get(feats, :intent_bias) ||
          Map.get(feats, "intent_bias") ||
          get_in(feats, [:features, :intent_bias]) || 0.0
      )

    w_lex * lex + w_rel * rel + w_act * act + w_int * ib
  end

  defp candidate_score(_feats, _weights), do: 0.0

  defp softmax_map(scored) do
    vals = Enum.map(scored, fn {_id, s} -> s end)
    probs = normalize_scores(vals)

    scored
    |> Enum.zip(probs)
    |> Enum.into(%{}, fn {{id, _}, p} -> {id, p} end)
  end

  defp linear_norm_map(scored) do
    vals = Enum.map(scored, fn {_id, s} -> max(s * 1.0, 0.0) end)
    z = Enum.sum(vals)

    probs =
      if z <= 0.0 do
        List.duplicate(1.0 / max(length(vals), 1), length(vals))
      else
        Enum.map(vals, &(&1 / z))
      end

    scored
    |> Enum.map(fn {id, _} -> id end)
    |> Enum.zip(probs)
    |> Enum.into(%{}, fn {id, p} -> {id, p} end)
  end

  defp num(v) when is_integer(v), do: v * 1.0
  defp num(v) when is_float(v), do: v

  defp num(v) when is_binary(v) do
    case Float.parse(v) do
      {f, ""} -> f
      _ -> 0.0
    end
  end

  defp num(_), do: 0.0

  # ── Feedback helpers ───────────────────────────────────────────────────────

# apps/brain/lib/brain/lifg.ex
# Replace fallback_feedback_from_slate/4 with this version.

defp fallback_feedback_from_slate(si_after, choices, margin_thr, eff_opts)
     when is_map(si_after) and is_list(choices) and is_list(eff_opts) do
  thr = (margin_thr || 0.0) * 1.0
  boost_amt = Keyword.get(eff_opts, :boost_amount, 0.05) * 1.0
  inhib_amt = Keyword.get(eff_opts, :inhib_amount, 0.02) * 1.0

  # IMPORTANT: use the same slate accessor you already trust elsewhere
  slate = slate_from_si(si_after)

  winners_by_idx =
    Enum.reduce(choices, %{}, fn ch, acc ->
      tidx = normalize_idx(choice_token_index(ch))
      Map.put(acc, tidx, ch)
    end)

  Enum.reduce(slate, %{boosts: [], inhibitions: []}, fn {raw_idx, senses}, acc ->
    tidx = normalize_idx(raw_idx)
    ch = Map.get(winners_by_idx, tidx)

    if is_nil(ch) do
      acc
    else
      margin = choice_margin(ch)
      chosen_id = choice_winner_id(ch)

      sense_list =
        senses
        |> List.wrap()
        |> Enum.map(&Safe.to_plain/1)

      cond do
        not is_integer(tidx) or tidx < 0 or not is_binary(chosen_id) ->
          acc

        sense_list == [] ->
          acc

        true ->
          # Always inhibit non-winners when there is competition in the slate bucket.
          # This is the behavior your tests are asserting under scores=:none.
          inhib_ids =
            sense_list
            |> Enum.map(fn s -> s[:id] || s["id"] || s[:lemma] || s["lemma"] end)
            |> Enum.reject(&is_nil/1)
            |> Enum.map(&to_string/1)
            |> Enum.reject(&(&1 == chosen_id))
            |> Enum.uniq()

          inhibs =
            Enum.map(inhib_ids, fn sid ->
              %{token_index: tidx, id: sid, amount: inhib_amt}
            end)

          boosts =
            if margin >= thr do
              [%{token_index: tidx, id: chosen_id, amount: boost_amt} | acc.boosts]
            else
              acc.boosts
            end

          %{acc | boosts: boosts, inhibitions: inhibs ++ acc.inhibitions}
      end
    end
  end)
end

defp fallback_feedback_from_slate(_si_after, _choices, _margin_thr, _eff_opts),
  do: %{boosts: [], inhibitions: []}

  defp fallback_feedback_from_choices(choices, margin_thr, eff_opts)
       when is_list(choices) and is_list(eff_opts) do
    thr = margin_thr * 1.0
    boost_amt = Keyword.get(eff_opts, :boost_amount, 0.05) * 1.0
    inhib_amt = Keyword.get(eff_opts, :inhib_amount, 0.02) * 1.0

    Enum.reduce(choices, %{boosts: [], inhibitions: []}, fn ch, acc ->
      margin = choice_margin(ch)
      tok_idx = normalize_idx(choice_token_index(ch))
      chosen = choice_winner_id(ch)

      alt_ids =
        List.wrap(Map.get(ch, :alt_ids) || Map.get(ch, "alt_ids") || [])
        |> Enum.reject(&is_nil/1)
        |> Enum.map(&to_string/1)
        |> Enum.reject(&(&1 == chosen))
        |> Enum.uniq()

      cond do
        not is_integer(tok_idx) or tok_idx < 0 or not is_binary(chosen) ->
          acc

        margin >= thr ->
          boost = %{token_index: tok_idx, id: chosen, amount: boost_amt}
          %{acc | boosts: [boost | acc.boosts]}

        alt_ids != [] ->
          inhibs = Enum.map(alt_ids, fn aid -> %{token_index: tok_idx, id: aid, amount: inhib_amt} end)
          %{acc | inhibitions: inhibs ++ acc.inhibitions}

        true ->
          acc
      end
    end)
  end

  defp fallback_feedback_from_choices(_choices, _margin_thr, _eff_opts),
    do: %{boosts: [], inhibitions: []}

  defp choice_margin(ch) when is_map(ch) do
    m0 =
      Map.get(ch, :margin) ||
        Map.get(ch, "margin") ||
        Map.get(ch, :prob_margin) ||
        Map.get(ch, "prob_margin")

    cond do
      is_number(m0) and m0 > 0.0 ->
        m0 * 1.0

      true ->
        sm0 =
          Map.get(ch, :probs) ||
            Map.get(ch, "probs") ||
            Map.get(ch, :scores) ||
            Map.get(ch, "scores") || %{}

        sm = if is_map(sm0), do: sm0, else: %{}
        vals = sm |> Map.values() |> Enum.map(&num/1) |> Enum.sort(:desc)

        case vals do
          [a, b | _] -> max(a - b, 0.0)
          _ -> 0.0
        end
    end
  end

  defp choice_margin(_), do: 0.0

  defp normalize_idx(i) when is_integer(i) and i >= 0, do: i

  defp normalize_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end

  defp normalize_idx(_), do: 0

  defp flatten_lifg_opts(nil), do: []
  defp flatten_lifg_opts(%{} = m), do: Map.to_list(m)
  defp flatten_lifg_opts(kw) when is_list(kw), do: kw

  # ── Assistant identity injection ───────────────────────────────────────────

  defp ensure_assistant_candidates(%{} = si) do
    tokens = Safe.get(si, :tokens, []) |> List.wrap()

    if tokens == [] do
      si
    else
      sc0 = Safe.get(si, :sense_candidates, %{}) || %{}
      sc = if is_map(sc0), do: sc0, else: %{}
      a = BrainConfig.assistant()
      assistant_id = "#{a.norm}|assistant|0"

      {sc2, added_idxs} =
        tokens
        |> Enum.with_index()
        |> Enum.reduce({sc, []}, fn {tok, i}, {acc, added} ->
          idx = token_index(tok, i)

          if BrainConfig.assistant_match?(tok) and is_integer(idx) do
            if has_candidate_id?(acc, idx, assistant_id) do
              {acc, added}
            else
              cand = assistant_candidate(a, tok, idx, assistant_id)
              {put_candidate(acc, idx, cand), [idx | added]}
            end
          else
            {acc, added}
          end
        end)

      if added_idxs == [] do
        si
      else
        evt = {:assistant_identity, %{token_indices: Enum.reverse(added_idxs), id: assistant_id}}

        si
        |> Map.put(:sense_candidates, sc2)
        |> Map.update(:trace, [evt], fn tr -> [evt | tr] end)
      end
    end
  end

  defp ensure_assistant_candidates(other), do: other

  defp token_index(tok, fallback) when is_map(tok) do
    v = Map.get(tok, :index) || Map.get(tok, "index")
    if is_integer(v), do: v, else: fallback
  end

  defp token_index(_tok, fallback), do: fallback

  defp assistant_candidate(a, tok, token_index, id) do
    raw =
      Map.get(tok, :phrase) ||
        Map.get(tok, "phrase") ||
        Map.get(tok, :norm) ||
        Map.get(tok, "norm") ||
        Map.get(tok, :text) ||
        Map.get(tok, "text") ||
        Map.get(tok, :word) ||
        Map.get(tok, "word") ||
        ""

    matched = norm_text(raw)

    %{
      id: id,
      token_index: token_index,
      lemma: matched,
      norm: a.norm,
      pos: "assistant",
      from: :assistant_identity,
      rank: 0,
      score: 0.95,
      margin: 1.0,
      features: %{assistant: true, matched: matched},
      raw: %{from: :assistant_identity, assistant: a}
    }
  end

  defp put_candidate(sc, idx, cand) do
    Map.update(sc, idx, [cand], fn list ->
      ([cand] ++ List.wrap(list))
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.filter(&is_map/1)
      |> Enum.uniq_by(fn c -> c[:id] || c["id"] end)
      |> Enum.reject(fn c -> is_nil(c[:id] || c["id"]) end)
    end)
  end

  defp has_candidate_id?(sc, idx, id) when is_map(sc) and is_integer(idx) and is_binary(id) do
    case Map.get(sc, idx) do
      nil ->
        false

      list when is_list(list) ->
        Enum.any?(list, fn c ->
          cid = (is_map(c) && (Map.get(c, :id) || Map.get(c, "id"))) || nil
          cid == id
        end)

      _ ->
        false
    end
  end

  defp has_candidate_id?(_, _, _), do: false

  defp norm_text(nil), do: ""

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm_text(v),
    do: v |> Kernel.to_string() |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  # ── Explanation enrichment ─────────────────────────────────────────────────

  defp attach_explanation(%{} = last) do
    already? = Map.has_key?(last, :explanation) or Map.has_key?(last, :explanation_text)

    exp =
      if already? do
        last[:explanation] || %{}
      else
        safe_build_explanation(last)
      end

    if exp == %{} do
      last
    else
      weak? = get_in(exp, [:decision, :weak?]) || false
      audit0 = last[:audit] || %{}

      audit =
        audit0
        |> Map.put_new(:feature_mix, Map.get(last, :feature_mix, %{}))
        |> Map.put(:weak_decisions, (weak? && 1) || Map.get(audit0, :weak_decisions, 0))

      last
      |> Map.put(:explanation, exp)
      |> Map.put(:explanation_text, exp[:text])
      |> Map.put(:audit, audit)
    end
  end

  defp attach_explanation(other), do: other

  defp safe_build_explanation(last) do
    try do
      if Code.ensure_loaded?(Explanation) and function_exported?(Explanation, :build, 1) do
        Explanation.build(last) || %{}
      else
        %{}
      end
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end
end

