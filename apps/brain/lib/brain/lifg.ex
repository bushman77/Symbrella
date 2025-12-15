defmodule Brain.LIFG do
  @moduledoc """
  Left Inferior Frontal Gyrus (LIFG) — Competitive Sense Selection.

  Entry points:
    • Legacy-compatible API (pure): `disambiguate_stage1/1,2`
      - Calls the new `Brain.LIFG.Stage1.run/2` engine and preserves the legacy event shape.
    • Full pipeline: `run/2`
      (ATL finalize → Stage-1 → optional fallback-rerun → optional ACC gate → optional pMTG
       → optional Stage-2 (sentence-level scaffold) → Post.finalize)

  Optional ACC hook:
    If `Brain.ACC` is available, `run/2` will compute a conflict score
    and only apply pMTG if `conflict >= acc_conflict_tau`. When ACC is absent,
    behavior is unchanged (pMTG applies according to opts).

  Assistant identity (Option B):
    If a token matches `Brain.Config.assistant/0` (name/norm/aliases), LIFG injects a
    system-entity candidate into `si.sense_candidates[token_index]` so the assistant
    name is handled as an internal entity (not a dictionary lookup).

  Stage-2 scaffold (forward-looking):
    `Brain.LIFG.Stage2.run/2` is invoked once per full `run/2` pipeline, after
    Stage-1 + pMTG have produced final choices, but before `Post.finalize/4`.
    At present Stage2 is an inert scaffold by default:

      • It never mutates `si` unless explicitly opted in (`attach_trace?: true`).
      • It returns `{:skip, ...}` unless explicitly enabled (`enabled?: true`).

    The wiring here ensures that once Stage2 grows sentence-level coherence logic,
    it can begin to influence the SI *without* further changes to the rest of the
    LIFG pipeline.
  """

  use Brain, region: :lifg
  require Logger

  alias Brain.Utils.Safe
  alias Brain.Config, as: BrainConfig
  alias Brain.LIFG.{Input, Post, Explanation}

  alias Brain.LIFG.{
    MWE,
    Choices,
    Stage1Bridge,
    ACCGate,
    FallbackRerun,
    Recorder,
    Config,
    Stage2
  }

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
  def disambiguate_stage1(%{} = si, opts) do
    si_plain = Safe.to_plain(si)
    slate = Input.slate_for(si_plain)

    # First build a clean SI for Stage-1 and keep the sense_candidates around
    si_for_stage0 =
      si_plain
      |> Map.put(:sense_candidates, slate)
      |> Map.delete(:candidates_by_token)
      |> Map.put_new(:trace, [])

    sense_candidates = Map.get(si_for_stage0, :sense_candidates, %{})

    si_for_stage =
      si_for_stage0
      |> ensure_assistant_candidates()
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
        # 1) Plain choices with true margins from Stage-1
        base_choices =
          raw_choices
          |> Enum.map(&Safe.to_plain/1)

        # 2) Compute boosts / inhibitions from full candidate slate.
        #    Winners go to boosts; all other candidates become inhibitions.
        %{boosts: boosts_out, inhibitions: inhibitions_out} =
          fallback_feedback_from_slate(
            si_after,
            base_choices,
            sense_candidates,
            margin_thr,
            eff_opts
          )

        # 3) Augment choices for downstream consumers (alt_ids, min_margin, etc.)
        choices =
          base_choices
          |> Choices.augment(si_after, min_margin)

        _ =
          Recorder.maybe_record_last(__MODULE__, :stage1, si_after, choices, audit, %{
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

  # ── Full pipeline (optional ATL + ACC + pMTG + Stage-2) ───────────────────

  @doc """
  Full LIFG pipeline:
    1) ATL finalize → slate (if available)
    2) Attach si.sense_candidates
    3) Stage-1 disambiguation (probabilities & margins)
    4) Fallback-rerun if winners are `|phrase|fallback` and WM disallows fallbacks
    5) ACC assess (optional; computes :conflict and appends trace)
    6) pMTG consult (sync rerun or async boost/none), gated by ACC when available
    7) Stage-2 consult (sentence-level scaffold; default `:skip`)
    8) Post.finalize: non-overlap cover and optional reanalysis
  """
  @spec run(map(), keyword()) ::
          {:ok,
           %{
             si: map(),
             choices: list(),
             slate: map(),
             cover: list(),
             flips: non_neg_integer()
           }}
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
        if Code.ensure_loaded?(Brain.ATL) and
             function_exported?(Brain.ATL, :attach_sense_candidates, 3) do
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

      si2a =
        si2
        |> MWE.ensure_mwe_candidates(opts)
        |> MWE.absorb_unigrams_into_mwe(opts)
        |> MWE.backfill_unigrams_from_active_cells(opts)

      scores_mode =
        Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

      margin_thr = Keyword.get(opts, :margin_threshold, 0.15)

      min_margin =
        Keyword.get(opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.05))

      weights_for_stage1 =
        Config.lifg_weights()
        |> Map.merge(Map.new(Keyword.get(opts, :weights, [])))

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

          {si3a, choices} =
            FallbackRerun.maybe_rerun(
              si3,
              choices0,
              weights_for_stage1,
              scores_mode,
              margin_thr,
              opts
            )

          {si3b, acc_conflict, acc_present?} = ACCGate.assess(si3a, choices, opts)

          acc_conf_tau =
            Keyword.get(
              opts,
              :acc_conflict_tau,
              Application.get_env(:brain, :acc_conflict_tau, 0.50)
            )

          needy =
            Enum.filter(choices, fn ch ->
              m = (ch[:margin] || 1.0) * 1.0
              alts? = (ch[:alt_ids] || []) != []

              p1 =
                (ch[:scores] || %{})
                |> Map.values()
                |> Enum.max(fn -> 0.0 end)
                |> Kernel.*(1.0)

              (m < margin_thr or p1 < Application.get_env(:brain, :acc_p_min, 0.65)) and alts?
            end)

          pmtg_mode = Keyword.get(opts, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost))

          pmtg_apply? =
            Keyword.get(opts, :pmtg_apply?, true) and
              (not acc_present? or acc_conflict >= acc_conf_tau)

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
                         Safe.get(si3b, :tokens, []),
                         already_needy: false,
                         margin_threshold: margin_thr,
                         limit: Keyword.get(opts, :limit, 5),
                         mode: :rerun,
                         rerun_only_if_hits: Keyword.get(opts, :rerun_only_if_hits, true),
                         rerun_weights_bump:
                           Keyword.get(
                             opts,
                             :rerun_weights_bump,
                             %{lex_fit: 0.05, rel_prior: 0.05}
                           )
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

          # ── Stage-2 (sentence-level scaffold; fail-open) ───────────────────
          stage2_opts = Keyword.get(opts, :stage2_opts, [])

          {si_for_post, stage2_meta} =
            if Code.ensure_loaded?(Stage2) and function_exported?(Stage2, :run, 2) do
              try do
                case Stage2.run(final_si, stage2_opts) do
                  {:ok, %{si: si4} = meta} ->
                    {si4, meta}

                  {:skip, %{si: si4} = meta} ->
                    {si4, meta}

                  {:error, reason} ->
                    Logger.debug(fn -> "[LIFG.Stage2] error: #{inspect(reason)}" end)
                    {final_si, %{error: reason}}
                end
              rescue
                e ->
                  Logger.error("[LIFG.Stage2] exception: #{inspect(e)}")
                  {final_si, %{error: e}}
              end
            else
              {final_si, %{reason: :stage2_unavailable}}
            end

          # ── Post.finalize (non-overlap cover + optional reanalysis) ────────
          post_out =
            Post.finalize(
              si_for_post,
              final_choices,
              reanalysis?: Keyword.get(opts, :reanalysis?, false),
              fail_fun: Keyword.get(opts, :fail_fun, fn _ -> false end),
              allow_overlaps?: Keyword.get(opts, :allow_overlaps?, false)
            )

          _ =
            Recorder.maybe_record_last(
              __MODULE__,
              :run,
              post_out.si,
              post_out.choices,
              audit,
              %{
                scores_mode: scores_mode,
                margin_threshold: margin_thr,
                pmtg_mode: pmtg_mode,
                pmtg_applied?: pmtg_apply?,
                acc_present?: acc_present?,
                acc_conflict: acc_conflict,
                acc_conflict_tau: acc_conf_tau,
                needy_count: length(needy),
                cover_count: length(post_out.cover),
                flips: post_out.flips,
                stage2: stage2_meta
              }
            )

          {:ok,
           %{
             si: post_out.si,
             choices: post_out.choices,
             slate: slate,
             cover: post_out.cover,
             flips: post_out.flips
           }}

        {:error, reason} ->
          _ =
            Recorder.maybe_record_last(__MODULE__, :run_error, si2a, [], %{}, %{
              error: inspect(reason)
            })

          {:error, {:stage1, reason}}
      end
    rescue
      e ->
        Logger.error("LIFG full run failed: #{inspect(e)}")

        _ =
          Recorder.maybe_record_last(__MODULE__, :run_exception, si, [], %{}, %{
            error: inspect(e)
          })

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

  # ── Internal helpers ───────────────────────────────────────────────────────

  # Normalize any token index-ish value into a non-negative integer.
  defp normalize_idx(i) when is_integer(i) and i >= 0, do: i

  defp normalize_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end

  defp normalize_idx(_), do: 0

  # ───────────────────── Feedback helpers ─────────────────────

  @doc false
  @spec fallback_feedback_from_slate(
          map(),
          list(),
          map(),
          number(),
          keyword()
        ) :: %{
          boosts: list(map()),
          inhibitions: list(map())
        }
  defp fallback_feedback_from_slate(
         _si_after,
         base_choices,
         sense_candidates,
         margin_thr,
         eff_opts
       ) do
    # Flatten the slate into a list of %{id, token_index}
    all_cands =
      sense_candidates
      |> Enum.flat_map(fn {idx, cands} ->
        cands
        |> List.wrap()
        |> Enum.map(fn cand ->
          cand_plain = Safe.to_plain(cand)

          %{
            id: cand_plain[:id],
            token_index: cand_plain[:token_index] || normalize_idx(idx)
          }
        end)
      end)
      |> Enum.reject(&is_nil(&1.id))

    all_ids = MapSet.new(Enum.map(all_cands, & &1.id))

    # Normalize choices once so we can reuse them.
    choices_plain =
      base_choices
      |> Enum.map(&Safe.to_plain/1)

    boosts_from_choices =
      choices_plain
      |> Enum.map(fn ch ->
        idx = normalize_idx(ch[:token_index] || ch[:index] || 0)
        amt = feedback_amount(:boost, ch, margin_thr, eff_opts)

        %{
          id: ch[:id],
          token_index: idx,
          amount: amt
        }
      end)
      |> Enum.reject(&is_nil(&1.id))

    winner_ids = MapSet.new(Enum.map(boosts_from_choices, & &1.id))

    # If the slate is empty or only contains winners, it is not sufficient.
    # In that case we fall back to alt_ids on the winners.
    use_alt? =
      MapSet.size(all_ids) == 0 or MapSet.equal?(all_ids, winner_ids)

    if use_alt? do
      # alt_ids-based path: winners are boosts; all alt_ids become inhibitions.
      fallback_feedback_from_choices(base_choices, margin_thr, eff_opts)
    else
      # Normal path: full slate → boosts + inhibitions for every candidate.
      winners_by_idx =
        Enum.reduce(choices_plain, %{}, fn choice, acc ->
          idx = normalize_idx(Map.get(choice, :token_index, 0))
          Map.put(acc, idx, choice)
        end)

      {boosts_acc, inhib_acc} =
        sense_candidates
        |> Enum.reduce({[], []}, fn {idx, cands}, {b_acc, i_acc} ->
          idx_norm = normalize_idx(idx)
          winner_choice = Map.get(winners_by_idx, idx_norm)

          winner_id =
            case winner_choice do
              nil -> nil
              ch -> ch[:chosen_id] || ch[:id]
            end

          Enum.reduce(cands, {b_acc, i_acc}, fn cand, {b1, i1} ->
            cand_plain = Safe.to_plain(cand)

            id =
              cand_plain[:id] ||
                cand_plain[:chosen_id] ||
                cand_plain[:norm] ||
                cand_plain[:lemma]

            if is_nil(id) do
              {b1, i1}
            else
              kind =
                if not is_nil(winner_id) and winner_id == id do
                  :boost
                else
                  :inhibit
                end

              amt = feedback_amount(kind, winner_choice, margin_thr, eff_opts)

              entry = %{
                id: id,
                token_index: idx_norm,
                amount: amt
              }

              case kind do
                :boost -> {[entry | b1], i1}
                :inhibit -> {b1, [entry | i1]}
              end
            end
          end)
        end)

      boosts = Enum.reverse(boosts_acc)
      inhibitions = Enum.reverse(inhib_acc)

      %{boosts: boosts, inhibitions: inhibitions}
    end
  end

  defp fallback_feedback_from_choices(base_choices, margin_thr, eff_opts) do
    choices =
      base_choices
      |> Enum.map(&Safe.to_plain/1)

    boosts =
      choices
      |> Enum.map(fn ch ->
        idx = normalize_idx(ch[:token_index] || ch[:index] || 0)
        amt = feedback_amount(:boost, ch, margin_thr, eff_opts)

        %{
          id: ch[:id],
          token_index: idx,
          amount: amt
        }
      end)
      |> Enum.reject(&is_nil(&1.id))

    winner_ids = MapSet.new(Enum.map(boosts, & &1.id))

    inhibitions =
      choices
      |> Enum.flat_map(fn ch ->
        idx = normalize_idx(ch[:token_index] || ch[:index] || 0)
        winner_choice = ch
        alt_ids = List.wrap(ch[:alt_ids] || [])

        Enum.map(alt_ids, fn alt_id ->
          amt = feedback_amount(:inhibit, winner_choice, margin_thr, eff_opts)

          %{
            id: alt_id,
            token_index: idx,
            amount: amt
          }
        end)
      end)
      |> Enum.reject(&is_nil(&1.id))
      |> Enum.reject(fn cand -> MapSet.member?(winner_ids, cand.id) end)
      |> Enum.uniq_by(&{&1.id, &1.token_index})

    %{boosts: boosts, inhibitions: inhibitions}
  end

  defp feedback_amount(kind, winner_choice, margin_thr, eff_opts) do
    margin = winner_margin(winner_choice, margin_thr)

    boost_base = Keyword.get(eff_opts, :boost_amount, 0.25)
    inhib_base = Keyword.get(eff_opts, :inhibit_amount, -0.25)

    raw =
      case kind do
        :boost -> boost_base + margin
        :inhibit -> inhib_base - margin
      end

    clamp(raw, -1.0, 1.0)
  end

  defp winner_margin(%{margin: m}, _margin_thr) when is_number(m), do: m * 1.0
  defp winner_margin(_winner_choice, margin_thr) when is_number(margin_thr), do: margin_thr * 1.0
  defp winner_margin(_winner_choice, _margin_thr), do: 0.0

  defp clamp(v, min, max) when is_number(v) and is_number(min) and is_number(max) do
    v
    |> max(min)
    |> min(max)
  end

  defp flatten_lifg_opts(nil), do: []
  defp flatten_lifg_opts(%{} = m), do: Map.to_list(m)
  defp flatten_lifg_opts(kw) when is_list(kw), do: kw

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
    do:
      v
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

  defp norm_text(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

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

