#apps/brain/lib/brain/lifg.ex
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
  alias Brain.LIFG.{Input, Post, Explanation}

  alias Brain.LIFG.{
    MWE,
    Choices,
    Legacy,
    Stage1Bridge,
    ACCGate,
    FallbackRerun,
    Recorder,
    Config
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

  si_for_stage =
    si_plain
    |> Map.put(:sense_candidates, slate)
    |> Map.delete(:candidates_by_token)
    |> Map.put_new(:trace, [])
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
      # 1) Plain choices with true margins from Stage1
      base_choices =
        raw_choices
        |> Enum.map(&Safe.to_plain/1)

      # 2) Compute boosts / inhibitions from *raw* margins
      {boosts_out, inhibitions_out} =
        Legacy.boosts_inhibitions(base_choices, margin_thr, scores_mode, eff_opts)

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
    8) (Planned) LIFG.Stage2 sentence-level coherence:
       - not yet wired into this function,
       - will run after `Post.finalize/2` to score whole-sentence interpretations
         and optionally reanalyse low-margin tokens when global gain is sufficient.

  Notes:
  - This function is the region-level orchestration entry point.
  - Callers that only need per-token sense selection can use `disambiguate_stage1/1,2`.
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
            FallbackRerun.maybe_rerun(si3, choices0, weights_for_stage1, scores_mode, margin_thr, opts)

          {si3b, acc_conflict, acc_present?} = ACCGate.assess(si3a, choices, opts)

          acc_conf_tau =
            Keyword.get(opts, :acc_conflict_tau, Application.get_env(:brain, :acc_conflict_tau, 0.50))

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
            Keyword.get(opts, :pmtg_apply?, true) and (not acc_present? or acc_conflict >= acc_conf_tau)

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
                           Keyword.get(opts, :rerun_weights_bump, %{lex_fit: 0.05, rel_prior: 0.05})
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

          post_out =
            Post.finalize(
              final_si,
              final_choices,
              reanalysis?: Keyword.get(opts, :reanalysis?, false),
              fail_fun: Keyword.get(opts, :fail_fun, fn _ -> false end),
              allow_overlaps?: Keyword.get(opts, :allow_overlaps?, false)
            )

          _ =
            Recorder.maybe_record_last(__MODULE__, :run, post_out.si, post_out.choices, audit, %{
              scores_mode: scores_mode,
              margin_threshold: margin_thr,
              pmtg_mode: pmtg_mode,
              pmtg_applied?: pmtg_apply?,
              acc_present?: acc_present?,
              acc_conflict: acc_conflict,
              acc_conflict_tau: acc_conf_tau,
              needy_count: length(needy),
              cover_count: length(post_out.cover),
              flips: post_out.flips
            })

          {:ok, %{si: post_out.si, choices: post_out.choices, slate: slate, cover: post_out.cover, flips: post_out.flips}}

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
  # ─────────────────────── Fallback feedback helper ───────────────────────

  defp fallback_feedback_from_slate(si_after, choices, margin_thr, eff_opts)
       when is_map(si_after) and is_list(choices) and is_list(eff_opts) do
    thr = margin_thr * 1.0

    boost_amt = Keyword.get(eff_opts, :boost_amount, 0.05) * 1.0
    inhib_amt = Keyword.get(eff_opts, :inhib_amount, 0.02) * 1.0

    slate = Map.get(si_after, :sense_candidates, %{}) || %{}

    # Index winners by token index
    winners_by_idx =
      Enum.reduce(choices, %{}, fn ch, acc ->
        tidx =
          Map.get(ch, :token_index) ||
            Map.get(ch, "token_index") ||
            Map.get(ch, :index) ||
            Map.get(ch, "index") ||
            0

        Map.put(acc, tidx, ch)
      end)

    Enum.reduce(slate, %{boosts: [], inhibitions: []}, fn {raw_idx, senses}, acc ->
      tidx = normalize_idx(raw_idx)
      ch = Map.get(winners_by_idx, tidx)

      case ch do
        nil ->
          acc

        _ ->
          margin =
            (Map.get(ch, :margin) ||
               Map.get(ch, "margin") ||
               Map.get(ch, :prob_margin) ||
               Map.get(ch, "prob_margin") ||
               1.0)
            |> Kernel.*(1.0)

          chosen_id =
            Map.get(ch, :chosen_id) ||
              Map.get(ch, "chosen_id") ||
              Map.get(ch, :id) ||
              Map.get(ch, "id")

          sense_list =
            senses
            |> List.wrap()
            |> Enum.map(&Safe.to_plain/1)

          cond do
            not is_integer(tidx) or not is_binary(chosen_id) ->
              acc

            # Clear margin: boost winner
            margin >= thr ->
              boost = %{token_index: tidx, id: chosen_id, amount: boost_amt}
              %{acc | boosts: [boost | acc.boosts]}

            # Tight margin with multiple senses: inhibit all non-winners
            margin < thr and sense_list != [] ->
              inhib_ids =
                sense_list
                |> Enum.map(fn s ->
                  s[:id] || s["id"] || s[:lemma] || s["lemma"]
                end)
                |> Enum.reject(&is_nil/1)
                |> Enum.reject(&(&1 == chosen_id))
                |> Enum.uniq()

              inhibs =
                Enum.map(inhib_ids, fn sid ->
                  %{token_index: tidx, id: sid, amount: inhib_amt}
                end)

              %{acc | inhibitions: inhibs ++ acc.inhibitions}

            true ->
              acc
          end
      end
    end)
  end

  defp normalize_idx(i) when is_integer(i) and i >= 0, do: i

  defp normalize_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end

  defp normalize_idx(_), do: 0

  # ─────────────────────── Fallback feedback helper ───────────────────────

  defp fallback_feedback_from_choices(choices, margin_thr, eff_opts)
       when is_list(choices) and is_list(eff_opts) do
    thr = margin_thr * 1.0

    boost_amt = Keyword.get(eff_opts, :boost_amount, 0.05) * 1.0
    inhib_amt = Keyword.get(eff_opts, :inhib_amount, 0.02) * 1.0

    Enum.reduce(choices, %{boosts: [], inhibitions: []}, fn ch, acc ->
      margin =
        (Map.get(ch, :margin) || Map.get(ch, :prob_margin) || 1.0)
        |> Kernel.*(1.0)

      tok_idx =
        Map.get(ch, :token_index) ||
          Map.get(ch, :index) ||
          0

      chosen =
        Map.get(ch, :chosen_id) ||
          Map.get(ch, :id)

      alt_ids =
        ch
        |> Map.get(:alt_ids, [])
        |> List.wrap()
        |> Enum.uniq()

      cond do
        not is_integer(tok_idx) or not is_binary(chosen) ->
          acc

        # Clear margin: winner gets a boost
        margin >= thr and alt_ids != [] ->
          boost = %{token_index: tok_idx, id: chosen, amount: boost_amt}
          %{acc | boosts: [boost | acc.boosts]}

        # Tight margin with alternatives: losers get inhibited
        margin < thr and alt_ids != [] ->
          inhibs =
            Enum.map(alt_ids, fn aid ->
              %{token_index: tok_idx, id: aid, amount: inhib_amt}
            end)

          %{acc | inhibitions: inhibs ++ acc.inhibitions}

        true ->
          acc
      end
    end)
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
      # Use the matched surface as lemma (useful for traces), but keep norm as assistant.norm
      lemma: matched,
      norm: a.norm,
      pos: "assistant",
      from: :assistant_identity,
      rank: 0,
      # Conservative but strong prior; Stage1 still re-scores.
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

