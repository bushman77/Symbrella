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

  @default_weights %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}

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

# was: def start_link(opts \\ %{}) when is_list(opts) or is_map(opts) do
def start_link(opts) when is_list(opts) or is_map(opts) do
  GenServer.start_link(__MODULE__, Map.new(opts), name: name())
end

  # ── Server bootstrap & runtime config ────────────────────────────────

  @impl true
  def init(opts) do
    eff = opts |> Map.new() |> effective_opts()
    {:ok, %{region: :lifg, opts: eff, last: nil}}
  end

  @doc "Returns effective options (env + overrides), even if the server hasn't started."
  def status(server \\ __MODULE__) do
    case GenServer.whereis(server) do
      nil ->
        effective_opts(%{})

      _pid ->
        try do
          GenServer.call(server, :status)
        catch
          :exit, _ -> effective_opts(%{})
        end
    end
  end

  @doc """
  Returns the most recent LIFG decision snapshot recorded by this server, or :empty.
  """
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
    eff = effective_opts(state.opts || %{})
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
    {:noreply, %{state | opts: effective_opts(merged)}}
  end

  @impl true
  def handle_cast({:record_last, payload}, state) when is_map(payload) do
    {:noreply, %{state | last: payload}}
  end

  # ── Stage-1 (legacy-compatible event shape) ──────────────────────────

  @doc """
  Pure, stateless Stage-1 wrapper over `Brain.LIFG.Stage1.run/2`.

  Preserves legacy event shape with `:choices/:boosts/:inhibitions`.
  - Boost amount = chosen margin (>= 0)
  - Inhibition amount = max(margin_threshold - score_gap, 0.0)
  - Supports `{id, amount}` tuples when `:emit_pairs` (or WM gating flags) are present.
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
      |> ensure_mwe_candidates(opts)
      |> absorb_unigrams_into_mwe(opts)
      |> backfill_unigrams_from_active_cells(opts)

    # Effective options (env + si + call-site)
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
      lifg_weights()
      |> Map.merge(Map.new(Keyword.get(eff_opts, :weights, [])))

    res =
      safe_stage1_call(
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

    case normalize_stage1_result(res) do
      {:ok, si_after, raw_choices, audit} ->
        choices =
          raw_choices
          |> Enum.map(&Safe.to_plain/1)
          |> augment_choices(si_after, min_margin)

        {boosts_out, inhibitions_out} =
          legacy_boosts_inhibitions(choices, margin_thr, scores_mode, eff_opts)

        _ =
          maybe_record_last(:stage1, si_after, choices, audit, %{
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
        _ = maybe_record_last(:stage1_error, si_for_stage, [], %{}, %{error: inspect(reason)})

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
    4) ACC assess (optional; computes :conflict and appends trace)
    5) pMTG consult (sync rerun or async boost/none), gated by ACC when available
    6) Post.finalize: non-overlap cover and optional reanalysis

  Returns:
    {:ok, %{si, choices, slate, cover, flips}} or {:error, term()}.
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
      si2a = ensure_mwe_candidates(si2, opts)

      scores_mode =
        Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

      margin_thr = Keyword.get(opts, :margin_threshold, 0.15)

      min_margin =
        Keyword.get(opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.05))

      # 3) Stage-1 disambiguation
      case Brain.LIFG.Stage1.run(
             si2a,
             weights:
               Map.get(opts, :weights, %{
                 lex_fit: 0.5,
                 rel_prior: 0.25,
                 activation: 0.15,
                 intent_bias: 0.10
               }),
             scores: scores_mode,
             margin_threshold: margin_thr,
             chargram_event: [:brain, :lifg, :chargram_violation],
             boundary_event: [:brain, :lifg, :boundary_drop]
           ) do
        {:ok, %{si: si3, choices: raw_choices, audit: audit}} ->
          choices =
            raw_choices
            |> Enum.map(&Safe.to_plain/1)
            |> augment_choices(si3, min_margin)

          # 4) ACC (optional). If present, record conflict & gate pMTG by threshold.
          {si3a, acc_conflict, acc_present?} = maybe_acc_gate(si3, choices, opts)

          acc_conf_tau =
            Keyword.get(
              opts,
              :acc_conflict_tau,
              Application.get_env(:brain, :acc_conflict_tau, 0.50)
            )

          # 5) pMTG consult (respect ACC gate when ACC is present)
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
                         Safe.get(si3a, :tokens, []),
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
                    _ -> {si3a, choices}
                  end

                _other ->
                  _ =
                    Brain.PMTG.consult(
                      choices,
                      Safe.get(si3a, :tokens, []),
                      margin_threshold: margin_thr,
                      limit: Keyword.get(opts, :limit, 5),
                      mode: pmtg_mode
                    )

                  {si3a, choices}
              end
            else
              {si3a, choices}
            end

          # 6) Post.finalize — non-overlap cover + optional reanalysis
          post_out =
            Post.finalize(
              final_si,
              final_choices,
              reanalysis?: Keyword.get(opts, :reanalysis?, false),
              fail_fun: Keyword.get(opts, :fail_fun, fn _ -> false end),
              allow_overlaps?: Keyword.get(opts, :allow_overlaps?, false)
            )

          maybe_record_last(:run, post_out.si, post_out.choices, audit, %{
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
          maybe_record_last(:run_error, si2a, [], %{}, %{error: inspect(reason)})
          {:error, {:stage1, reason}}
      end
    rescue
      e ->
        Logger.error("LIFG full run failed: #{inspect(e)}")
        maybe_record_last(:run_exception, si, [], %{}, %{error: inspect(e)})
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

    if na <= 1.0e-15 or nb <= 1.0e-15,
      do: 0.0,
      else:
        (Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)) / (na * nb)
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

  # ── Internal helpers ─────────────────────────────────────────────────

  # Flatten :lifg_opts embedded as kw/map into a keyword list
  defp flatten_lifg_opts(nil), do: []
  defp flatten_lifg_opts(%{} = m), do: Map.to_list(m)
  defp flatten_lifg_opts(kw) when is_list(kw), do: kw

  defp lifg_weights do
    env = Application.get_env(:brain, :lifg_stage1_weights, %{})
    Map.merge(@default_weights, env || %{})
  end

  # Build the effective option set we report via `status/0`
  defp effective_opts(overrides) when is_map(overrides) do
    %{
      weights:
        lifg_weights()
        |> Map.merge(Map.new(Map.get(overrides, :weights, %{}))),
      scores:
        Map.get(overrides, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all)),
      margin_threshold: Map.get(overrides, :margin_threshold, 0.15),
      min_margin: Application.get_env(:brain, :lifg_min_margin, 0.05),
      pmtg_mode: Map.get(overrides, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost)),
      pmtg_window_keep: Application.get_env(:brain, :pmtg_window_keep, 50),
      acc_conflict_tau: Application.get_env(:brain, :acc_conflict_tau, 0.50),
      mwe_fallback: Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
    }
  end

  # Robust Stage1 call (supports new/legacy arities + shapes)
  defp safe_stage1_call(si, kw_opts, eff_opts) do
    try do
      Brain.LIFG.Stage1.run(si, kw_opts)
    rescue
      _ ->
        weights_only = Keyword.get(kw_opts, :weights, %{})
        Brain.LIFG.Stage1.run(si, weights_only, eff_opts)
    end
  end

  # Normalize all supported return shapes into a single tuple
  defp normalize_stage1_result(result) do
    case result do
      {:ok, %{si: si, choices: choices, audit: audit}}
           when is_map(si) and is_list(choices) and is_map(audit) ->
        {:ok, si, choices, audit}

      {:ok, si, meta} when is_map(si) and is_map(meta) ->
        choices = Map.get(meta, :choices, [])
        audit   = Map.get(meta, :audit, %{})
        {:ok, si, List.wrap(choices), audit}

      {:ok, si} when is_map(si) ->
        {:ok, si, [], %{}}

      other ->
        {:error, other}
    end
  end

  # Choice augmentation: ensure chosen_id/alt_ids/margin are present & sane
  defp augment_choices(raw_choices, si_after, min_margin) when is_list(raw_choices) do
    slate =
      Safe.get(si_after, :sense_candidates, %{}) ||
        Safe.get(si_after, :candidates_by_token, %{}) || %{}

    Enum.map(raw_choices, fn ch0 ->
      ch = Safe.to_plain(ch0)
      idx = Safe.get(ch, :token_index, 0)
      scores = Safe.get(ch, :scores, %{}) || %{}

      chosen_id =
        case Safe.get(ch, :chosen_id) do
          nil ->
            case scores |> Enum.max_by(fn {_id, s} -> s end, fn -> nil end) do
              {id, _} -> id
              _ -> nil
            end

          x ->
            x
        end

      alt_from_scores = if map_size(scores) > 0, do: Map.keys(scores), else: []

      slate_alts =
        case Map.get(slate, idx) do
          list when is_list(list) ->
            list
            |> Enum.map(fn c ->
              c = Safe.to_plain(c)
              Safe.get(c, :id) || Safe.get(c, :lemma) || Safe.get(c, :word)
            end)
            |> Enum.reject(&is_nil/1)

          _ ->
            []
        end

      chosen_id_s = if is_nil(chosen_id), do: nil, else: to_string(chosen_id)

      alt_ids =
        (Safe.get(ch, :alt_ids, []) ++ alt_from_scores ++ slate_alts)
        |> Enum.map(&to_string/1)
        |> Enum.reject(&(&1 in [nil, ""]))
        |> Enum.uniq()
        |> then(fn ids -> if chosen_id_s, do: ids -- [chosen_id_s], else: ids end)

      singleton? = length(alt_ids) == 0

      margin0 =
        case Safe.get(ch, :margin) do
          m when is_number(m) and m > 0.0 and not singleton? ->
            m

          _ ->
            vals = scores |> Map.values() |> Enum.sort(:desc)
            case vals do
              [a, b | _] -> a - b
              [_a] -> 0.0
              _ -> 0.0
            end
        end

      margin = Float.round(Kernel.max(margin0, min_margin), 6)

      ch
      |> Map.put(:chosen_id, chosen_id_s)
      |> Map.put(:alt_ids, alt_ids)
      |> Map.put(:margin, margin)
    end)
  end

  # Legacy boosts/inhibitions view for downstream consumers
  defp legacy_boosts_inhibitions(choices, margin_thr, _scores_mode, eff_opts) do
    {boosts_maps, inhibitions_maps} =
      Enum.reduce(choices, {[], []}, fn ch, {boos, inhs} ->
        token_index = Safe.get(ch, :token_index, 0)
        chosen_id = Safe.get(ch, :chosen_id, nil)
        margin = (Safe.get(ch, :margin, 0.0) || 0.0) * 1.0
        scores = Safe.get(ch, :scores, %{}) || %{}

        top_s =
          if is_map(scores) and map_size(scores) > 0 do
            Map.get(scores, chosen_id, 0.0) * 1.0
          else
            0.0
          end

        loser_ids =
          if is_map(scores) and map_size(scores) > 0 do
            scores |> Map.keys() |> Enum.reject(&(&1 == chosen_id))
          else
            Safe.get(ch, :alt_ids, [])
          end

        boost_here = %{
          token_index: token_index,
          id: chosen_id,
          amount: Float.round(Kernel.max(margin, 0.0), 6)
        }

        inhibitions_here =
          Enum.map(loser_ids, fn lid ->
            gap = Kernel.max(top_s - Map.get(scores, lid, 0.0), 0.0)
            amt = Float.round(Kernel.max(margin_thr - gap, 0.0), 6)
            %{token_index: token_index, id: lid, amount: amt}
          end)

        {[boost_here | boos], inhibitions_here ++ inhs}
      end)

    pairs_mode? =
      Keyword.has_key?(eff_opts, :gate_into_wm) or
        Keyword.has_key?(eff_opts, :lifg_min_score) or
        Keyword.get(eff_opts, :emit_pairs, false)

    to_pair = fn
      %{id: id, amount: amt} -> {to_string(id), (amt || 0.0) * 1.0}
      {id, amt} -> {to_string(id), (amt || 0.0) * 1.0}
      _ -> nil
    end

    if pairs_mode? do
      {
        boosts_maps |> Enum.map(&to_pair.(&1)) |> Enum.reject(&is_nil/1) |> Enum.reverse(),
        inhibitions_maps |> Enum.map(&to_pair.(&1)) |> Enum.reject(&is_nil/1) |> Enum.reverse()
      }
    else
      {Enum.reverse(boosts_maps), Enum.reverse(inhibitions_maps)}
    end
  end

  # ── MWE helpers & unigram backfill ------------------------------------------

  # If a multiword token has no compatible MWE senses, synthesize a lightweight phrase candidate,
  # then backfill real phrase cells from active_cells (if present).
  defp ensure_mwe_candidates(%{tokens: tokens} = si, opts) when is_list(tokens) do
    enable? =
      Keyword.get(
        opts,
        :mwe_fallback,
        Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
      )

    unless enable?, do: si

    sc0 = Map.get(si, :sense_candidates, %{})

    {sc, emitted} =
      Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
        token_n = Map.get(tok, :n, if(Map.get(tok, :mw, false), do: 2, else: 1))
        phrase  = Map.get(tok, :phrase) || Map.get(tok, :lemma)
        mw?     = Map.get(tok, :mw, token_n > 1)

        cond do
          not mw? or is_nil(phrase) ->
            {acc, n}

          has_compatible_mwe?(acc, idx) ->
            {acc, n}

          true ->
            # heuristic score: bounded so real phrase cells can win
            score_guess =
              acc
              |> unigram_neighbor_idxs(idx)
              |> Enum.map(fn j ->
                acc
                |> Map.get(j, [])
                |> Enum.map(&Map.get(&1, :score, 0.0))
                |> Enum.max(fn -> 0.0 end)
              end)
              |> case do
                [] -> 0.25
                xs -> Enum.sum(xs) / max(length(xs), 1)
              end
              |> min(0.45)

            candidate = %{
              id: "#{phrase}|phrase|fallback",
              lemma: phrase,
              norm: phrase,
              mw: true,
              pos: :phrase,
              rel_prior: 0.30,
              score: Float.round(score_guess, 4),
              source: :mwe_fallback
            }

            updated = Map.update(acc, idx, [candidate], fn lst -> [candidate | lst] end)

            safe_exec_telemetry([:brain, :pmtg, :mwe_fallback_emitted], %{count: 1}, %{
              token_index: idx,
              phrase: phrase,
              score: candidate.score
            })

            {updated, n + 1}
        end
      end)

    si1 = if emitted > 0, do: Map.put(si, :sense_candidates, sc), else: si
    backfill_real_mwe_from_active_cells(si1)
  end

  defp ensure_mwe_candidates(si, _opts), do: si

# --- absorb unigram senses into overlapping MWE buckets -----------------

defp absorb_unigrams_into_mwe(%{tokens: toks} = si, opts) when is_list(toks) do
  # opt-in to avoid surprises; turn on with opts: [absorb_unigrams_into_mwe?: true]
  unless Keyword.get(opts, :absorb_unigrams_into_mwe?, false), do: si

  sc0    = Map.get(si, :sense_candidates, %{})
  cells  = Safe.get(si, :active_cells, []) |> Enum.map(&Safe.to_plain/1)

  cells_by_norm =
    Enum.group_by(cells, fn c ->
      (Safe.get(c, :norm) || Safe.get(c, :word) || "") |> down()
    end)

  updated =
    Enum.reduce(Enum.with_index(toks), sc0, fn {tok, idx}, acc ->
      n   = Safe.get(tok, :n, 1)
      mw? = Safe.get(tok, :mw, n > 1)
      mwe_span = Safe.get(tok, :span)

      if mw? and is_tuple(mwe_span) do
        child_norms =
          toks
          |> Enum.with_index()
          |> Enum.filter(fn {t, j} ->
            j != idx and Safe.get(t, :n, 1) == 1 and inside?(Safe.get(t, :span), mwe_span)
          end)
          |> Enum.map(fn {t, _} ->
            (Safe.get(t, :phrase) || Safe.get(t, :word) || "") |> down()
          end)
          |> Enum.reject(&(&1 == ""))

        absorbed =
          child_norms
          |> Enum.flat_map(&Map.get(cells_by_norm, &1, []))
          |> Enum.map(fn c ->
            norm = (Safe.get(c, :norm) || Safe.get(c, :word) || "") |> to_string()
            %{
              id: to_string(Safe.get(c, :id)),
              lemma: norm,
              norm: norm,
              mw: false,
              pos: Safe.get(c, :pos),
              rel_prior: 0.20,
              score: 0.20,
              source: :absorbed_unigram
            }
          end)

        if absorbed == [] do
          acc
        else
          merged =
            absorbed ++ Map.get(acc, idx, [])
            |> Enum.uniq_by(&(&1[:id] || &1["id"]))

          Map.put(acc, idx, merged)
        end
      else
        acc
      end
    end)

  Map.put(si, :sense_candidates, updated)
end

defp absorb_unigrams_into_mwe(si, _opts), do: si

# helpers used above
defp inside?({s, l}, {ps, pl})
     when is_integer(s) and is_integer(l) and is_integer(ps) and is_integer(pl) do
  e  = s + l
  pe = ps + pl
  s >= ps and e <= pe
end
defp inside?(_, _), do: false

defp down(s) when is_binary(s),
  do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")
defp down(_), do: ""

  # If a multiword token only has a |phrase|fallback, inject real phrase cells from si.active_cells.
  defp backfill_real_mwe_from_active_cells(si) do
    sc0    = Map.get(si, :sense_candidates, %{})
    tokens = Map.get(si, :tokens, [])
    cells  = Safe.get(si, :active_cells, [])

    phrase_cells_by_norm =
      cells
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.filter(fn c ->
        p = Safe.get(c, :pos)
        to_string(p) == "phrase"
      end)
      |> Enum.group_by(fn c ->
        String.downcase(to_string(Safe.get(c, :norm) || Safe.get(c, :word)))
      end)

    {sc, added} =
      Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
        phrase = Safe.get(tok, :phrase)
        mw?    = Safe.get(tok, :mw, false) or (Safe.get(tok, :n, 1) > 1)
        bucket = Map.get(acc, idx, [])

        has_real? =
          Enum.any?(bucket, fn c ->
            pos  = Safe.get(c, :pos)
            norm = Safe.get(c, :norm) || Safe.get(c, :lemma) || ""
            (pos == :phrase or to_string(pos) == "phrase") and String.contains?(norm, " ")
          end)

        cond do
          not mw? or is_nil(phrase) or has_real? ->
            {acc, n}

          true ->
            norm_key = String.downcase(to_string(phrase))
            case Map.get(phrase_cells_by_norm, norm_key, []) do
              [] -> {acc, n}
              list ->
                cands =
                  Enum.map(list, fn c ->
                    id = Safe.get(c, :id) || "#{phrase}|phrase|0"
                    %{
                      id: to_string(id),
                      lemma: phrase,
                      norm: phrase,
                      mw: true,
                      pos: :phrase,
                      rel_prior: 0.30,
                      score: 0.30,
                      source: :active_cells
                    }
                  end)

                {Map.put(acc, idx, cands ++ bucket), n + length(cands)}
            end
        end
      end)

    if added > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  defp has_compatible_mwe?(sc, idx) do
    sc
    |> Map.get(idx, [])
    |> Enum.any?(fn c ->
      norm = c[:norm] || c["norm"] || c[:lemma] || c["lemma"] || ""
      String.contains?(norm, " ")
    end)
  end

  # Neighbor heuristic: immediate neighbors as unigram companions
  defp unigram_neighbor_idxs(_sc, idx), do: [idx - 1, idx + 1] |> Enum.filter(&(&1 >= 0))

  # --- Unigram backfill from active_cells -------------------------------------

  defp backfill_unigrams_from_active_cells(si, _opts) when is_map(si) do
    sc0    = Map.get(si, :sense_candidates, %{})
    tokens = Map.get(si, :tokens, [])
    cells  = Safe.get(si, :active_cells, [])

    # Group real (non-phrase) cells by normalized norm
    cells_by_norm =
      cells
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.reject(fn c ->
        pos = Safe.get(c, :pos)
        to_string(pos) == "phrase"
      end)
      |> Enum.group_by(fn c ->
        norm_key(Safe.get(c, :norm) || Safe.get(c, :word))
      end)

    {sc, added} =
      tokens
      |> Enum.with_index()
      |> Enum.reduce({sc0, 0}, fn {tok, idx}, {acc, n} ->
        n_tok = Safe.get(tok, :n, 1)
        mw?   = Safe.get(tok, :mw, n_tok > 1)
        surface =
          Safe.get(tok, :phrase) ||
            Safe.get(tok, :word) ||
            Safe.get(tok, :lemma)

        cond do
          mw? or is_nil(surface) ->
            {acc, n}

          true ->
            nk      = norm_key(surface)
            bucket  = Map.get(acc, idx, [])
            have_unigram? =
              Enum.any?(bucket, &unigram_candidate?/1)

            if have_unigram? do
              {acc, n}
            else
              case Map.get(cells_by_norm, nk, []) do
                [] ->
                  {acc, n}

                list ->
                  existing_ids =
                    bucket
                    |> Enum.map(fn c -> c[:id] || c["id"] end)
                    |> MapSet.new()

                  cands =
                    list
                    |> Enum.map(&cell_to_unigram_candidate(&1, surface))
                    |> Enum.reject(&(is_nil(&1[:id]) or MapSet.member?(existing_ids, &1[:id])))

                  if cands == [] do
                    {acc, n}
                  else
                    :telemetry.execute(
                      [:brain, :lifg, :unigram_backfill_emitted],
                      %{count: length(cands)},
                      %{token_index: idx, norm: surface}
                    )

                    {Map.put(acc, idx, cands ++ bucket), n + length(cands)}
                  end
              end
            end
        end
      end)

    if added > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  defp cell_to_unigram_candidate(cell, surface) do
    id  = Safe.get(cell, :id) || Safe.get(cell, "id")
    pos = Safe.get(cell, :pos) || Safe.get(cell, "pos") || :other

    %{
      id: to_string(id),
      lemma: surface,
      norm: surface,
      mw: false,
      pos: pos,
      # Small priors that let real phrase/unigram cells compete fairly:
      rel_prior: 0.20,
      # If DB carries an activation, pass it through; else a gentle baseline
      activation: (Safe.get(cell, :activation, Safe.get(cell, :modulated_activation, 0.25)) || 0.25) * 1.0,
      score: 0.30,
      source: :active_cells
    }
  end

  defp unigram_candidate?(cand) do
    nrm = cand[:norm] || cand["norm"] || cand[:lemma] || cand["lemma"] || ""
    nrm = to_string(nrm)
    nrm != "" and not String.contains?(nrm, " ")
  end

  defp norm_key(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  defp norm_key(_), do: ""

  # ── Optional ACC hook & telemetry -------------------------------------------

  # Returns {si_with_trace, conflict_score, acc_present?}
  defp maybe_acc_gate(si, choices, opts) when is_map(si) and is_list(choices) do
    acc_present? =
      Code.ensure_loaded?(Brain.ACC) and
        (function_exported?(Brain.ACC, :assess, 3) or
           function_exported?(Brain.ACC, :assess, 2) or
           function_exported?(Brain.ACC, :score, 2))

    if acc_present? do
      now_ms = System.system_time(:millisecond)

      # Use apply/3 so the compiler doesn't warn about undefined/private ACC functions.
      result =
        cond do
          function_exported?(Brain.ACC, :assess, 3) -> apply(Brain.ACC, :assess, [si, choices, opts])
          function_exported?(Brain.ACC, :assess, 2) -> apply(Brain.ACC, :assess, [si, choices])
          function_exported?(Brain.ACC, :score, 2)  -> apply(Brain.ACC, :score,  [si, choices])
          true -> :skip
        end

      case result do
        {:ok, %{si: si2, conflict: c} = payload} when is_number(c) ->
          ev = payload |> Map.drop([:si]) |> Map.merge(%{stage: :acc, ts_ms: now_ms})
          si3 = Map.update(si2, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si3, clamp01(c), true}

        {:ok, c} when is_number(c) ->
          ev = %{stage: :acc, conflict: c, ts_ms: now_ms}
          si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si2, clamp01(c), true}

        c when is_number(c) ->
          ev = %{stage: :acc, conflict: c, ts_ms: now_ms}
          si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si2, clamp01(c), true}

        _ ->
          {si, 1.0, false}
      end
    else
      {si, 1.0, false}
    end
  end

  defp clamp01(x) when is_number(x) do
    x = x * 1.0
    cond do
      x < 0.0 -> 0.0
      x > 1.0 -> 1.0
      true -> x
    end
  end

  defp clamp01(_), do: 0.0

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end

  # ── Recorder for the last decision ------------------------------------------

  defp maybe_record_last(source, si, choices, audit, meta) do
    server = GenServer.whereis(__MODULE__)
    if is_pid(server) do
      payload = build_last_payload(source, si, choices, audit, meta)
      GenServer.cast(server, {:record_last, payload})
    end
    :ok
  end

  defp build_last_payload(source, si, choices, audit, meta) do
    now = System.system_time(:millisecond)

    tokens =
      Safe.get(si, :tokens, [])
      |> Enum.map(fn t ->
        %{
          index: Safe.get(t, :index, 0),
          phrase:
            Safe.get(t, :phrase) ||
              Safe.get(t, :text) ||
              Safe.get(t, :lemma),
          mw: Safe.get(t, :mw, false),
          n: Safe.get(t, :n, 1),
          span: Safe.get(t, :span)
        }
      end)

    finalists =
      Enum.map(choices, fn ch ->
        scores = Safe.get(ch, :scores, %{}) || %{}
        ranking =
          scores
          |> Enum.sort_by(fn {_id, s} -> -s end)

        %{
          token_index: Safe.get(ch, :token_index, 0),
          ranking: ranking
        }
      end)

    guards = %{
      chargram_violation:
        (is_map(audit) && Map.get(audit, :chargram_violation)) ||
          Safe.get(si, :chargram_violation, 0) || 0,
      rejected_by_boundary:
        (is_map(audit) && Map.get(audit, :rejected_by_boundary)) || []
    }

    %{
      ts_ms: now,
      source: source,
      si_sentence: Safe.get(si, :sentence),
      intent: Safe.get(si, :intent),
      confidence: Safe.get(si, :confidence),
      tokens: tokens,
      choices:
        Enum.map(choices, fn ch ->
          %{
            token_index: Safe.get(ch, :token_index, 0),
            chosen_id: Safe.get(ch, :chosen_id),
            margin: Safe.get(ch, :margin, 0.0),
            scores: Safe.get(ch, :scores, %{}) || %{},
            alt_ids: Safe.get(ch, :alt_ids, []) || []
          }
        end),
      finalists: finalists,
      guards: guards,
      feature_mix: Map.get(audit || %{}, :feature_mix, %{}),
      audit: audit || %{},
      meta: meta || %{}
    }
  end
end

