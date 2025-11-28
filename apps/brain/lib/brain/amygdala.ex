defmodule Brain.Amygdala do
  @moduledoc """
  Amygdala — fast affective appraisal over latents (threat/safety/reward/control).

  Responsibilities
  ----------------
  • Fuse three sources into a reaction:
      1. Current MoodCore snapshot (latents + indices)
      2. Episodic emotional priors from Hippocampus recall
      3. Instantaneous deltas from the current intent/utterance
  • Maintain a short-lived "emotional memory" (decaying latent state)
  • Emit a compact reaction map and telemetry for downstream consumers

  Public API
  ----------
    • start_link/1      — start the amygdala GenServer
    • react/2           — compute a reaction for a stimulus, update state, and return it
    • snapshot/0        — inspect internal latent state and last reaction
    • subscribe/0       — subscribe a process to amygdala reactions via Telemetry
    • detach/1          — detach a Telemetry subscriber

  Telemetry
  ---------
    [:brain, :amygdala, :reaction]

    Measurements:
      %{
        threat: float(),
        safety: float(),
        reward: float(),
        control: float(),
        arousal: float(),
        tone_reaction: atom(),
        valence: :negative | :positive | :mixed | :neutral
      }

    Metadata:
      %{
        intent: atom() | nil,
        confidence: float(),
        keyword: String.t() | nil,
        episodes_used: non_neg_integer(),
        mood_used?: boolean(),
        source_latents: :mood | :episodes | :both | :baseline,
        baseline: latents(),
        raw: map()
      }

  Stimulus shape (flexible)
  -------------------------
  `react/2` accepts any map; it tries to normalize from common fields:

    %{
      intent:        atom(),
      confidence:    float(),
      keyword:       String.t(),
      tone_hint:     atom(),
      mood_snapshot: Brain.MoodCore.snapshot() | map(),
      episodes:      [episode_rec()],
      evidence:      %{episodes: [episode_rec()]},
      hippo_episodes:[episode_rec()]
    }

  An `episode_rec()` is expected to be what Hippocampus attaches into
  `si.evidence[:episodes]`, i.e. a map that may contain:

    %{
      score: float(),
      emotion_priors: %{
        threat_prior: float(),
        safety_prior: float(),
        reward_prior: float(),
        control_prior: float(),
        tone_reaction: atom()
      },
      episode: %{meta: map(), ...}
    }

  The module is intentionally **read-only** with respect to MoodCore:
  it does not call `Brain.MoodCore.bump/1` or `apply_intent/2`. That’s
  left to higher-level policy modules that can listen to amygdala
  telemetry and decide how strongly to push the neuromodulators.
  """

  use GenServer

  @event_reaction [:brain, :amygdala, :reaction]

  @type latent_key :: :threat | :safety | :reward | :control
  @type latents :: %{optional(latent_key()) => float()}
  @type tone :: :warm | :cool | :deescalate | :neutral

  @type reaction :: %{
          latents: latents(),
          tone_reaction: tone(),
          valence: :negative | :positive | :mixed | :neutral,
          arousal: float(),
          from: %{
            intent: atom() | nil,
            confidence: float(),
            keyword: String.t() | nil,
            episodes_used: non_neg_integer(),
            mood_used?: boolean(),
            source_latents: :mood | :episodes | :both | :baseline
          }
        }

  @type stimulus :: map()

  # ---------- Public API ----------

  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  @doc """
  Compute a reaction for `stimulus`, update the amygdala state, and return it.

  This is the main entry point for the pipeline.

      Brain.Amygdala.react(%{
        intent: :abuse,
        confidence: 0.98,
        keyword: "fuck you symbrella",
        evidence: %{episodes: si.evidence[:episodes]}
      })
  """
  @spec react(stimulus(), keyword()) :: reaction()
  def react(stimulus, opts \\ []) when is_map(stimulus) or is_struct(stimulus),
    do: GenServer.call(__MODULE__, {:react, stimulus, Map.new(opts)})

  @doc """
  Full state snapshot (baseline, memory latents, last reaction).

  Intended for debugging / dashboard use.
  """
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(__MODULE__, :snapshot)

  @doc """
  Subscribe the current process to amygdala reactions.

  Messages received:

      {:amygdala_reaction, measurements_map, meta_map}
  """
  def subscribe() do
    id = "amygdala-subscriber-#{System.unique_integer([:positive])}"
    :telemetry.attach(id, @event_reaction, &__MODULE__.handle_reaction_event/4, self())
    {:ok, id}
  end

  def handle_reaction_event(_ev, meas, meta, pid) when is_pid(pid) do
    send(pid, {:amygdala_reaction, meas, meta})
  end

  @doc "Detach a previously registered subscriber id returned from subscribe/0."
  def detach(id) when is_binary(id), do: :telemetry.detach(id)

  # ---------- GenServer ----------

  @impl true
  def init(_opts) do
    cfg = Application.get_env(:brain, __MODULE__, []) |> normalize_cfg()

    baseline =
      %{
        threat: 0.25,
        safety: 0.65,
        reward: 0.50,
        control: 0.55
      }
      |> merge_latent_defaults(get_latent_map(cfg, :baseline_latents))

    weights =
      %{
        mood: 0.50,
        episodes: 0.30,
        instant: 0.20,
        memory: 0.30
      }
      |> merge_weight_defaults(get_weight_map(cfg, :weights))
      |> renorm_weights()

    half_life_ms =
      cfg
      |> Keyword.get(:half_life_ms, 20_000)
      |> to_pos_int(20_000)

    state = %{
      baseline: baseline,
      weights: weights,
      half_life_ms: half_life_ms,
      memory_latents: baseline,
      last_reaction: nil,
      last_at: now_ms()
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:react, stimulus_in, opts}, _from, state) do
    now = now_ms()
    ctx = normalize_stimulus(stimulus_in)

    {reaction, new_state} = compute_reaction(ctx, opts, state, now)

    meas = %{
      threat: reaction.latents.threat,
      safety: reaction.latents.safety,
      reward: reaction.latents.reward,
      control: reaction.latents.control,
      arousal: reaction.arousal,
      tone_reaction: reaction.tone_reaction,
      valence: reaction.valence
    }

    meta = %{
      intent: reaction.from.intent,
      confidence: reaction.from.confidence,
      keyword: reaction.from.keyword,
      episodes_used: reaction.from.episodes_used,
      mood_used?: reaction.from.mood_used?,
      source_latents: reaction.from.source_latents,
      baseline: state.baseline,
      raw: ctx.raw
    }

    :telemetry.execute(@event_reaction, meas, meta)

    {:reply, reaction, new_state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    reply = %{
      baseline: state.baseline,
      weights: state.weights,
      half_life_ms: state.half_life_ms,
      memory_latents: state.memory_latents,
      last_reaction: state.last_reaction,
      last_at: state.last_at,
      age_ms: max(now_ms() - (state.last_at || now_ms()), 0)
    }

    {:reply, reply, state}
  end

  # ---------- Core reaction logic ----------

  defp compute_reaction(ctx, _opts, state, now) do
    baseline = state.baseline

    # 1) Grab mood snapshot and latents (or fall back to baseline)
    {mood_latents, mood_indices, mood_used?} =
      case ctx.mood_snapshot do
        %{} = snap ->
          lats = ensure_latents(Map.get(snap, :latents) || Map.get(snap, :latents, %{}), baseline)

          indices =
            Map.get(snap, :mood) ||
              Map.get(snap, :mood_indices) ||
              %{}

          {lats, indices, true}

        _ ->
          {baseline, %{}, false}
      end

    # 2) Aggregate episodic emotional priors from Hippocampus
    episodic_latents = aggregate_episode_latents(ctx.episodes)

    # 3) Instantaneous deltas from current intent
    inst_deltas = latent_deltas_from_intent(ctx.intent, ctx.confidence)

    instant_latents = apply_latent_deltas(mood_latents, inst_deltas, baseline)

    # 4) Fuse mood + episodic + instant into a candidate reaction
    fused_without_memory =
      fuse_latents(
        mood_latents,
        episodic_latents,
        instant_latents,
        baseline,
        state.weights
      )

    # 5) Decay previous emotional memory towards baseline, then blend
    decayed_memory =
      decay_latents(
        state.memory_latents || baseline,
        baseline,
        max(now - (state.last_at || now), 0),
        state.half_life_ms
      )

    fused =
      fuse_with_memory(
        fused_without_memory,
        decayed_memory,
        baseline,
        state.weights
      )

    # 6) Classify tone + valence + arousal
    tone = classify_tone(fused, mood_indices, ctx.tone_hint)
    valence = classify_valence(fused)
    arousal = estimate_arousal(mood_indices, fused)

    source_latents =
      cond do
        episodic_latents != %{} and mood_used? -> :both
        episodic_latents != %{} -> :episodes
        mood_used? -> :mood
        true -> :baseline
      end

    reaction = %{
      latents: fused,
      tone_reaction: tone,
      valence: valence,
      arousal: arousal,
      from: %{
        intent: ctx.intent,
        confidence: ctx.confidence,
        keyword: ctx.keyword,
        episodes_used: length(ctx.episodes || []),
        mood_used?: mood_used?,
        source_latents: source_latents
      }
    }

    new_state = %{
      state
      | memory_latents: fused,
        last_reaction: reaction,
        last_at: now
    }

    {reaction, new_state}
  end

  # ---------- Stimulus normalization ----------

  defp normalize_stimulus(stimulus_in) do
    stim =
      case stimulus_in do
        %_struct{} = s -> Map.from_struct(s)
        %{} = m -> m
        other -> %{:raw => other}
      end

    intent =
      case Map.get(stim, :intent) || Map.get(stim, "intent") do
        i when is_atom(i) -> i
        i when is_binary(i) -> String.to_atom(i)
        _ -> nil
      end

    confidence =
      stim[:confidence] || stim["confidence"] || stim[:conf] || stim["conf"] || 1.0

    confidence =
      case confidence do
        c when is_number(c) ->
          clamp(c, 0.0, 1.0)

        c when is_binary(c) ->
          case Float.parse(String.trim(c)) do
            {n, _} -> clamp(n, 0.0, 1.0)
            _ -> 1.0
          end

        _ ->
          1.0
      end

    keyword =
      stim[:keyword] || stim["keyword"] ||
        stim[:sentence] || stim["sentence"]

    tone_hint =
      stim[:tone_hint] || stim["tone_hint"] ||
        stim[:tone_reaction] || stim["tone_reaction"] ||
        stim[:tone] || stim["tone"]

    mood_snapshot =
      stim[:mood_snapshot] || stim["mood_snapshot"] ||
        safe_mood_snapshot()

    episodes =
      cond do
        is_list(stim[:episodes]) ->
          stim[:episodes]

        is_list(stim["episodes"]) ->
          stim["episodes"]

        is_map(stim[:hippo_episodes]) and is_list(stim[:hippo_episodes]) ->
          stim[:hippo_episodes]

        is_map(stim["hippo_episodes"]) and is_list(stim["hippo_episodes"]) ->
          stim["hippo_episodes"]

        is_map(stim[:evidence]) and is_list(stim[:evidence][:episodes]) ->
          stim[:evidence][:episodes]

        is_map(stim["evidence"]) and is_list(stim["evidence"]["episodes"]) ->
          stim["evidence"]["episodes"]

        true ->
          []
      end

    %{
      intent: intent,
      confidence: confidence,
      keyword: if(is_binary(keyword), do: keyword, else: nil),
      tone_hint: normalize_tone(tone_hint),
      mood_snapshot: mood_snapshot,
      episodes: List.wrap(episodes),
      raw: stimulus_in
    }
  end

  defp safe_mood_snapshot do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :snapshot, 0) do
      try do
        Brain.MoodCore.snapshot()
      rescue
        _ -> nil
      catch
        _, _ -> nil
      end
    else
      nil
    end
  end

  # ---------- Episodic aggregation ----------

  defp aggregate_episode_latents(episodes) do
    episodes = List.wrap(episodes || [])

    {sum_w, acc} =
      Enum.reduce(episodes, {0.0, empty_latents()}, fn rec, {sw, acc} ->
        score =
          case rec do
            %{score: s} when is_number(s) -> max(s, 0.0)
            _ -> 0.0
          end

        if score <= 0.0 do
          {sw, acc}
        else
          ep =
            case rec do
              %{emotion_priors: m} when is_map(m) -> m
              _ -> %{}
            end

          thr = to_unit(ep[:threat_prior] || ep["threat_prior"])
          saf = to_unit(ep[:safety_prior] || ep["safety_prior"])
          rew = to_unit(ep[:reward_prior] || ep["reward_prior"])
          ctl = to_unit(ep[:control_prior] || ep["control_prior"])

          # Skip completely empty priors
          if is_nil(thr) and is_nil(saf) and is_nil(rew) and is_nil(ctl) do
            {sw, acc}
          else
            lat = %{
              threat: thr || 0.0,
              safety: saf || 0.0,
              reward: rew || 0.0,
              control: ctl || 0.0
            }

            new_acc =
              Enum.reduce(lat, acc, fn {k, v}, acc_inner ->
                Map.update!(acc_inner, k, &(&1 + score * v))
              end)

            {sw + score, new_acc}
          end
        end
      end)

    if sum_w <= 0.0 do
      %{}
    else
      Enum.into(acc, %{}, fn {k, v} -> {k, clamp(v / sum_w, 0.0, 1.0)} end)
    end
  end

  defp empty_latents do
    %{threat: 0.0, safety: 0.0, reward: 0.0, control: 0.0}
  end

  # ---------- Intent → latent deltas ----------

  defp latent_deltas_from_intent(nil, _conf), do: %{}

  defp latent_deltas_from_intent(intent, conf) do
    k = clamp(conf || 1.0, 0.0, 1.0)

    case intent do
      :abuse ->
        %{
          threat: +0.45 * k,
          safety: -0.35 * k,
          reward: -0.20 * k,
          control: -0.25 * k
        }

      :insult ->
        latent_deltas_from_intent(:abuse, conf)

      :gratitude ->
        %{
          threat: -0.25 * k,
          safety: +0.40 * k,
          reward: +0.35 * k,
          control: +0.20 * k
        }

      :greeting ->
        %{
          threat: -0.10 * k,
          safety: +0.20 * k,
          reward: +0.10 * k,
          control: +0.05 * k
        }

      :help ->
        %{
          threat: -0.05 * k,
          safety: +0.25 * k,
          reward: +0.15 * k,
          control: +0.15 * k
        }

      :question ->
        # Curious/neutral: small uptick in control/reward; safety mostly unchanged
        %{
          threat: 0.0,
          safety: +0.05 * k,
          reward: +0.12 * k,
          control: +0.12 * k
        }

      :instruction ->
        %{
          threat: 0.0,
          safety: 0.0,
          reward: +0.05 * k,
          control: +0.15 * k
        }

      _other ->
        # Gentle, mostly neutral nudge
        %{
          threat: -0.02 * k,
          safety: +0.05 * k,
          reward: +0.05 * k,
          control: +0.05 * k
        }
    end
  end

  defp apply_latent_deltas(latents, deltas, baseline) do
    base = ensure_latents(latents, baseline)

    Enum.reduce([:threat, :safety, :reward, :control], %{}, fn k, acc ->
      v = Map.get(base, k, Map.get(baseline, k, 0.5))
      d = Map.get(deltas, k, 0.0)
      Map.put(acc, k, clamp(v + d, 0.0, 1.0))
    end)
  end

  # ---------- Fusion & decay ----------

  defp fuse_latents(mood_latents, episodic_latents, instant_latents, baseline, weights) do
    mood = ensure_latents(mood_latents, baseline)

    ep =
      if episodic_latents == %{}, do: baseline, else: ensure_latents(episodic_latents, baseline)

    inst = ensure_latents(instant_latents, baseline)

    wm = weights.mood
    we = if episodic_latents == %{}, do: 0.0, else: weights.episodes
    wi = weights.instant

    norm = max(wm + we + wi, 0.0001)

    Enum.reduce([:threat, :safety, :reward, :control], %{}, fn k, acc ->
      mv = Map.get(mood, k)
      ev = Map.get(ep, k)
      iv = Map.get(inst, k)

      fused = (wm * mv + we * ev + wi * iv) / norm
      Map.put(acc, k, clamp(fused, 0.0, 1.0))
    end)
  end

  defp decay_latents(latents, baseline, dt_ms, half_life_ms) do
    hl = max(half_life_ms, 1)

    fac =
      if dt_ms <= 0 do
        1.0
      else
        :math.pow(0.5, dt_ms / hl)
      end

    base = ensure_latents(latents, baseline)

    Enum.reduce([:threat, :safety, :reward, :control], %{}, fn k, acc ->
      b = Map.get(baseline, k, 0.5)
      v = Map.get(base, k, b)
      Map.put(acc, k, clamp(b + (v - b) * fac, 0.0, 1.0))
    end)
  end

  defp fuse_with_memory(fused, memory, baseline, weights) do
    new = ensure_latents(fused, baseline)
    mem = ensure_latents(memory, baseline)
    wm = weights.memory

    # Simple blend: mostly new, with a tail from memory
    α = clamp(wm, 0.0, 1.0)

    Enum.reduce([:threat, :safety, :reward, :control], %{}, fn k, acc ->
      nv = Map.get(new, k)
      mv = Map.get(mem, k)
      blended = (1.0 - α) * nv + α * mv
      Map.put(acc, k, clamp(blended, 0.0, 1.0))
    end)
  end

  # ---------- Classification ----------

  defp classify_tone(latents, mood_indices, tone_hint) do
    thr = Map.get(latents, :threat, 0.5)
    saf = Map.get(latents, :safety, 0.5)
    rew = Map.get(latents, :reward, 0.5)
    ctl = Map.get(latents, :control, 0.5)

    vig =
      case mood_indices do
        %{vigilance: v} when is_number(v) -> v
        _ -> (thr + saf) / 2.0
      end

    exp =
      case mood_indices do
        %{exploration: v} when is_number(v) -> v
        _ -> (rew + ctl) / 2.0
      end

    # If we were passed an explicit tone hint that clearly indicates danger,
    # respect it unless we're obviously in a very safe latent configuration.
    case normalize_tone(tone_hint) do
      :deescalate when thr >= 0.4 and saf <= 0.8 ->
        :deescalate

      :cool when thr >= 0.35 and saf <= 0.85 ->
        :cool

      _ ->
        :ok
    end
    |> case do
      :deescalate -> :deescalate
      :cool -> :cool
      :ok -> classify_tone_from_latents(thr, saf, rew, ctl, vig, exp)
    end
  end

  defp classify_tone_from_latents(thr, saf, rew, ctl, vig, exp) do
    # Rough ranges:
    #   • deescalate: high threat + high vigilance, safety not able to compensate
    #   • cool: moderate threat OR low control with middling reward
    #   • warm: strong safety + reward, threat clearly low
    #   • neutral: otherwise
    cond do
      thr >= 0.75 and vig >= 0.70 and saf <= 0.65 ->
        :deescalate

      thr >= 0.55 and ctl <= 0.45 ->
        :cool

      saf >= 0.70 and rew >= 0.60 and thr <= 0.45 ->
        :warm

      # High exploration with middling threat tends toward warm-ish
      exp >= 0.65 and thr <= 0.55 and saf >= 0.55 ->
        :warm

      true ->
        :neutral
    end
  end

  defp classify_valence(latents) do
    thr = Map.get(latents, :threat, 0.5)
    saf = Map.get(latents, :safety, 0.5)
    rew = Map.get(latents, :reward, 0.5)

    cond do
      thr > saf and rew < 0.45 ->
        :negative

      saf >= thr and rew > 0.55 ->
        :positive

      abs(rew - 0.5) < 0.08 and abs(saf - thr) < 0.08 ->
        :neutral

      true ->
        :mixed
    end
  end

  defp estimate_arousal(mood_indices, latents) do
    base =
      case mood_indices do
        %{vigilance: v, exploration: e} when is_number(v) and is_number(e) ->
          0.5 * (clamp(v, 0.0, 1.0) + clamp(e, 0.0, 1.0))

        %{vigilance: v} when is_number(v) ->
          clamp(v, 0.0, 1.0)

        _ ->
          0.5
      end

    thr = Map.get(latents, :threat, 0.5)
    rew = Map.get(latents, :reward, 0.5)

    # Threat and reward both increase arousal; clamp.
    clamp(0.4 * base + 0.3 * thr + 0.3 * rew, 0.0, 1.0)
  end

  # ---------- Config helpers ----------

  defp normalize_cfg(cfg) when is_list(cfg) do
    if Keyword.keyword?(cfg), do: cfg, else: []
  end

  defp normalize_cfg(%{} = cfg), do: Map.to_list(cfg)
  defp normalize_cfg(_), do: []

  defp get_latent_map(cfg, key) do
    case Keyword.get(cfg, key, %{}) do
      %{} = m -> m
      l when is_list(l) -> Map.new(l)
      _ -> %{}
    end
  end

  defp get_weight_map(cfg, key) do
    case Keyword.get(cfg, key, %{}) do
      %{} = m -> m
      l when is_list(l) -> Map.new(l)
      _ -> %{}
    end
  end

  defp merge_latent_defaults(defaults, overrides) do
    Enum.reduce(defaults, %{}, fn {k, v}, acc ->
      val =
        case overrides do
          %{} -> Map.get(overrides, k, v)
          _ -> v
        end

      Map.put(acc, k, to_unit(val) || v)
    end)
  end

  defp merge_weight_defaults(defaults, overrides) do
    Enum.reduce(defaults, %{}, fn {k, v}, acc ->
      val =
        case overrides do
          %{} -> Map.get(overrides, k, v)
          _ -> v
        end

      Map.put(acc, k, to_float(val, v))
    end)
  end

  defp renorm_weights(weights) do
    sum =
      [:mood, :episodes, :instant, :memory]
      |> Enum.map(&Map.get(weights, &1, 0.0))
      |> Enum.sum()

    if sum <= 0.0 do
      %{mood: 0.5, episodes: 0.3, instant: 0.2, memory: 0.3}
    else
      Enum.reduce([:mood, :episodes, :instant, :memory], %{}, fn k, acc ->
        v = Map.get(weights, k, 0.0)
        Map.put(acc, k, v / sum)
      end)
    end
  end

  # ---------- Small helpers ----------

  defp ensure_latents(map, baseline) do
    %{
      threat: clamp(Map.get(map, :threat, Map.get(baseline, :threat, 0.5)), 0.0, 1.0),
      safety: clamp(Map.get(map, :safety, Map.get(baseline, :safety, 0.5)), 0.0, 1.0),
      reward: clamp(Map.get(map, :reward, Map.get(baseline, :reward, 0.5)), 0.0, 1.0),
      control: clamp(Map.get(map, :control, Map.get(baseline, :control, 0.5)), 0.0, 1.0)
    }
  end

  defp to_unit(nil), do: nil

  defp to_unit(v) when is_number(v) do
    v = v * 1.0

    cond do
      v < 0.0 -> 0.0
      v > 1.0 -> 1.0
      true -> v
    end
  end

  defp to_unit(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {n, _} -> to_unit(n)
      _ -> nil
    end
  end

  defp to_unit(_), do: nil

  defp to_pos_int(v, _default) when is_integer(v) and v > 0, do: v

  defp to_pos_int(v, _default) when is_float(v) and v > 0.0, do: trunc(v)

  defp to_pos_int(v, default) when is_binary(v) do
    case Integer.parse(String.trim(v)) do
      {n, _} when n > 0 -> n
      _ -> default
    end
  end

  defp to_pos_int(_, default), do: default

  defp to_float(v, _default) when is_number(v), do: v * 1.0

  defp to_float(v, default) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {n, _} -> n
      _ -> default * 1.0
    end
  end

  defp to_float(_, default), do: default * 1.0

  defp clamp(v, lo, hi) when is_number(v) and is_number(lo) and is_number(hi) and lo <= hi do
    cond do
      v < lo -> lo
      v > hi -> hi
      true -> v
    end
  end

  defp now_ms, do: System.monotonic_time(:millisecond)

  defp normalize_tone(nil), do: nil

  defp normalize_tone(t) when is_atom(t) do
    case t do
      :warm -> :warm
      :cool -> :cool
      :deescalate -> :deescalate
      :neutral -> :neutral
      _ -> nil
    end
  end

  defp normalize_tone(t) when is_binary(t) do
    t
    |> String.trim()
    |> String.downcase()
    |> case do
      "warm" -> :warm
      "cool" -> :cool
      "deescalate" -> :deescalate
      "de-escalate" -> :deescalate
      "de_escalate" -> :deescalate
      "neutral" -> :neutral
      _ -> nil
    end
  end

  defp normalize_tone(_), do: nil
end
