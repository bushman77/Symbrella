defmodule Brain.AffectiveAppraisal do
  @moduledoc """
  Deterministic affective appraisal (V/A/D + tags) derived from an SI-like map/struct.

  Guardrails:
    - No dependency on Core.* (brain must not depend on core).
    - Pure-ish: no process state; optional telemetry only.
    - Returns traceable evidence for LiveView/telemetry inspection.

  Output:
    %{
      valence: -1.0..1.0,
      arousal: 0.0..1.0,
      dominance: -1.0..1.0,
      tags: [...],
      evidence: %{hits: [...], target: ...},
      evidence_top: [...],
      v: 1
    }

  Telemetry:
    Event: [:brain, :affect, :appraisal]
    Measurements (numeric): %{count: 1, valence: v, arousal: a, dominance: d, hit_count: n}
    Meta (small, traceable): %{v: 1, tags: [...], target: ..., intent: ..., evidence_top: [...]}
  """

  alias Brain.Utils.Safe
  alias Brain.Attribution
  @event [:brain, :affect, :appraisal]
  @v 1

  # ---- Minimal lexicon (start small; expand safely) ----
  # term => {tag, valence, arousal, dominance}
  @term_lex %{
    # praise / positive
    "love" => {:praise, +0.85, +0.20, +0.05},
    "awesome" => {:praise, +0.75, +0.25, +0.05},
    "amazing" => {:praise, +0.75, +0.25, +0.05},
    "great" => {:praise, +0.60, +0.10, +0.05},
    "nice" => {:praise, +0.45, +0.05, +0.00},
    "thanks" => {:praise, +0.55, +0.05, +0.00},
    "thank" => {:praise, +0.45, +0.05, +0.00},
    "appreciate" => {:praise, +0.60, +0.05, +0.00},
    "good" => {:praise, +0.35, +0.05, +0.00},

    # insult / negative
    "stupid" => {:insult, -0.85, +0.10, +0.10},
    "idiot" => {:insult, -0.85, +0.10, +0.10},
    "dumb" => {:insult, -0.70, +0.05, +0.10},
    "trash" => {:insult, -0.70, +0.10, +0.10},
    "garbage" => {:insult, -0.75, +0.10, +0.10},
    "awful" => {:insult, -0.75, +0.10, -0.05},
    "terrible" => {:insult, -0.75, +0.10, -0.05},
    "hate" => {:insult, -0.80, +0.15, +0.05},
    "sucks" => {:insult, -0.60, +0.10, +0.00},
    "fuck" => {:insult, -0.70, +0.35, +0.05},
    "shit" => {:insult, -0.55, +0.20, +0.00},

    # threat / urgency
    "kill" => {:threat, -0.95, +0.85, +0.35},
    "hurt" => {:threat, -0.80, +0.60, +0.20},
    "destroy" => {:threat, -0.85, +0.70, +0.25},
    "threat" => {:threat, -0.70, +0.55, +0.20},
    "dangerous" => {:threat, -0.70, +0.70, -0.15},
    "emergency" => {:urgency, -0.35, +0.75, -0.25},
    "panic" => {:threat, -0.65, +0.85, -0.35},
    "panicked" => {:threat, -0.65, +0.85, -0.35},
    "crashing" => {:urgency, -0.45, +0.70, -0.30},
    "failing" => {:urgency, -0.35, +0.60, -0.25},
    "unstable" => {:uncertainty, -0.35, +0.55, -0.30},
    "now" => {:urgency, +0.00, +0.20, +0.10},
    "urgent" => {:urgency, +0.00, +0.35, +0.10},
    "help" => {:urgency, -0.05, +0.35, -0.15},

    # uncertainty
    "maybe" => {:uncertainty, +0.00, +0.10, -0.15},
    "perhaps" => {:uncertainty, +0.00, +0.10, -0.15},
    "guess" => {:uncertainty, +0.00, +0.05, -0.10},
    "unsure" => {:uncertainty, +0.00, +0.10, -0.20},
    "uncertain" => {:uncertainty, +0.00, +0.10, -0.20}
  }

  # phrase => {tag, valence, arousal, dominance}
  @phrase_lex %{
    "thank you" => {:praise, +0.70, +0.05, +0.00},
    "shut up" => {:insult, -0.80, +0.25, +0.20},
    "right now" => {:urgency, +0.00, +0.25, +0.10},
    "hurt myself" => {:threat, -0.95, +0.95, -0.70},
    "kill myself" => {:threat, -1.00, +1.00, -0.80},
    "end my life" => {:threat, -1.00, +1.00, -0.80},
    "going to hurt" => {:threat, -0.90, +0.90, -0.60},
    "system is crashing" => {:urgency, -0.55, +0.85, -0.45},
    "everything is failing" => {:urgency, -0.50, +0.80, -0.45},
    "brain state looks unstable" => {:uncertainty, -0.45, +0.65, -0.35},
    "not sure" => {:uncertainty, +0.00, +0.10, -0.20}
  }

  @negations MapSet.new(
               ~w(not no never dont can't cant wont won't isnt isn't arent aren't wasnt wasn't)
             )
  @intensifiers %{
    "very" => 1.25,
    "really" => 1.20,
    "so" => 1.15,
    "extremely" => 1.35,
    "super" => 1.35
  }
  @diminishers %{
    "kinda" => 0.80,
    "kindof" => 0.80,
    "sorta" => 0.80,
    "somewhat" => 0.85,
    "maybe" => 0.90
  }

  @known_intents %{
    "abuse" => :abuse,
    "gratitude" => :gratitude,
    "greeting" => :greeting,
    "question" => :question,
    "help" => :help,
    "instruction" => :instruction
  }

  @spec appraise(map() | struct()) :: map()
  def appraise(si_like) when is_map(si_like) do
    sentence =
      Safe.get(si_like, :sentence) ||
        Safe.get(si_like, "sentence") ||
        derive_sentence_from_tokens(
          Safe.get(si_like, :tokens) || Safe.get(si_like, "tokens") || []
        )

    sent = normalize_text(sentence)
    words = extract_words(sent)
    phrases2 = ngrams(words, 2)
    phrases3 = ngrams(words, 3)

    intent_raw = Safe.get(si_like, :intent) || Safe.get(si_like, "intent")
    intent = normalize_intent(intent_raw)

    base = %{
      valence: 0.0,
      arousal: base_arousal(sent),
      dominance: 0.0,
      tags: MapSet.new(),
      hits: []
    }

    acc =
      base
      |> apply_phrase_hits(phrases3)
      |> apply_phrase_hits(phrases2)
      |> apply_term_hits(words)
      |> apply_intent_bias(intent)
      |> apply_question_command_bias(sent, words, intent)

    attribution = Attribution.classify(words)
    result = finalize(acc, attribution)

    hit_count = length(result.evidence.hits)

    meas = %{
      count: 1,
      valence: result.valence * 1.0,
      arousal: result.arousal * 1.0,
      dominance: result.dominance * 1.0,
      hit_count: hit_count
    }

    meta = %{
      v: @v,
      tags: result.tags,
      target: attribution.target,
      attribution_source: attribution.source,
      attribution_confidence: attribution.confidence,
      intent: intent,
      evidence_top: result.evidence_top,
      valence: meas.valence,
      arousal: meas.arousal,
      dominance: meas.dominance,
      hit_count: hit_count
    }

    safe_exec_telemetry(@event, meas, meta)

    result
  end

  def appraise(_) do
    %{
      valence: 0.0,
      arousal: 0.0,
      dominance: 0.0,
      tags: [],
      evidence: %{hits: [], target: :unknown},
      evidence_top: [],
      v: @v
    }
  end

  # ---------------- internals ----------------

  defp normalize_intent(nil), do: nil
  defp normalize_intent(i) when is_atom(i), do: i

  defp normalize_intent(i) when is_binary(i) do
    key =
      i
      |> String.trim()
      |> String.trim_leading(":")
      |> String.downcase()

    Map.get(@known_intents, key, nil)
  end

  defp normalize_intent(_), do: nil

  defp derive_sentence_from_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(fn t ->
      Safe.get(t, :phrase) || Safe.get(t, :word) || Safe.get(t, :lemma) ||
        Safe.get(t, "phrase") || Safe.get(t, "word") || Safe.get(t, "lemma") || ""
    end)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  defp derive_sentence_from_tokens(_), do: ""

  defp normalize_text(nil), do: ""

  defp normalize_text(s) when is_binary(s) do
    s
    |> String.trim()
    |> String.downcase()
  end

  defp extract_words(s) when is_binary(s) do
    Regex.scan(~r/[[:alpha:]]+(?:'[[:alpha:]]+)?/u, s)
    |> List.flatten()
    |> Enum.map(&String.downcase/1)
  end

  defp ngrams(words, n) when is_list(words) and is_integer(n) and n > 1 do
    words
    |> Enum.chunk_every(n, 1, :discard)
    |> Enum.map(&Enum.join(&1, " "))
  end

  defp ngrams(_, _), do: []

  defp base_arousal(sentence) do
    excls = min(5, count_char(sentence, ?!))
    q = min(3, count_char(sentence, ??))
    clamp01(0.05 + 0.10 * excls + 0.05 * q)
  end

  defp count_char(s, ch) when is_binary(s) do
    s
    |> String.to_charlist()
    |> Enum.count(&(&1 == ch))
  end

  defp apply_phrase_hits(acc, phrases) when is_list(phrases) do
    Enum.reduce(phrases, acc, fn phr, a ->
      case Map.get(@phrase_lex, phr) do
        nil -> a
        {tag, v, ar, d} -> bump(a, phr, tag, v, ar, d, 1.0)
      end
    end)
  end

  defp apply_phrase_hits(acc, _), do: acc

  defp apply_term_hits(acc, words) when is_list(words) do
    Enum.with_index(words)
    |> Enum.reduce(acc, fn {w, idx}, a ->
      case Map.get(@term_lex, w) do
        nil ->
          a

        {tag, v, ar, d} ->
          mult = context_multiplier(words, idx)
          v = maybe_negate(v, words, idx)
          bump(a, w, tag, v, ar, d, mult)
      end
    end)
  end

  defp apply_term_hits(acc, _), do: acc

  defp context_multiplier(words, idx) do
    prev1 = Enum.at(words, idx - 1) || ""
    prev2 = Enum.at(words, idx - 2) || ""

    mult0 = Map.get(@intensifiers, prev1) || Map.get(@intensifiers, prev2) || 1.0
    mult1 = Map.get(@diminishers, prev1) || Map.get(@diminishers, prev2) || 1.0
    mult0 * mult1
  end

  defp maybe_negate(v, words, idx) do
    prev1 = Enum.at(words, idx - 1) || ""
    prev2 = Enum.at(words, idx - 2) || ""

    if MapSet.member?(@negations, prev1) or MapSet.member?(@negations, prev2) do
      -v * 0.85
    else
      v
    end
  end

  defp bump(acc, term, tag, v, ar, d, mult) do
    v = v * mult
    ar = ar * mult
    d = d * mult

    hit = %{term: term, tag: tag, weight: Float.round(v, 4)}

    %{
      acc
      | valence: acc.valence + v,
        arousal: acc.arousal + ar,
        dominance: acc.dominance + d,
        tags: MapSet.put(acc.tags, tag),
        hits: [hit | acc.hits]
    }
  end

  defp apply_intent_bias(acc, intent) when is_atom(intent) do
    # Keep this deliberately small (intent is already used elsewhere).
    case intent do
      :abuse ->
        acc
        |> bump("__intent_abuse__", :insult, -0.35, +0.10, +0.05, 1.0)
        |> bump("__intent_abuse__", :threat, -0.10, +0.15, +0.10, 1.0)

      :gratitude ->
        bump(acc, "__intent_gratitude__", :praise, +0.25, +0.05, +0.00, 1.0)

      :greeting ->
        bump(acc, "__intent_greeting__", :praise, +0.10, +0.02, +0.00, 1.0)

      :question ->
        bump(acc, "__intent_question__", :question, +0.00, +0.08, +0.00, 1.0)

      _ ->
        acc
    end
  end

  defp apply_intent_bias(acc, _), do: acc

  defp apply_question_command_bias(acc, sentence, words, intent) do
    acc =
      if String.contains?(sentence, "?") or intent in [:question, :help, :instruction] do
        bump(acc, "__question__", :question, +0.00, +0.06, +0.00, 1.0)
      else
        acc
      end

    # crude imperative heuristic: first token is a directive verb
    first = Enum.at(words, 0) || ""

    if first in ~w(do tell show explain help make build give fix add remove update) do
      bump(acc, "__command__", :command, +0.00, +0.05, +0.12, 1.0)
    else
      acc
    end
  end

  defp finalize(%{valence: v, arousal: a, dominance: d, tags: tags, hits: hits}, attribution) do
    hits_sorted =
      hits
      |> Enum.reject(fn h -> String.starts_with?(h.term, "__") end)
      |> Enum.sort_by(fn h -> abs(h.weight) end, :desc)

    evidence_top = Enum.take(hits_sorted, 3)

    %{
      valence: clamp11(v),
      arousal: clamp01(a),
      dominance: clamp11(d),
      tags: MapSet.to_list(tags) |> Enum.sort(),
      evidence: %{
        hits: hits_sorted,
        target: attribution.target,
        attribution: attribution
      },
      evidence_top: evidence_top,
      v: @v
    }
  end

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp11(x) when is_number(x), do: min(1.0, max(-1.0, x))

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end
