defmodule Core.Intent.Selection do
  @moduledoc """
  Lightweight intent selector with cue-based confidence.

  Returns SI with :intent, :keyword, :confidence.
  Appends {:intent, %{...}} to si.trace.
  Emits [:core,:intent,:selected] and [:brain,:intent,:selected].
  """

  @type si :: map()
@type intent :: :greet | :translate | :abuse | :insult | :command | :ask | :unknown


  @spec select(si(), Keyword.t()) :: si()
  def select(si, _opts \\ [])

  def select(%{sentence: _} = si, _opts) do
    kw0 = extract_keyword(si)
    kw  = normalize_text(kw0)

    {intent, conf} = infer_intent(kw)

    text0 = text_from_si(si, kw)
    text  = normalize_text(text0)

    si2 =
      si
      |> Map.put(:intent, intent)
      |> Map.put(:keyword, kw)
      |> Map.put(:confidence, conf)
      |> Map.update(:trace, [], &[{:intent, %{keyword: kw, intent: intent, confidence: conf}} | &1])

    emit(intent, kw, conf, text)
    si2
  end

  def select(si, _opts), do: si

  # ─────────────────────────── emit ───────────────────────────

  defp emit(intent, kw, conf, text)
       when is_atom(intent) and is_binary(kw) and is_number(conf) and is_binary(text) do
    meas = %{confidence: conf}
    meta = %{intent: intent, keyword: kw, text: text, source: :core}

    _ =
      try do
        Brain.set_latest_intent(%{intent: intent, keyword: kw, confidence: conf})
      catch
        _, _ -> :ok
      end

    if function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute([:core, :intent, :selected],  meas, meta)
      :telemetry.execute([:brain, :intent, :selected], meas, meta)
    end

    :ok
  end
  defp emit(_, _, _, _), do: :ok

  # ─────────────────────── normalization ───────────────────────

  defp extract_keyword(%{keyword: kw}) when is_binary(kw) and kw != "" do
    kw |> String.trim() |> squish() |> String.downcase()
  end

  defp extract_keyword(%{tokens: tokens}) when is_list(tokens) do
    tokens
    |> Enum.map(fn t -> t[:phrase] || t["phrase"] || "" end)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&squish/1)
    |> Enum.uniq()
    |> prefer_multiword_keyword()
  end

  defp extract_keyword(%{sentence: s}) when is_binary(s),
    do: s |> String.trim() |> squish() |> String.downcase()

  defp extract_keyword(_), do: ""

  defp text_from_si(%{sentence: s}, _kw) when is_binary(s) and s != "", do: s
  defp text_from_si(%{text: s}, _kw) when is_binary(s) and s != "", do: s
  defp text_from_si(%{tokens: tokens}, kw) when is_list(tokens) do
    joined =
      tokens
      |> Enum.map(fn t -> t[:phrase] || t["phrase"] || "" end)
      |> Enum.reject(&(&1 == ""))
      |> Enum.join(" ")
      |> String.trim()

    if joined == "", do: kw || "", else: joined
  end
  defp text_from_si(_si, kw), do: kw || ""

  defp normalize_text(nil), do: ""
  defp normalize_text(s) when is_binary(s) do
    s
    |> String.downcase()
    |> squish()
    |> String.replace(~r/([!?.,])\1+/u, "\\1")         # collapse !!!, ???, ...
    |> String.replace(~r/([a-z])\1{2,}/u, "\\1\\1")    # loooove -> loove
    |> String.trim(".!,? ")
  end
  defp normalize_text(other), do: other |> to_string() |> normalize_text()

  defp squish(s), do: s |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp prefer_multiword_keyword([]), do: ""
  defp prefer_multiword_keyword(phrases) do
    phrases
    |> Enum.sort_by(fn p -> {word_count(p), String.length(p)} end, :desc)
    |> List.first()
    |> String.downcase()
  end
  defp word_count(p), do: length(String.split(p, ~r/\s+/, trim: true))

  # ──────────────────── cue-based inference ────────────────────

# precedence for near ties (highest wins first)
@precedence [:greet, :translate, :abuse, :insult, :command, :ask]


  defp infer_intent(kw) when kw in ["", nil], do: {:unknown, 0.0}
  defp infer_intent(kw) do
scores = %{
  greet:     score_greet(kw),
  translate: score_translate(kw),
  abuse:     score_abuse(kw),
  insult:    score_insult(kw),
  command:   score_command(kw),   # NEW
  ask:       score_question(kw)
}

    {label, top, second} = pick_label(scores)

    # gate :ask if not question-shaped (defensive)
    label =
      case label do
        :ask -> if looks_like_question?(kw), do: :ask, else: :unknown
        other -> other
      end

    conf = conf_from_scores(top, second)

    # if everything is extremely weak, allow unknown
    if top < 0.35, do: {:unknown, 0.40}, else: {label, conf}
  end

  defp pick_label(scores) do
    sorted =
      scores
      |> Enum.sort_by(fn {_k, v} -> v end, :desc)

    [{best_label, best} | rest] = sorted
    second = case rest do
      [] -> 0.0
      [{_, v2} | _] -> v2
    end

    # resolve near ties by precedence
    near_ties =
      sorted
      |> Enum.filter(fn {_k, v} -> abs(v - best) <= 0.05 end)
      |> Enum.map(&elem(&1, 0))

    label =
      if length(near_ties) > 1 do
        Enum.find(@precedence, fn p -> p in near_ties end) || best_label
      else
        best_label
      end

    {label, best, second}
  end

  defp conf_from_scores(top, second) do
    margin = max(top - second, 0.0)
    # emphasize both absolute evidence (top) and separation (margin)
    conf = 0.65 * top + 0.35 * margin
    if conf > 1.0, do: 1.0, else: conf
  end

  # ─────────────── cue scorers (0.0 .. 1.0) ───────────────
# Imperatives like "please build...", "send me...", "fix this", "open ..."
defp score_command(s) do
  qmark = String.contains?(s, "?")

  # Do NOT treat explicit "translate ..." as a command; let :translate win cleanly.
  if Regex.match?(~r/\btranslate\b/i, s) do
    0.0
  else
    strong =
      Regex.match?(
        ~r/^\s*(?:please\s+)?(?:add|create|make|show|open|close|run|build|deploy|install|remove|delete|fix|update|set|write|rename|refactor|generate|explain|summarize|send|tell|give)\b/i,
        s
      ) ||
      Regex.match?(~r/^\s*(?:give me|send me|tell me)\b/i, s)

    polite = Regex.match?(~r/\bplease\b/i, s)

    cond do
      strong and not qmark -> 0.85
      polite and not qmark -> 0.70
      true -> 0.0
    end
  end
end


  # elongated greetings (heeellooo, heyyy, hiii, yooo, gm, good morning/…)
defp score_greet(s) do
  base  = if Regex.match?(greet_rx(), s), do: 0.70, else: 0.0
  extra = if base > 0.0 and String.contains?(s, "!"), do: 0.10, else: 0.0
  min(1.0, base + extra)
end

defp greet_rx do
  # Start-of-string greetings with elongations.
  # - 'yo' must NOT be followed by an apostrophe (so it won't match "you're")
  # - require a word boundary or punctuation/end after the greeting token
  ~r/
    ^
    (?:
      h+e+l{1,2}o+(?:\b|[!.?]|$) |
      he+y+(?:\b|[!.?]|$)        |
      hi+(?:\b|[!.?]|$)          |
      yo+(?!')(?:\b|[!.?]|$)     |
      gm\b |
      good\s+(?:morning|afternoon|evening)\b
    )
  /ix
end

  defp score_translate(s) do
    k1 = Regex.match?(~r/\btranslate\b/i, s)
    k2 = Regex.match?(~r/\b(?:to|into)\s+(english|spanish|french|german|italian|portuguese|chinese|japanese|korean|arabic|hindi)\b/i, s)
    k3 = Regex.match?(~r/\bwhat(?:'s| is)\s+.+?\s+in\s+(english|spanish|french|german|italian|portuguese|chinese|japanese|korean|arabic|hindi)\b/i, s)

    cond do
      k3 and k1    -> 0.95
      k3 or (k1 && k2) -> 0.85
      k1           -> 0.60
      true         -> 0.0
    end
  end

  # strong phrase → highest, word-only → medium-high
defp score_abuse(s) do
  phrase_hit? = Enum.any?(abuse_phrase_regexes(), &Regex.match?(&1, s))
  word_hit?   = Regex.match?(compiled_word_regex(abuse_words()), s) or
                case env_abuse_regex() do
                  nil -> false
                  rx  -> Regex.match?(rx, s)
                end

  cond do
    phrase_hit? -> 0.98  # was 0.95
    word_hit?   -> 0.70  # mild words-only hit slightly lower
    true        -> 0.0
  end
end

  defp score_insult(s) do
    pattern_hit? =
      Regex.match?(~r/\b(you\s+are|you're|ur)\s+(a\s+)?(#{words_alt(insult_words())})\b/i, s)

    word_hit?   = Regex.match?(compiled_word_regex(insult_words()), s) or
                  case env_insult_regex() do
                    nil -> false
                    rx -> Regex.match?(rx, s)
                  end

    cond do
      pattern_hit? -> 0.90
      word_hit?    -> 0.70
      true         -> 0.0
    end
  end

defp score_question(s) do
  qm = String.contains?(s, "?")

  starter = Regex.match?(
    ~r/^\s*(who|what|when|where|why|how|do|does|did|can|could|will|would|should|is|are|am|have|has|had|may|might|was|were)\b/i,
    s
  )

  greet = Regex.match?(greet_rx(), s)

  cond do
    # real questions: strong
    qm and starter -> 0.90
    starter        -> 0.70

    # expressive greeting like "hello?!" or "heyy??" — treat as greeting, not question
    qm and greet   -> 0.20

    # lone question mark with no interrogative — mild signal
    qm             -> 0.55
    true           -> 0.0
  end
end

  defp looks_like_question?(s), do: score_question(s) >= 0.70

  # ─────────────── word lists / config helpers ───────────────

  defp abuse_phrase_regexes do
    [
      ~r/\b(fuck\s+you|f\W*\s*u)\b/i,
      ~r/\b(fuck\s*off)\b/i,
      ~r/\b(screw\s+you)\b/i,
      ~r/\b(piss\s*off)\b/i,
      ~r/\b(go\s+to\s+hell)\b/i
    ]
  end

  defp abuse_words,  do: ~w(asshole bitch bastard dickhead motherfucker shithead cocksucker retard retarded)
  defp insult_words, do: ~w(idiot stupid dumb moron loser pathetic jerk clown trash garbage worthless useless brainless)

  defp compiled_word_regex(words) when is_list(words) and words != [] do
    Regex.compile!("\\b(" <> Enum.map_join(words, "|", &Regex.escape/1) <> ")\\b", "i")
  end
  defp compiled_word_regex(_), do: ~r/(?!)/

  defp words_alt(words), do: Enum.map_join(words, "|", &Regex.escape/1)

  defp env_abuse_regex() do
    terms = Application.get_env(:core, __MODULE__, [])[:abuse_terms] || []
    if terms == [], do: nil, else: compiled_word_regex(terms)
  end

  defp env_insult_regex() do
    terms = Application.get_env(:core, __MODULE__, [])[:insult_terms] || []
    if terms == [], do: nil, else: compiled_word_regex(terms)
  end
end

