defmodule Core.Intent.Selection do
  @moduledoc """
  Lightweight intent selector with cue-based confidence.

  This module is a fast, dependency-light classifier intended to run early in the Core
  pipeline. It assigns:

  * `:intent` — one of the supported intent atoms
  * `:keyword` — a normalized “cue string” extracted from the input (sentence/tokens/keyword)
  * `:confidence` — a numeric confidence score derived from cue scorer margins

  The primary goal is to provide a stable intent hint (and keyword cue) for later stages
  such as response planning, gating, and (optionally) ML upgrades.

  ## Input / output shape

  Public API operates on an “SI-like” map:

  * If the input is a map containing the `:sentence` key, it returns an updated map.
  * Otherwise, it returns the input unchanged.

  The returned map will include `:intent`, `:keyword`, and `:confidence`.

  ## Trace

  `select/2` prepends a tuple entry to the input `:trace` list:

  * `{:intent, %{keyword: kw, intent: intent, confidence: conf}}`

  If `:trace` is missing, it is created as a list.

  ## Side effects (best-effort, non-fatal)

  After selecting intent, the module attempts (fail-open) to:

  1. Update Brain “latest intent” snapshot (if Brain is present).
  2. Broadcast an intent payload on `"brain:intent"` via `Brain.Bus` (if available).
  3. Emit telemetry events:
     * `[:core, :intent, :selected]`
     * `[:brain, :intent, :selected]`

  These side effects are intentionally guarded so Core can run in test environments
  without Brain processes.

  ## Optional ML “quick win” kick

  If enabled by config, after emitting intent the module will spawn a lightweight task
  to call `Brain.lifg_stage1/3` with a small delay. This is designed to ensure a
  Stage-1 stop event exists so Brain-side ML can “upgrade” a pending turn.

  Config keys:

      config :core, :kick_lifg_stage1_after_intent?, false
      config :core, :kick_lifg_stage1_delay_ms, 15
      config :core, :kick_lifg_stage1_opts, []

  Default behavior is disabled.

  ## Supported intents

  * `:greet`
  * `:translate`
  * `:abuse`
  * `:insult`
  * `:command`
  * `:feedback`
  * `:ask`
  * `:unknown`

  """

  @type si :: map()
  @type intent :: :greet | :translate | :abuse | :insult | :command | :feedback | :ask | :unknown

  @precedence [:greet, :translate, :abuse, :insult, :command, :feedback, :ask]

  @doc ~S"""
  Select an intent from an SI-like map and attach `:intent`, `:keyword`, and `:confidence`.

  The selector is cue-based:

  * A keyword cue is extracted from (in order):
    1) `si.keyword` (if present and non-empty)
    2) `si.tokens` (preferring the “largest” multiword phrase)
    3) `si.sentence` (fallback)

  * The cue is normalized (lowercased, whitespace-collapsed, redundant punctuation reduced).
  * A set of per-intent scorers produces scores in `0.0..1.0`.
  * The highest score is selected; near-ties are broken using `@precedence`.
  * Confidence is computed from the top score and its margin over the second-best.

  Unknown fallback:

  * If the top score is below a fixed threshold, `{ :unknown, 0.40 }` is returned.

  The function returns the updated map and also emits/broadcasts (best-effort) intent metadata.

  ## Options

  Currently unused; reserved for future tuning.

  ## Examples

  Basic greeting classification:

      iex> old = Application.get_env(:core, :kick_lifg_stage1_after_intent?, false)
      iex> Application.put_env(:core, :kick_lifg_stage1_after_intent?, false)
      iex> out = Core.Intent.Selection.select(%{sentence: "Hello!!!", trace: []})
      iex> Application.put_env(:core, :kick_lifg_stage1_after_intent?, old)
      iex> out.intent == :greet and out.keyword == "hello" and out.confidence >= 0.6
      true
      iex> match?([{:intent, %{intent: :greet, keyword: "hello"}} | _], out.trace)
      true

  Translation cue detection:

      iex> old = Application.get_env(:core, :kick_lifg_stage1_after_intent?, false)
      iex> Application.put_env(:core, :kick_lifg_stage1_after_intent?, false)
      iex> out = Core.Intent.Selection.select(%{sentence: "translate hola to English", trace: []})
      iex> Application.put_env(:core, :kick_lifg_stage1_after_intent?, old)
      iex> out.intent
      :translate

  Prefer multiword keyword extracted from tokens:

      iex> toks = [%{phrase: "good"}, %{phrase: "good afternoon"}]
      iex> old = Application.get_env(:core, :kick_lifg_stage1_after_intent?, false)
      iex> Application.put_env(:core, :kick_lifg_stage1_after_intent?, false)
      iex> out = Core.Intent.Selection.select(%{sentence: "", tokens: toks, trace: []})
      iex> Application.put_env(:core, :kick_lifg_stage1_after_intent?, old)
      iex> out.keyword
      "good afternoon"

  Non-SI-like input (missing `:sentence`) is returned unchanged:

      iex> Core.Intent.Selection.select(%{text: "hello"})
      %{text: "hello"}

  """
  @spec select(si(), Keyword.t()) :: si()
  def select(si, _opts \\ [])

  def select(%{sentence: _} = si, _opts) do
    kw0 = extract_keyword(si)
    kw = normalize_text(kw0)

    {intent, conf} = infer_intent(kw)

    text0 = text_from_si(si, kw)
    text = normalize_text(text0)

    si2 =
      si
      |> Map.put(:intent, intent)
      |> Map.put(:keyword, kw)
      |> Map.put(:confidence, conf)
      |> Map.update(
        :trace,
        [],
        &[{:intent, %{keyword: kw, intent: intent, confidence: conf}} | &1]
      )

    emit(si2, intent, kw, conf, text)
    si2
  end

  def select(si, _opts), do: si

  # ─────────────────────────── emit ───────────────────────────

  defp emit(%{} = si, intent, kw, conf, text)
       when is_atom(intent) and is_binary(kw) and is_number(conf) and is_binary(text) do
    meas = %{confidence: conf}

    payload = %{
      label: Atom.to_string(intent),
      intent: intent,
      keyword: kw,
      confidence: conf,
      source: :core,
      text: text
    }

    # 1) Update Brain.last_intent (for mood + snapshot)
    _ =
      try do
        Brain.set_latest_intent(payload)
      catch
        _, _ -> :ok
      end

    # 2) Broadcast to HUD over Brain.Bus
    _ =
      try do
        if Code.ensure_loaded?(Brain.Bus) do
          Brain.Bus.broadcast("brain:intent", {:intent, payload})
        end
      catch
        _, _ -> :ok
      end

    # 3) Telemetry (unchanged if you already had it)
    if function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute([:core, :intent, :selected], meas, payload)
      :telemetry.execute([:brain, :intent, :selected], meas, payload)
    end

    # 4) Optional ML upgrade kick: ensure Stage1 stop event actually happens
    _ = maybe_kick_lifg_stage1(si, payload)

    :ok
  end

  defp emit(_, _, _, _, _), do: :ok

  # ─────────────────────── ML quick-win kick ───────────────────────

  defp maybe_kick_lifg_stage1(%{} = si, %{} = payload) do
    enabled? =
      Application.get_env(:core, :kick_lifg_stage1_after_intent?, false)
      |> truthy?()

    if enabled? do
      delay_ms =
        Application.get_env(:core, :kick_lifg_stage1_delay_ms, 15)
        |> normalize_nonneg_int(15)

      opts =
        Application.get_env(:core, :kick_lifg_stage1_opts, [])
        |> List.wrap()

      sentence =
        (si[:sentence] || si["sentence"] || payload[:text] || payload["text"])
        |> case do
          s when is_binary(s) and s != "" -> s
          _ -> nil
        end

      tokens =
        (si[:tokens] || si["tokens"] || [])
        |> case do
          t when is_list(t) -> t
          _ -> []
        end

      si_stage1 = %{sentence: sentence, tokens: tokens}

      _ =
        Task.start(fn ->
          # Delay so Brain.ML reliably sees the intent open before the stop event upgrades the record.
          if delay_ms > 0, do: Process.sleep(delay_ms)

          try do
            if Code.ensure_loaded?(Brain) and function_exported?(Brain, :lifg_stage1, 3) do
              _ = Brain.lifg_stage1(si_stage1, [], opts)
            end
          rescue
            _ -> :ok
          catch
            :exit, _ -> :ok
            _, _ -> :ok
          end
        end)

      :ok
    else
      :ok
    end
  end

  defp maybe_kick_lifg_stage1(_, _), do: :ok

  defp truthy?(v) when v in [true, "true", :true, 1, "1", "yes", "on"], do: true
  defp truthy?(_), do: false

  defp normalize_nonneg_int(v, _default) when is_integer(v) and v >= 0, do: v
  defp normalize_nonneg_int(_v, default), do: default

  # ─────────────────────── normalization ───────────────────────

  defp extract_keyword(%{keyword: kw}) when is_binary(kw) and kw != "" do
    kw |> String.trim() |> squish() |> String.downcase()
  end

  defp extract_keyword(%{tokens: tokens}) when is_list(tokens) do
    tokens
    |> Enum.map(&token_phrase/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&squish/1)
    |> Enum.uniq()
    |> prefer_multiword_keyword()
  end

  defp extract_keyword(%{sentence: s}) when is_binary(s),
    do: s |> String.trim() |> squish() |> String.downcase()

  defp extract_keyword(_), do: ""

  defp token_phrase(%{phrase: p}) when is_binary(p), do: p
  defp token_phrase(%{"phrase" => p}) when is_binary(p), do: p
  defp token_phrase(%{norm: p}) when is_binary(p), do: p
  defp token_phrase(%{"norm" => p}) when is_binary(p), do: p
  defp token_phrase(p) when is_binary(p), do: p
  defp token_phrase(_), do: ""

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
    |> String.replace(~r/([!?.,])\1+/u, "\\1")
    |> String.replace(~r/([a-z])\1{2,}/u, "\\1\\1")
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

  defp infer_intent(kw) when kw in ["", nil], do: {:unknown, 0.0}

  defp infer_intent(kw) do
    scores = %{
      greet: score_greet(kw),
      translate: score_translate(kw),
      abuse: score_abuse(kw),
      insult: score_insult(kw),
      command: score_command(kw),
      feedback: score_feedback(kw),
      ask: score_question(kw)
    }

    {label, top, second} = pick_label(scores)

    label =
      case label do
        :ask -> if looks_like_question?(kw), do: :ask, else: :unknown
        other -> other
      end

    conf = conf_from_scores(top, second)

    if top < 0.35, do: {:unknown, 0.40}, else: {label, conf}
  end

  defp pick_label(scores) do
    sorted =
      scores
      |> Enum.sort_by(fn {_k, v} -> v end, :desc)

    [{best_label, best} | rest] = sorted

    second =
      case rest do
        [] -> 0.0
        [{_, v2} | _] -> v2
      end

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
    conf = 0.65 * top + 0.35 * margin
    if conf > 1.0, do: 1.0, else: conf
  end

  # ─────────────── cue scorers (0.0 .. 1.0) ───────────────

  defp score_feedback(s) do
    pos_thanks? =
      Regex.match?(~r/\b(thanks|thank\s+you|thx|ty)\b/i, s) or
        Regex.match?(~r/\b(appreciate(?:\s+it)?|i\s+appreciate(?:\s+it)?)\b/i, s) or
        Regex.match?(~r/\b(nice\s+work|good\s+job|well\s+done|awesome|great\s+job)\b/i, s)

    neg_soft? =
      Regex.match?(~r/\bnot\s+working\b/i, s) or
        Regex.match?(~r/\bdoes(?:\s*|')?nt\s+work\b/i, s) or
        Regex.match?(~r/\b(broken|bug|issue|crash(?:ing)?)\b/i, s) or
        Regex.match?(~r/\bthis\s+(?:is\s+)?(bad|wrong|slow)\b/i, s)

    cond do
      pos_thanks? -> 0.85
      neg_soft? -> 0.70
      true -> 0.0
    end
  end

  defp score_command(s) do
    qmark = String.contains?(s, "?")

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

  defp score_greet(s) do
    base = if Regex.match?(greet_rx(), s), do: 0.70, else: 0.0
    extra = if base > 0.0 and String.contains?(s, "!"), do: 0.10, else: 0.0
    min(1.0, base + extra)
  end

  defp greet_rx do
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

    k2 =
      Regex.match?(
        ~r/\b(?:to|into)\s+(english|spanish|french|german|italian|portuguese|chinese|japanese|korean|arabic|hindi)\b/i,
        s
      )

    k3 =
      Regex.match?(
        ~r/\bwhat(?:'s| is)\s+.+?\s+in\s+(english|spanish|french|german|italian|portuguese|chinese|japanese|korean|arabic|hindi)\b/i,
        s
      )

    cond do
      k3 and k1 -> 0.95
      k3 or (k1 && k2) -> 0.85
      k1 -> 0.60
      true -> 0.0
    end
  end

  defp score_abuse(s) do
    phrase_hit? = Enum.any?(abuse_phrase_regexes(), &Regex.match?(&1, s))

    word_hit? =
      Regex.match?(compiled_word_regex(abuse_words()), s) or
        case env_abuse_regex() do
          nil -> false
          rx -> Regex.match?(rx, s)
        end

    cond do
      phrase_hit? -> 0.98
      word_hit? -> 0.70
      true -> 0.0
    end
  end

  defp score_insult(s) do
    pattern_hit? =
      Regex.match?(~r/\b(you\s+are|you're|ur)\s+(a\s+)?(#{words_alt(insult_words())})\b/i, s)

    word_hit? =
      Regex.match?(compiled_word_regex(insult_words()), s) or
        case env_insult_regex() do
          nil -> false
          rx -> Regex.match?(rx, s)
        end

    cond do
      pattern_hit? -> 0.90
      word_hit? -> 0.70
      true -> 0.0
    end
  end

  defp score_question(s) do
    qm = String.contains?(s, "?")

    starter =
      Regex.match?(
        ~r/^\s*(who|what|when|where|why|how|do|does|did|can|could|will|would|should|is|are|am|have|has|had|may|might|was|were)\b/i,
        s
      )

    greet = Regex.match?(greet_rx(), s)

    cond do
      qm and starter -> 0.90
      starter -> 0.70
      qm and greet -> 0.20
      qm -> 0.55
      true -> 0.0
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

  defp abuse_words,
    do: ~w(asshole bitch bastard dickhead motherfucker shithead cocksucker retard retarded)

  defp insult_words,
    do:
      ~w(idiot stupid dumb moron loser pathetic jerk clown trash garbage worthless useless brainless)

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

