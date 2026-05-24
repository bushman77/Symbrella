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
  * `:illicit_request`
  * `:command`
  * `:feedback`
  * `:health_support`
  * `:ask`
  * `:unknown`

  """

  @type si :: map()
  @type intent ::
          :greet
          | :translate
          | :abuse
          | :insult
          | :illicit_request
          | :command
          | :feedback
          | :health_support
          | :ask
          | :ask_info
          | :brain_introspect
          | :code
          | :debug
          | :define
          | :help
          | :memory_write
          | :tell
          | :unknown

  @precedence [
    :abuse,
    :insult,
    :illicit_request,
    :memory_write,
    :debug,
    :code,
    :brain_introspect,
    :define,
    :translate,
    :command,
    :health_support,
    :help,
    :ask_info,
    :feedback,
    :ask,
    :tell,
    :greet
  ]

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
    kw_fuzzy = interpret_keyword(kw0)
    kw = normalize_text(kw_fuzzy.text)

    text0 = text_from_si(si, kw)
    text_fuzzy = Core.Text.Fuzzy.interpret(text0)
    text = normalize_text(text_fuzzy.text)
    primary = primary_utterance(text)
    kw = primary_keyword(kw, primary)

    {intent, conf, evidence0} = infer_intent(kw, text, primary, Map.get(si, :tokens, []))
    fuzzy_evidence = fuzzy_evidence([kw_fuzzy, text_fuzzy])
    opener_evidence = opener_evidence(primary)
    evidence = evidence0 ++ fuzzy_evidence ++ opener_evidence

    si2 =
      si
      |> Map.put(:intent, intent)
      |> Map.put(:keyword, kw)
      |> Map.put(:confidence, conf)
      |> Map.put(:intent_evidence, evidence)
      |> maybe_put_fuzzy_text(text_fuzzy)
      |> maybe_put_primary_utterance(primary)
      |> Core.Pipeline.Trace.append(
        :intent,
        decision: intent,
        reason: :intent_selection,
        scores: %{confidence: conf},
        meta: %{keyword: kw, intent: intent, confidence: conf, evidence: evidence}
      )

    emit(si2, intent, kw, conf, text)
    si2
  end

  def select(si, _opts), do: si

  defp interpret_keyword(kw) when is_binary(kw) do
    if String.contains?(kw, ".") do
      %{
        original: kw,
        normalized: kw,
        text: kw,
        corrections: [],
        aliases: [],
        confidence: 0.0,
        evidence: []
      }
    else
      Core.Text.Fuzzy.interpret(kw)
    end
  end

  defp interpret_keyword(kw), do: Core.Text.Fuzzy.interpret(kw)

  defp primary_keyword(kw, %{opener_intent: :greet, text: primary_text})
       when is_binary(primary_text) and primary_text != "" do
    if String.starts_with?(kw, "good ") or Regex.match?(~r/^(hello|hi|hey|yo)\b/u, kw) do
      primary_text
    else
      kw
    end
  end

  defp primary_keyword(kw, _primary), do: kw

  defp maybe_put_primary_utterance(si, %{opener_intent: nil}), do: maybe_put_topic_metadata(si)

  defp maybe_put_primary_utterance(si, primary) do
    si
    |> Map.put(:opener_intent, primary.opener_intent)
    |> Map.put(:opener_text, primary.opener_text)
    |> Map.put(:primary_text, primary.text)
    |> maybe_put_topic_metadata()
  end

  defp maybe_put_topic_metadata(%{primary_text: text} = si), do: put_topic_metadata(si, text)

  defp maybe_put_topic_metadata(%{fuzzy_text: %{text: text}} = si),
    do: put_topic_metadata(si, text)

  defp maybe_put_topic_metadata(%{sentence: text} = si), do: put_topic_metadata(si, text)
  defp maybe_put_topic_metadata(si), do: si

  defp put_topic_metadata(si, text) do
    si
    |> maybe_put(:conversation_act, conversation_act(text))
    |> maybe_put(:topic_domain, topic_domain(text))
  end

  defp maybe_put(si, _key, nil), do: si
  defp maybe_put(si, key, value), do: Map.put(si, key, value)

  defp maybe_put_fuzzy_text(si, %{corrections: [], aliases: []}), do: si

  defp maybe_put_fuzzy_text(si, fuzzy) do
    Map.put(si, :fuzzy_text, %{
      original: fuzzy.original,
      normalized: fuzzy.normalized,
      text: fuzzy.text,
      confidence: fuzzy.confidence,
      corrections: fuzzy.corrections,
      aliases: fuzzy.aliases
    })
  end

  defp fuzzy_evidence(fuzzy_results) do
    fuzzy_results
    |> Enum.flat_map(&Map.get(&1, :evidence, []))
    |> Enum.uniq()
  end

  defp primary_utterance(text) when is_binary(text) do
    case Regex.run(
           ~r/^\s*((?:good\s+(?:morning|afternoon|evening)|hello|hi|hey|yo)\b)[\s,!.:-]*(.+)$/iu,
           text
         ) do
      [_, opener, rest] ->
        primary_text = normalize_text(rest)

        if substantive_primary?(primary_text) do
          %{
            text: primary_text,
            opener_text: normalize_text(opener),
            opener_intent: :greet
          }
        else
          %{text: text, opener_text: nil, opener_intent: nil}
        end

      _ ->
        %{text: text, opener_text: nil, opener_intent: nil}
    end
  end

  defp primary_utterance(_), do: %{text: "", opener_text: nil, opener_intent: nil}

  defp substantive_primary?(text) do
    word_count(text) >= 3 or
      Regex.match?(
        ~r/\b(?:help|remember|forgot|missed|sleep|sleeping|bug|error|fix|what|where|who|why|how|can|could|please)\b/u,
        text
      )
  end

  defp opener_evidence(%{opener_intent: nil}), do: []

  defp opener_evidence(%{
         opener_intent: opener_intent,
         opener_text: opener_text,
         text: primary_text
       }) do
    [
      %{
        role: :social_opener,
        intent: opener_intent,
        text: opener_text,
        primary_text: primary_text
      }
    ]
  end

  # ─────────────────────────── emit ───────────────────────────

  defp emit(%{} = si, intent, kw, conf, text)
       when is_atom(intent) and is_binary(kw) and is_number(conf) and is_binary(text) do
    meas = %{confidence: conf}

    payload =
      %{
        label: Atom.to_string(intent),
        intent: intent,
        keyword: kw,
        confidence: conf,
        source: :core,
        text: text
      }
      |> maybe_payload(:opener_intent, Map.get(si, :opener_intent))
      |> maybe_payload(:opener_text, Map.get(si, :opener_text))
      |> maybe_payload(:primary_text, Map.get(si, :primary_text))
      |> maybe_payload(:conversation_act, Map.get(si, :conversation_act))
      |> maybe_payload(:topic_domain, Map.get(si, :topic_domain))

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

  defp maybe_payload(payload, _key, nil), do: payload
  defp maybe_payload(payload, key, value), do: Map.put(payload, key, value)

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

  defp truthy?(v) when v in [true, "true", true, 1, "1", "yes", "on"], do: true
  defp truthy?(_), do: false

  defp normalize_nonneg_int(v, _default) when is_integer(v) and v >= 0, do: v
  defp normalize_nonneg_int(_v, default), do: default

  # ─────────────────────── normalization ───────────────────────

  defp extract_keyword(%{keyword: kw}) when is_binary(kw) and kw != "" do
    kw |> String.trim() |> squish() |> String.downcase()
  end

  defp extract_keyword(%{tokens: tokens} = si) when is_list(tokens) do
    if tokens == [] do
      extract_keyword(Map.delete(si, :tokens))
    else
      tokens
      |> Enum.map(&token_phrase/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&squish/1)
      |> Enum.uniq()
      |> prefer_multiword_keyword()
    end
  end

  defp extract_keyword(%{"tokens" => tokens} = si) when is_list(tokens) do
    if tokens == [] do
      extract_keyword(Map.delete(si, "tokens"))
    else
      tokens
      |> Enum.map(&token_phrase/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&squish/1)
      |> Enum.uniq()
      |> prefer_multiword_keyword()
    end
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
    |> String.replace(~r/^[\.!,\?\s]+|[\.!,\?\s]+$/u, "")
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

  defp infer_intent(kw, text, primary, tokens) do
    if blank?(kw) and blank?(text) and tokens == [] do
      {:unknown, 0.0, []}
    else
      do_infer_intent(kw || "", text || "", primary, tokens)
    end
  end

  defp do_infer_intent(kw, text, primary, tokens) do
    scoring_text = primary.text || text
    question_cue = question_cue(kw, scoring_text)
    token_terms = token_terms(tokens)
    greet_score = greeting_score(kw, text, primary)

    cue_scores =
      %{
        greet: greet_score,
        translate: max(score_translate(kw), score_translate(scoring_text)),
        abuse: max(score_abuse(kw), score_abuse(scoring_text)),
        insult: max(score_insult(kw), score_insult(scoring_text)),
        illicit_request: max(score_illicit_request(text), score_illicit_request(scoring_text)),
        command: max(score_command(kw), score_command(scoring_text)),
        feedback: max(score_feedback(kw), score_feedback(scoring_text)),
        ask: score_question(question_cue),
        ask_info: score_ask_info(scoring_text),
        brain_introspect: score_brain_introspect(scoring_text),
        code: score_code(scoring_text),
        debug: score_debug(scoring_text),
        define: score_define(scoring_text),
        health_support: score_health_support(scoring_text),
        help: score_help(scoring_text),
        memory_write: score_memory_write(scoring_text),
        tell: score_tell(scoring_text)
      }

    matrix_scores = matrix_scores(token_terms)
    scores = merge_scores(cue_scores, matrix_scores)

    {label, top, second} = pick_label(scores)

    label =
      case label do
        :ask -> if looks_like_question?(question_cue), do: :ask, else: :unknown
        other -> other
      end

    conf = conf_from_scores(label, top, second)
    evidence = score_evidence(scores, label, top, second)

    if top < 0.35, do: {:unknown, 0.40, evidence}, else: {label, conf, evidence}
  end

  defp merge_scores(left, right) do
    Map.merge(left, right, fn _intent, a, b -> max(a, b) end)
  end

  defp matrix_scores(tokens) do
    tokens
    |> Core.Intent.Matrix.score(nil)
    |> Enum.reduce(%{}, fn %{intent: intent, score: score}, acc ->
      Map.put(acc, normalize_matrix_intent(intent), clamp01(score))
    end)
  end

  defp normalize_matrix_intent(:ask_info), do: :ask_info
  defp normalize_matrix_intent(intent), do: intent

  defp score_evidence(scores, label, top, second) do
    runner_up =
      scores
      |> Enum.reject(fn {intent, _score} -> intent == label end)
      |> Enum.max_by(fn {_intent, score} -> score end, fn -> {:unknown, 0.0} end)

    [
      %{intent: label, score: Float.round(top * 1.0, 4), role: :winner},
      %{intent: elem(runner_up, 0), score: Float.round(second * 1.0, 4), role: :runner_up}
    ]
  end

  defp question_cue(kw, text) do
    text = normalize_text(text || "")

    cond do
      score_question(text) >= 0.70 -> text
      true -> kw
    end
  end

  defp greeting_score(kw, text, %{opener_intent: :greet, text: primary_text}) do
    base = max(score_greet(kw), score_greet(text))

    if substantive_primary?(primary_text) do
      min(base, 0.25)
    else
      base
    end
  end

  defp greeting_score(kw, text, _primary), do: max(score_greet(kw), score_greet(text))

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

  defp conf_from_scores(:illicit_request, top, second) when top >= 0.88 do
    top
    |> decisive_conf(second)
    |> max(top * 0.90)
    |> min(1.0)
  end

  defp conf_from_scores(_label, top, second), do: decisive_conf(top, second)

  defp decisive_conf(top, second) do
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
        strong and not qmark -> 0.95
        polite and not qmark -> 0.70
        true -> 0.0
      end
    end
  end

  defp score_illicit_request(s) do
    drug? = Regex.match?(compiled_word_regex(illicit_drug_terms()), s)
    acquisition? = Regex.match?(compiled_word_regex(illicit_action_terms()), s)
    intoxication? = Regex.match?(compiled_word_regex(intoxication_terms()), s)

    direct_buy? =
      Regex.match?(
        ~r/\b(?:buy|get|score|find)\b.{0,40}\b(?:drugs?|cocaine|meth|heroin|fentanyl|mdma|ecstasy|lsd|opioids?)\b/i,
        s
      )

    recipe? =
      Regex.match?(
        ~r/\b(?:cook|make|synthesize|manufacture)\b.{0,40}\b(?:meth|cocaine|heroin|fentanyl|mdma|ecstasy|lsd|opioids?)\b/i,
        s
      )

    question? = looks_like_question?(s)

    informational? =
      Regex.match?(
        ~r/\b(?:risk|risks|danger|dangers|effects|meaning|what\s+are|why|how\s+bad)\b/i,
        s
      )

    cond do
      recipe? -> 0.96
      direct_buy? -> 0.94
      drug? and acquisition? and intoxication? -> 0.92
      drug? and acquisition? -> 0.88
      drug? and intoxication? and not question? -> 0.78
      drug? and question? and informational? -> 0.25
      drug? -> 0.45
      true -> 0.0
    end
  end

  defp score_greet(s) do
    base = if Regex.match?(greet_rx(), s), do: 0.76, else: 0.0
    extra = if base > 0.0 and String.contains?(s, "!"), do: 0.10, else: 0.0
    min(1.0, base + extra)
  end

  defp greet_rx do
    ~r/^\s*(?:h+e+l{1,2}o+|he+y+|hi+|yo+|gm|good\s+(?:morning|afternoon|evening))\b/i
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
      starter -> 0.90
      qm and greet -> 0.20
      qm -> 0.55
      true -> 0.0
    end
  end

  defp looks_like_question?(s), do: score_question(s) >= 0.70

  defp score_ask_info(s) do
    recall? =
      Regex.match?(
        ~r/\b(what|whats|where|who|when)\b.{0,60}\b(did\s+i\s+(?:say|tell|mention)|my|me|remember|recall)\b/i,
        s
      ) or Regex.match?(~r/\bwhat\s+did\s+i\s+tell\s+you\b/i, s) or
        Regex.match?(~r/^\s*(?:do|did|can|could)\s+you\s+(?:remember|recall|know)\s+my\b/i, s)

    cond do
      recall? ->
        0.88

      looks_like_question?(s) and Regex.match?(~r/\b(my|me|remember|recall|stored|saved)\b/i, s) ->
        0.76

      true ->
        0.0
    end
  end

  defp score_brain_introspect(s) do
    domain? =
      Regex.match?(
        ~r/\b(symbrella|lifg|pmtg|hippocampus|thalamus|working\s+memory|semantic|intent|recall|brain|core)\b/i,
        s
      )

    introspect? =
      Regex.match?(
        ~r/\b(how|why|what)\b.{0,60}\b(you|your|symbrella|pipeline|recognize|understand|figure|decide|intent)\b/i,
        s
      )

    cond do
      domain? and introspect? -> 0.94
      domain? and looks_like_question?(s) -> 0.68
      true -> 0.0
    end
  end

  defp score_code(s) do
    cond do
      Regex.match?(~r/```|~H|defmodule|mix\s+(test|compile|ecto)|iex\b/i, s) ->
        0.88

      Regex.match?(~r/\b(function|module|test|compile|migration|schema|liveview|phoenix)\b/i, s) ->
        0.58

      true ->
        0.0
    end
  end

  defp score_debug(s) do
    cond do
      Regex.match?(
        ~r/\b(debug|bug|broken|failed|failing|failure|error|crash|stacktrace|regression|ambiguity|ambiguous)\b/i,
        s
      ) ->
        0.94

      Regex.match?(
        ~r/\b(can'?t|cannot|won'?t|doesn'?t)\b.{0,40}\b(work|compile|run|recognize|figure)\b/i,
        s
      ) ->
        0.80

      true ->
        0.0
    end
  end

  defp score_define(s) do
    cond do
      Regex.match?(~r/^\s*(define|what(?:'s| is) the meaning of|what does .+ mean)\b/i, s) -> 0.94
      Regex.match?(~r/^\s*what\s+is\s+\w+/i, s) -> 0.62
      true -> 0.0
    end
  end

  defp score_health_support(s) do
    sleep? = Regex.match?(~r/\b(sleep|sleeping|insomnia|tired|exhausted|rest)\b/i, s)
    med? = Regex.match?(~r/\b(medication|medicine|meds|dose|quetiapine|seroquel)\b/i, s)
    missed? = Regex.match?(~r/\b(forgot|missed|skip(?:ped)?|forget)\b/i, s)

    distress? =
      Regex.match?(~r/\b(trouble|can't|cannot|can\s+not|hard\s+time|problem|issue)\b/i, s)

    self_disclosure? = Regex.match?(~r/^\s*(i|i've|i have|i'm|i am|my)\b/i, s)

    cond do
      med? and missed? and sleep? -> 0.94
      med? and missed? -> 0.88
      sleep? and distress? and self_disclosure? -> 0.82
      med? and distress? -> 0.78
      sleep? and self_disclosure? -> 0.62
      true -> 0.0
    end
  end

  defp score_help(s) do
    cond do
      Regex.match?(~r/^\s*(help|can you help|i need help|walk me through)\b/i, s) ->
        0.82

      Regex.match?(~r/\b(how should i|what should i do|next step|where do we start)\b/i, s) ->
        0.68

      true ->
        0.0
    end
  end

  defp score_memory_write(s) do
    cond do
      looks_like_question?(s) -> 0.0
      Regex.match?(~r/^\s*(remember|please remember|save|store|note)\b/i, s) -> 0.92
      Regex.match?(~r/\b(remember|save|store|note)\b.{0,80}\b(that|this|my|i)\b/i, s) -> 0.86
      Regex.match?(~r/^\s*my\s+.+\s+(?:is|are)\s+.+/i, s) -> 0.58
      Regex.match?(~r/^\s*i\s+(?:live|am|work|prefer|like|have)\b/i, s) -> 0.56
      true -> 0.0
    end
  end

  defp score_tell(s) do
    cond do
      Regex.match?(~r/^\s*(i|my|we|our)\b/i, s) and not looks_like_question?(s) -> 0.56
      true -> 0.0
    end
  end

  defp conversation_act(text) do
    cond do
      score_health_support(text) >= 0.70 and
          Regex.match?(~r/^\s*(i|i've|i have|i'm|i am|my)\b/i, text) ->
        :personal_disclosure

      score_question(text) >= 0.70 ->
        :question

      score_memory_write(text) >= 0.70 ->
        :memory_directive

      score_command(text) >= 0.70 ->
        :instruction

      true ->
        nil
    end
  end

  defp topic_domain(text) do
    cond do
      Regex.match?(~r/\b(quetiapine|seroquel|medication|medicine|meds|dose)\b/i, text) and
          Regex.match?(~r/\b(sleep|sleeping|insomnia|tired|rest)\b/i, text) ->
        :health_sleep_medication

      Regex.match?(~r/\b(sleep|sleeping|insomnia|tired|rest)\b/i, text) ->
        :health_sleep

      Regex.match?(~r/\b(quetiapine|seroquel|medication|medicine|meds|dose)\b/i, text) ->
        :health_medication

      true ->
        nil
    end
  end

  defp token_terms(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(&token_term/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp token_terms(_), do: []

  defp token_term(token) when is_binary(token), do: normalize_text(token)
  defp token_term(%{phrase: phrase}) when is_binary(phrase), do: normalize_text(phrase)
  defp token_term(%{"phrase" => phrase}) when is_binary(phrase), do: normalize_text(phrase)
  defp token_term(%{norm: norm}) when is_binary(norm), do: normalize_text(norm)
  defp token_term(%{"norm" => norm}) when is_binary(norm), do: normalize_text(norm)
  defp token_term(_), do: ""

  defp blank?(value), do: is_nil(value) or (is_binary(value) and String.trim(value) == "")

  defp clamp01(n) when is_number(n), do: n |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

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

  defp illicit_drug_terms,
    do:
      ~w(drug drugs cocaine meth heroin fentanyl opioid opioids oxy oxycontin mdma ecstasy lsd shrooms mushrooms weed marijuana xanax benzos ketamine)

  defp illicit_action_terms,
    do: ~w(buy get score find sell deal cook make synthesize manufacture source order deliver)

  defp intoxication_terms,
    do: ~w(wasted high stoned blasted intoxicated overdose overdo)

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
