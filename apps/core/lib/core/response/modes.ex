defmodule Core.Response.Modes do
  @moduledoc """
  Deterministic fallback text for Core.Response when LLM synthesis is unavailable.
  Terse, contextual, and predictable (no randomness).

  Public API
  ----------
    compose(intent, tone, mode) :: String.t()
    compose(intent, tone, mode, opts) :: String.t()

  Optional `opts` slots (ignored if absent):
    :file_hint    — short path/module to reflect (e.g., "apps/brain/lib/brain/lifg.ex")
    :flag         — top guardrail label (e.g., :lifg_move, :acyclic_violation)
    :next_step    — one-line next step chosen by the planner
    :variant_seed — integer used to deterministically pick among phrasing variants
  """

  alias Core.Response.Topics

  @type intent ::
          :abuse
          | :illicit_request
          | :greeting
          | :greet
          | :gratitude
          | :smalltalk
          | :question
          | :instruction
          | :health_support
          | :help
          | :command
          | :refactor
          | :review
          | :plan
          | :diagram
          | :explain
          | :bug
          | :optimize
          | :benchmark
          | :unknown

  @type tone :: :warm | :neutral | :firm | :deescalate
  @type mode :: :collaborator | :coach | :scribe | :editor | :explainer | :supportive_care

  @type opts :: %{
          optional(:file_hint) => String.t(),
          optional(:flag) => any(),
          optional(:next_step) => String.t(),
          optional(:variant_seed) => non_neg_integer()
        }

  @doc "Backward-compatible 3-arity; delegates to compose/4 with an empty opts map."
  @spec compose(intent, tone, mode) :: String.t()
  def compose(intent, tone, mode), do: compose(intent, tone, mode, %{})

  @spec compose(intent, tone, mode, opts | keyword) :: String.t()
  def compose(:abuse, :deescalate, _mode, _opts),
    do:
      "I'll keep this respectful and useful. Tell me what you want changed in Symbrella and I'll help with that."

  def compose(:abuse, :firm, _mode, _opts),
    do: "Let's keep it constructive. Name the file or task you want changed and I'll proceed."

  def compose(:illicit_request, _tone, _mode, _opts),
    do:
      "I can't help with buying drugs or getting wasted. I can help with safety, health risks, or getting support instead."

  def compose(:health_support, _tone, _mode, _opts),
    do:
      "That sounds rough. If you are unsure what to do after a missed medication dose, check with your pharmacist or prescriber. I can help you set up a reminder plan or think through what to ask them, but I should not tell you how to change the dose."

  def compose(:greeting, _tone, _mode, raw_opts) do
    opts = normalize_opts(raw_opts)

    if personal_life_update_text?(opts[:text]) do
      personal_life_update_fallback(opts[:text])
    else
      greeting_text(opts)
    end
  end

  def compose(:greet, _tone, _mode, raw_opts) do
    opts = normalize_opts(raw_opts)

    if personal_life_update_text?(opts[:text]) do
      personal_life_update_fallback(opts[:text])
    else
      greeting_text(opts)
    end
  end

  def compose(:smalltalk, _tone, _mode, _opts),
    do: "I'm here and ready to talk."

  def compose(:gratitude, _tone, _mode, _opts),
    do: "You're welcome."

  # ── Fallbacks ───────────────────────────────────────────────────────────────

  def compose(_intent, :deescalate, _mode, raw_opts) do
    opts = normalize_opts(raw_opts)

    with_file_hint(
      "No rush. We'll go one piece at a time. What's the single most helpful change right now?",
      opts[:file_hint]
    )
  end

  def compose(_intent, :firm, _mode, raw_opts) do
    opts = normalize_opts(raw_opts)

    with_file_hint(
      "Got it. Send the concrete target or error and I'll keep the next step focused.",
      opts[:file_hint]
    )
  end

  def compose(intent, tone, mode, raw_opts) do
    opts = normalize_opts(raw_opts)
    seed = opts[:variant_seed] || stable_seed(intent, tone, mode, opts)

    base =
      cond do
        personal_finance_text?(opts[:text]) ->
          "That is a reasonable start. Before committing to consolidation, compare the total cost, fees, interest rate, payment amount, timeline, and whether they require you to stop paying creditors. Keep current bills current if you can, and verify anything inaccurate on your credit reports before agreeing to a plan."

        peace_or_war_question_text?(opts[:text]) ->
          peace_or_war_fallback(opts[:text])

        cosmic_life_text?(opts[:text]) ->
          cosmic_life_fallback(opts[:text], opts)

        personal_life_update_text?(opts[:text]) ->
          personal_life_update_fallback(opts[:text])

        casual_or_companion_text?(opts[:text]) ->
          casual_fallback(opts[:text])

        technical_text?(opts[:text]) ->
          technical_fallback_base(seed)

        alien_life_thread_followup?(opts[:text], opts) ->
          alien_life_thread_fallback(opts[:text])

        missing_context_followup?(opts[:text], opts) ->
          missing_context_fallback()

        true ->
          general_fallback_base(seed)
      end

    base
    |> with_next_step(opts[:next_step])
    |> with_file_hint(opts[:file_hint])
  end

  # ── Helpers ─────────────────────────────────────────────────────────────────

  defp with_file_hint(text, nil), do: text

  defp with_file_hint(text, hint) when is_binary(hint) and hint != "" do
    text <> "\n" <> "Relevant target: `#{hint}`."
  end

  defp with_file_hint(text, _), do: text

  defp with_next_step(text, nil), do: text

  defp with_next_step(text, step) when is_binary(step) and step != "" do
    text <> " Suggested next step: " <> step
  end

  defp with_next_step(text, _), do: text

  # Deterministic variant chooser for small phrasing variety; defaults to first.
  defp choose(nil, [first | _]), do: first

  defp choose(seed, list) when is_integer(seed) and seed >= 0 and is_list(list) and list != [] do
    idx = rem(seed, length(list))
    Enum.at(list, idx)
  end

  # ——— internals ———
  defp normalize_opts(opts) when is_list(opts), do: Map.new(opts)
  defp normalize_opts(%{} = opts), do: opts
  defp normalize_opts(_), do: %{}

  defp technical_fallback_base(seed) do
    choose(seed, [
      "I need one concrete target before I can make this useful. Send the module, file, or failing output.",
      "Give me the next concrete target and I'll tailor the response to that context.",
      "I can help from here, but I need the specific module, file, or behavior you want changed.",
      "Point me at the code or error you want handled and I'll respond with the next practical step."
    ])
  end

  defp general_fallback_base(seed) do
    choose(seed, [
      "I may have missed the thread. I’m here with you; say it one more way and I’ll stay with the conversation.",
      "I’m with you. I don’t want to force this into a task; what are you trying to talk through?",
      "I hear you. Let’s keep this conversational instead of turning it into a technical target.",
      "I’m here. I may need the last bit in simpler words, but I’m not trying to redirect you into code."
    ])
  end

  defp personal_life_update_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(my own place|own apartment|own house|new apartment|new place|getting (?:my )?own place|moving out|move into (?:my|our) place|got approved for (?:an apartment|a place)|signed (?:a )?lease)\b/iu,
      text
    )
  end

  defp personal_life_update_text?(_), do: false

  defp personal_life_update_fallback(text) when is_binary(text) do
    if Regex.match?(~r/\bgood\s+afternoon\b/iu, text) do
      "Good afternoon. That sounds like a big step. Getting close to having your own place is exciting, and probably a little intense too. What part is feeling most real right now?"
    else
      "That sounds like a big step. Getting close to having your own place is exciting, and probably a little intense too. What part is feeling most real right now?"
    end
  end

  defp casual_fallback(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(~r/^\s*huh+\??\s*$/u, down) ->
        "Yeah, that came out wrong. I’m here with you."

      Regex.match?(~r/(what'?s|whats|whaats|wats)\s+u+p|c+mon.*up/u, down) ->
        "I’m here with you. On my side it’s just the current Symbrella state and this conversation, but I can still hang out and talk."

      Regex.match?(~r/companion|not.*code|write code|default response/u, down) ->
        "You're right. I should stay in companion mode unless you ask for technical help."

      true ->
        "Hey. I’m here with you."
    end
  end

  defp casual_fallback(_), do: "I’m here with you."

  defp greeting_text(opts) do
    text = String.downcase(to_string(opts[:text] || opts[:user_text] || ""))

    cond do
      String.contains?(text, "good morning") -> "Good morning. I'm here with you."
      String.contains?(text, "good afternoon") -> "Good afternoon. I'm here with you."
      String.contains?(text, "good evening") -> "Good evening. I'm here with you."
      true -> "Hello. I'm here with you."
    end
  end

  defp personal_finance_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(credit|credit karma|debt|debts|consolidat(?:e|ion|ing)|collections?|collector|loan|loans|interest rate|apr|minimum payment|bankruptcy|charge[-\s]?off|delinquen|late payment)\b/iu,
      text
    )
  end

  defp personal_finance_text?(_), do: false

  defp cosmic_life_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(aliens?|extraterrestrial|life\s+elsewhere|universe|galax(?:y|ies)|solar\s+system|planet|planets|exoplanets?|ufos?|uaps?|unidentified\s+(?:flying\s+)?objects?|disclosure|declassif(?:y|ied|ication)|footage)\b/iu,
      text
    )
  end

  defp cosmic_life_text?(_), do: false

  defp cosmic_life_fallback(text, opts) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(
        ~r/\b(ufos?|uaps?|unidentified\s+(?:flying\s+)?objects?|disclosure|declassif(?:y|ied|ication)|footage)\b/u,
        down
      ) ->
        context_bridge(opts) <>
          "UFO or UAP footage can make the question feel more concrete, but footage of something unidentified is not the same as confirmed alien life. I’d separate three layers: whether the footage is real, whether the object is actually unexplained, and whether it points to extraterrestrial life."

      Regex.match?(~r/\bsolar\s+system\b/u, down) and Regex.match?(~r/\buniverse\b/u, down) ->
        "Right, scale changes the intuition. If the universe were only the size of our solar system, no alien life would feel much more plausible. But with billions of galaxies and a huge number of planets, it is reasonable to think life elsewhere could exist, even though we do not have confirmed evidence yet."

      Regex.match?(~r/\baliens?\b/u, down) ->
        "That is the key distinction: alien life is plausible because the universe is enormous, but confirmed evidence is still missing. So the grounded answer is not “definitely yes”; it is “very possible, not yet proven.”"

      true ->
        "The scale of the universe matters a lot here. More stars and planets means more chances for life, but we still have to separate probability from confirmed evidence."
    end
  end

  defp cosmic_life_fallback(_, _), do: "The scale of the universe matters a lot here."

  defp alien_life_thread_followup?(text, opts) when is_binary(text) do
    context_topic?(opts, :alien_life) and Topics.followup?(text, :alien_life)
  end

  defp alien_life_thread_followup?(_, _), do: false

  defp alien_life_thread_fallback(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(~r/\b(elite|admit|cover\s*up|hide|hidden|government|world)\b/u, down) ->
        "That fits the same alien-life thread. I can see why secrecy or institutions would come up in that conversation, but I’d keep it split: alien life being plausible is one question; whether specific powerful groups know and hide proof is a separate claim that needs evidence."

      true ->
        "That fits the same alien-life thread. Alien life can be plausible while specific claims still need evidence, so I’d keep the speculation and the confirmed facts separate."
    end
  end

  defp context_bridge(opts) do
    if context_history_present?(opts) do
      "That fits the same thread: "
    else
      "I do not have prior chat context available in this response, but this still belongs to the alien-life thread: "
    end
  end

  defp missing_context_fallback do
    "I do not have prior chat context available in this response, and that looks like a follow-up. Restate the reference in one sentence and I can continue from there."
  end

  defp missing_context_followup?(text, opts) when is_binary(text) do
    not context_history_present?(opts) and
      Regex.match?(
        ~r/^\s*(well|so|but|also|and|then|recently|that|this|it|they|those|these|he|she)\b/iu,
        text
      )
  end

  defp missing_context_followup?(_, _), do: false

  defp context_history_present?(opts) when is_map(opts) do
    case Map.get(opts, :context_status) do
      %{history_present?: true} -> true
      %{"history_present?" => true} -> true
      _ -> false
    end
  end

  defp context_history_present?(_), do: false

  defp context_topic?(opts, topic) when is_map(opts) do
    opts
    |> Map.get(:context_status, Map.get(opts, "context_status", %{}))
    |> case do
      %{} = status -> Topics.has?(Map.get(status, :topics, Map.get(status, "topics", %{})), topic)
      _ -> false
    end
  end

  defp context_topic?(_, _), do: false

  defp casual_or_companion_text?(text) when is_binary(text) do
    not substantive_question_text?(text) and
      Regex.match?(
        ~r/\b(he+y+|hi+|hello|yo+|sup|wh+a+t'?s*\s*u+p+|wha+t+s+\s*u+p+|huh+\??|c+mon|companion|friend|talk|chat|made you|default responses?|not code|write code)\b/iu,
        text
      )
  end

  defp casual_or_companion_text?(_), do: false

  defp peace_or_war_question_text?(text) when is_binary(text) do
    Regex.match?(~r/\bwhat\s+do\s+you\s+think\s+(of|about)\b/iu, text) or
      (Regex.match?(
         ~r/\b(altern+a+tives?|options?|instead|other\s+ways?|peace|diplomacy|negotiation|de[-\s]?escalation|ceasefire|sanctions?|mediation|war)\b/iu,
         text
       ) and Regex.match?(~r/\b(war|conflict|fighting|violence)\b/iu, text))
  end

  defp peace_or_war_question_text?(_), do: false

  defp substantive_question_text?(text) when is_binary(text) do
    peace_or_war_question_text?(text) or
      Regex.match?(
        ~r/\b(what|why|how|when|where|who|which|should|could|would|can)\b/iu,
        text
      )
  end

  defp substantive_question_text?(_), do: false

  defp peace_or_war_fallback(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(
        ~r/\b(altern+a+tives?|options?|instead|other\s+ways?|peace|diplomacy|negotiation|ceasefire|mediation)\b/u,
        down
      ) ->
        "Alternatives to war usually start with de-escalation: diplomacy, mediated negotiation, ceasefires, peacekeeping, humanitarian corridors, targeted sanctions, international legal pressure, economic agreements, and long-term conflict-resolution work. None are magic, but they preserve more lives and keep more futures possible than armed escalation."

      Regex.match?(~r/\bwar\b/u, down) ->
        "War is one of the clearest failures of human coordination: it destroys lives, families, trust, infrastructure, and futures. I can understand why people study its causes or argue about defense and responsibility, but as a companion my default posture is anti-suffering and pro-deescalation."

      true ->
        "I can give you a grounded take. I’d separate the human impact, the incentives driving it, and what a less harmful path would look like."
    end
  end

  defp peace_or_war_fallback(_), do: "I can give you a grounded take."

  defp technical_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(code|coding|compile|compiler|debug|error|stacktrace|module|function|phoenix|elixir|liveview|server|repo|test|refactor|api|database|migration|deploy|pipeline)\b/iu,
      text
    )
  end

  defp technical_text?(_), do: false

  defp stable_seed(intent, tone, mode, opts) do
    :erlang.phash2(
      {intent, tone, mode, Map.get(opts, :file_hint), Map.get(opts, :flag),
       Map.get(opts, :next_step)}
    )
  end
end
