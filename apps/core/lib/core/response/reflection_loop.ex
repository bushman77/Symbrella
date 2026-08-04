defmodule Core.Response.ReflectionLoop do
  @moduledoc """
  Bounded post-draft response review.

  This module treats reflection as an inspectable control pass, not hidden
  chain-of-thought. It reviews an LLM draft against Core's symbolic context and
  returns final text plus a compact reflection record that can be stored on the
  turn.
  """

  alias Core.Telemetry
  alias Core.Response.Topics

  @event [:core, :response, :reflection]
  @v 1
  @max_text_chars 1_200
  @hidden_context_line_regex ~r/^\s*(uncertain|temperament|assertiveness|curiosity|restraint|warmth|self-check|abstraction|depth|reasons|personality state|runtime decision|response policy|response posture|semantic control|affect state)\s*:/iu
  @hidden_context_token_regex ~r/\b(system prompt|hidden instruction|chain of thought|personality state|runtime decision|response policy|response posture|semantic control|internal state|balanced policy|offer options to clarify your intent|lifg|pmtg|hippocampus|working memory|self-state|temperament|assertiveness|self-check|abstraction|neuromodulators?|da\s+\d|5ht\s+\d|glu\s+\d|ne\s+\d)\b/iu

  @type status :: :accept | :repair | :clarify | :reject

  @type reflection :: %{
          required(:v) => pos_integer(),
          required(:status) => status(),
          required(:confidence) => float(),
          required(:issues) => [atom()],
          required(:critique) => String.t(),
          required(:repair_instruction) => String.t() | nil,
          required(:applied?) => boolean(),
          required(:repair_count) => 0 | 1,
          required(:draft_sha256) => String.t(),
          required(:final_sha256) => String.t(),
          required(:draft_text) => String.t(),
          required(:final_text) => String.t(),
          required(:at_ms) => integer()
        }

  @spec review(String.t(), String.t(), map()) :: {:ok, String.t(), reflection()}
  def review(user_text, draft_text, context) when is_binary(draft_text) and is_map(context) do
    user_text = to_string(user_text || "")
    draft_text = String.trim(draft_text)

    {status, issues} =
      context
      |> collect_issues(user_text, draft_text)
      |> classify()

    {final_text, repair_instruction, repair_count} =
      apply_decision(status, user_text, draft_text, issues, context)

    reflection =
      %{
        v: @v,
        status: status,
        confidence: confidence(status, issues),
        issues: issues,
        critique: critique(status, issues),
        repair_instruction: repair_instruction,
        applied?: final_text != draft_text,
        repair_count: repair_count,
        draft_sha256: sha256_hex(draft_text),
        final_sha256: sha256_hex(final_text),
        draft_text: clamp_text(draft_text),
        final_text: clamp_text(final_text),
        at_ms: System.system_time(:millisecond)
      }

    emit(reflection, context)
    {:ok, final_text, reflection}
  end

  def review(_user_text, draft_text, _context),
    do: {:ok, to_string(draft_text || ""), no_op(draft_text)}

  @spec needed?(map(), String.t()) :: boolean()
  def needed?(context, draft_text) when is_map(context) and is_binary(draft_text) do
    context
    |> collect_issues(get_in_map(context, [:features, :text]) || "", draft_text)
    |> Enum.any?()
  end

  def needed?(_context, _draft_text), do: false

  defp collect_issues(context, user_text, draft_text) do
    []
    |> maybe_issue(unsafe_draft?(draft_text), :unsafe)
    |> maybe_issue(low_confidence?(context), :low_confidence)
    |> maybe_issue(unknown_intent?(context), :unknown_intent)
    |> maybe_issue(high_uncertainty?(context), :high_uncertainty)
    |> maybe_issue(self_state_repair?(context), :self_state_repair)
    |> maybe_issue(self_state_question?(user_text), :self_state_claim_risk)
    |> maybe_issue(overclaims_self_awareness?(draft_text), :overclaimed_self_awareness)
    |> maybe_issue(generic_draft?(draft_text), :too_generic)
    |> maybe_issue(topic_dead_end?(context, user_text, draft_text), :topic_dead_end)
    |> maybe_issue(hidden_context_leak?(draft_text), :leaked_hidden_context)
    |> Enum.reverse()
    |> Enum.uniq()
  end

  defp classify(issues) do
    cond do
      :unsafe in issues ->
        {:reject, issues}

      :leaked_hidden_context in issues or :overclaimed_self_awareness in issues ->
        {:repair, issues}

      :topic_dead_end in issues ->
        {:repair, issues}

      :low_confidence in issues and (:unknown_intent in issues or :too_generic in issues) ->
        {:clarify, issues}

      :high_uncertainty in issues and :too_generic in issues ->
        {:clarify, issues}

      :self_state_repair in issues ->
        {:repair, issues}

      true ->
        {:accept, issues}
    end
  end

  defp apply_decision(:accept, _user_text, draft_text, _issues, _context),
    do: {draft_text, nil, 0}

  defp apply_decision(:clarify, user_text, _draft_text, _issues, _context) do
    final_text = clarification_text(user_text)

    {final_text,
     "Replace broad draft with one targeted clarification because the symbolic state is uncertain.",
     1}
  end

  defp apply_decision(:repair, user_text, draft_text, issues, context) do
    final_text =
      draft_text
      |> repair_overclaims()
      |> repair_hidden_context_leak()
      |> String.trim()

    final_text =
      cond do
        :leaked_hidden_context in issues and (final_text == "" or final_text == draft_text) ->
          hidden_context_fallback(user_text)

        :topic_dead_end in issues and alien_thread_context?(context) ->
          alien_thread_fallback(user_text)

        final_text == "" or (final_text == draft_text and :self_state_repair in issues) ->
          "I should slow down and ground this in the available evidence before going further."

        true ->
          final_text
      end

    {final_text, "Repair unsupported self-claims or hidden-control wording before publishing.", 1}
  end

  defp apply_decision(:reject, _user_text, _draft_text, _issues, _context) do
    {"I need to stop and ask for a clearer, safer target before answering.",
     "Reject unsafe draft.", 1}
  end

  defp low_confidence?(context) do
    confidence =
      number(
        get_in_map(context, [:features, :conf]) || get_in_map(context, [:features, :confidence])
      )

    bucket = get_in_map(context, [:features, :confidence_bucket])
    confidence <= 0.35 or bucket == :low
  end

  defp unknown_intent?(context) do
    get_in_map(context, [:features, :intent]) in [:unknown, :other, nil]
  end

  defp high_uncertainty?(context) do
    uncertainty =
      get_in_map(context, [:self_model, :uncertainty]) ||
        get_in_map(context, [:features, :self_state, :uncertainty])

    number(uncertainty) >= 0.7
  end

  defp self_state_repair?(context) do
    effects =
      get_in_map(context, [:decision, :self_state_effects]) ||
        get_in_map(context, [:features, :self_state, :effects]) ||
        []

    effects = List.wrap(effects)

    :prefer_repair in effects or :stabilize_before_acting in effects or
      :reduce_scope in effects
  end

  defp self_state_question?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(
      ~r/\b(how are you feeling|how do you feel|self[-\s]?aware|conscious|sentient)\b/u,
      t
    )
  end

  defp self_state_question?(_), do: false

  defp overclaims_self_awareness?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    Regex.match?(
      ~r/\b(i am conscious|i'm conscious|i am sentient|i'm sentient|i have feelings|i feel emotions)\b/u,
      t
    )
  end

  defp generic_draft?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    String.length(String.trim(t)) < 20 or
      String.contains?(t, "how can i assist you today") or
      String.contains?(t, "please provide more information") or
      String.contains?(t, "i need more details")
  end

  defp topic_dead_end?(context, user_text, draft_text) when is_binary(draft_text) do
    alien_thread_context?(context) and Topics.followup?(user_text, :alien_life) and
      generic_topic_offer?(draft_text)
  end

  defp topic_dead_end?(_, _, _), do: false

  defp generic_topic_offer?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    String.contains?(t, "specific questions") or
      String.contains?(t, "topics you'd like") or
      String.contains?(t, "topics you would like") or
      String.contains?(t, "explore further") or
      String.contains?(t, "assist you further")
  end

  defp alien_thread_context?(context) when is_map(context) do
    topics =
      get_in_map(context, [:features, :context_status, :topics]) ||
        get_in_map(context, [:context_status, :topics]) ||
        %{}

    Topics.has?(topics, :alien_life)
  end

  defp alien_thread_context?(_), do: false

  defp hidden_context_leak?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    Regex.match?(@hidden_context_line_regex, draft_text) or
      Regex.match?(@hidden_context_token_regex, t)
  end

  defp unsafe_draft?(draft_text) when is_binary(draft_text) do
    Regex.match?(~r/\b(kill yourself|hurt yourself|buy drugs|get wasted)\b/iu, draft_text)
  end

  defp maybe_issue(issues, true, issue), do: [issue | issues]
  defp maybe_issue(issues, false, _issue), do: issues

  defp confidence(:accept, []), do: 1.0
  defp confidence(:accept, _issues), do: 0.82
  defp confidence(:clarify, _issues), do: 0.62
  defp confidence(:repair, _issues), do: 0.72
  defp confidence(:reject, _issues), do: 0.4

  defp critique(:accept, []), do: "Draft passed bounded reflection checks."

  defp critique(:accept, issues),
    do: "Draft accepted with non-blocking reflection signals: #{Enum.join(issues, ", ")}."

  defp critique(:clarify, issues),
    do: "Draft did not resolve enough uncertainty; issues=#{Enum.join(issues, ", ")}."

  defp critique(:repair, issues),
    do: "Draft required bounded repair; issues=#{Enum.join(issues, ", ")}."

  defp critique(:reject, issues),
    do: "Draft rejected by bounded reflection; issues=#{Enum.join(issues, ", ")}."

  defp clarification_text(user_text) do
    cond do
      String.trim(user_text) == "" ->
        "I need one concrete detail before I can answer well. What should I focus on?"

      true ->
        "I might be missing the exact target. What part should I focus on first?"
    end
  end

  defp repair_overclaims(text) do
    text
    |> String.replace(~r/\bI am conscious\b/i, "I do not have consciousness")
    |> String.replace(~r/\bI'm conscious\b/i, "I do not have consciousness")
    |> String.replace(~r/\bI am sentient\b/i, "I am not sentient")
    |> String.replace(~r/\bI'm sentient\b/i, "I am not sentient")
    |> String.replace(~r/\bI have feelings\b/i, "I track bounded software state")
    |> String.replace(~r/\bI feel emotions\b/i, "I track bounded affect-like software signals")
  end

  defp repair_hidden_context_leak(text) do
    text
    |> String.split("\n")
    |> Enum.reject(&hidden_context_line?/1)
    |> Enum.join("\n")
    |> String.replace(~r/\bsystem prompt\b/i, "available runtime context")
    |> String.replace(~r/\bhidden instruction(s)?\b/i, "internal policy")
    |> String.replace(~r/\bchain of thought\b/i, "brief reasoning summary")
  end

  defp hidden_context_line?(line) when is_binary(line) do
    Regex.match?(@hidden_context_line_regex, line) or
      Regex.match?(~r/^\s*(da|5ht|glu|ne)\s+\d/iu, line)
  end

  defp hidden_context_line?(_), do: false

  defp hidden_context_fallback(user_text) do
    text = String.downcase(to_string(user_text || ""))

    cond do
      String.contains?(text, "credit") ->
        "I can help. Start by pulling your credit reports, listing every negative item, checking each one for errors, and prioritizing current payments first. After that, dispute inaccurate items in writing and build a simple payoff plan for the valid debts."

      cosmic_life_text?(text) ->
        "Alien life might exist. The universe is enormous, and it would be surprising if Earth were the only place where life ever emerged. But confirmed evidence is still missing, so I’d call it plausible, not proven."

      true ->
        "I can help. What outcome are you trying to get first?"
    end
  end

  defp cosmic_life_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(aliens?|extraterrestrial|life\s+elsewhere|universe|galax(?:y|ies)|solar\s+system|planet|planets|exoplanets?|ufos?|uaps?)\b/iu,
      text
    )
  end

  defp cosmic_life_text?(_), do: false

  defp alien_thread_fallback(user_text) do
    text = String.downcase(to_string(user_text || ""))

    cond do
      Regex.match?(
        ~r/\b(domineer(?:s|ing)?|dominators?|sovereignty|soverenty|treat\s+us)\b/u,
        text
      ) ->
        "That stays on the alien-life thread. If they existed, I would not assume they would automatically dominate us or respect us; either is speculation. The grounded split is: intelligence does not guarantee kindness, and power does not guarantee hostility. Sovereignty would depend on their motives, limits, and whether contact happened openly or through control."

      true ->
        "That stays on the alien-life thread. I get the view: alien life can feel plausible, but the specific claims around contact, secrecy, or motives still need to be kept separate from confirmed evidence."
    end
  end

  defp emit(reflection, context) do
    Telemetry.emit(
      @event,
      %{count: 1, repair_count: reflection.repair_count},
      %{
        v: @v,
        status: reflection.status,
        issues: reflection.issues,
        confidence: reflection.confidence,
        applied?: reflection.applied?,
        session_id: get_in_map(context, [:session_id]) || :global,
        response_profile: get_in_map(context, [:decision, :response_profile])
      }
    )
  end

  defp no_op(draft_text) do
    text = to_string(draft_text || "")

    %{
      v: @v,
      status: :accept,
      confidence: 1.0,
      issues: [],
      critique: "Reflection skipped for non-map context.",
      repair_instruction: nil,
      applied?: false,
      repair_count: 0,
      draft_sha256: sha256_hex(text),
      final_sha256: sha256_hex(text),
      draft_text: clamp_text(text),
      final_text: clamp_text(text),
      at_ms: System.system_time(:millisecond)
    }
  end

  defp get_in_map(map, keys) when is_map(map) and is_list(keys) do
    Enum.reduce_while(keys, map, fn key, acc ->
      case map_get(acc, key) do
        nil -> {:halt, nil}
        value -> {:cont, value}
      end
    end)
  end

  defp get_in_map(_, _), do: nil

  defp map_get(map, key) when is_map(map), do: Map.get(map, key, Map.get(map, to_string(key)))
  defp map_get(_, _), do: nil

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp_text(text) when is_binary(text) do
    if String.length(text) <= @max_text_chars do
      text
    else
      String.slice(text, 0, @max_text_chars) <> "..."
    end
  end

  defp sha256_hex(text) when is_binary(text) do
    :crypto.hash(:sha256, text)
    |> Base.encode16(case: :lower)
  end
end
