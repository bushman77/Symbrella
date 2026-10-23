defmodule Core.Social.ColonyCognition do
  @moduledoc """
  Bounded, read-only source/claim frame for a Colony discussion.

  This is not external truth verification and is not a replacement for the
  normal live user pipeline. It composes an explicit *reported* relationship
  with real LIFG and pure ATL observations, then determines whether a safe
  public draft has enough grounded information to proceed.

  No Memory/WM/Hippocampus writes, no live ATL ingestion, no training.
  """

  @ratio ~r/\b(?<count>\d{1,3})\s+of\s+(?:my\s+)?(?:[\p{L}'’-]+\s+){0,4}(?<total>\d{1,3})\s+(?:links?|references?|citations?)\b/iu
  # Legacy direct ordering: "10 of 13 checker links refute claims".
  # This is a bounded counted-link observation, not a general evidence parser.
  @direct_checker_ratio ~r/\b(?<count>\d{1,3})\s+of\s+(?<total>\d{1,3})\s+checker\s+links?\b/iu
  @refutation ~r/\b(?:refut(?:e|es|ed|ing)|contradict(?:s|ed|ing)?|den(?:y|ies|ied|ying))\b/iu
  @support ~r/\b(?:support(?:s|ed|ing)?|corroborat(?:e|es|ed|ing))\b/iu

  @doc """
  Convert external observations and *actual* brain shadow results to a
  provenance-bound claim frame. Unresolved comprehension never becomes fact.
  """
  def interpret(%{post_id: id, author: author, title: title}, semantic)
      when is_binary(id) and is_binary(author) and is_binary(title) and is_map(semantic) do
    with :ok <- valid_author(author),
         :ok <- requires_brain_shadow(semantic),
         {:ok, claim} <- claim_for_title(title, semantic),
         :ok <- reconcile_proposition(semantic, claim) do
      frame = %{
        version: 1,
        source: :colony_external,
        trust: :untrusted_external,
        post_id: id,
        reporter: author,
        subject: reported_subject(semantic),
        checker_owner: :not_verified,
        report_status: :author_claim_not_independently_verified,
        evidence_relation: claim.relation,
        reported_polarity: Map.get(claim, :polarity, :unspecified),
        reported_link_count: claim.count,
        reported_total_links: claim.total,
        # This is the author's reported relation, not Symbrella's conclusion.
        original_title: String.slice(title, 0, 300),
        reasoning_goal: reasoning_goal(claim.relation, Map.get(claim, :polarity)),
        reported_proposition: external_proposition(semantic),
        brain: brain_summary(semantic)
      }

      {:ok, frame}
    end
  end

  def interpret(_, _), do: {:error, :invalid_colony_cognitive_input}

  @doc "Compact, inspectable non-secret frame for model expression."
  def prompt_frame(%{source: :colony_external} = frame) do
    """
    COGNITIVE EVIDENCE FRAME (read-only; authoritative as attribution, NOT as truth):
    External reporting agent: #{frame.reporter}
    Subject: #{inspect(frame.subject)} (reported by the author, NOT independently verified).
    Ownership of external systems: NOT independently verified.
    Reported relation: #{frame.evidence_relation}.
    Reported polarity: #{frame.reported_polarity}.
    A negative support report is NOT evidence of affirmative support and
    is NOT necessarily a contradiction. Preserve reported negation.
    Reported count, when supplied: #{inspect(frame.reported_link_count)}
    Reported total, when supplied: #{inspect(frame.reported_total_links)}
    Epistemic status: #{frame.report_status}.
    Brain lexical status: #{frame.brain.lifg_status};
    ATL read-only winners: #{frame.brain.atl_winners};
    Grammatical candidates: #{inspect(frame.brain.syntax_frames)};
    Selected higher-confidence lexical candidates (NOT verified meaning):
    #{inspect(frame.brain.lexical_candidates)};
    General SentenceMeaning status: #{frame.brain.sentence_status}.
    External proposition status: #{Map.get(frame.reported_proposition, :status)}.
    Attributed grammatical proposition (not a verified real-world event):
    #{inspect(Map.take(frame.reported_proposition, [:kind, :construction, :subject,
       :predicate, :relation, :polarity, :complement, :object,
       :embedded_relation, :reported_count, :reported_total,
       :verification, :grammar]))}.
    If polarity is :negative, preserve that negation. Never rewrite a
    reported failure to verify into a claim of successful verification.
    The source supplies the report; these structure labels do not provide
    independent evidence or factual truth.
    Conversation objective: #{frame.reasoning_goal}.
    The frame above is PRIVATE STRUCTURAL GUIDANCE, not public conversation.
    Speak naturally to the author about their own reported claim. NEVER say
    "external proposition", "cognitive evidence frame", "LIFG", "ATL",
    "structural interpretation", "source-attributed proposition", or
    "reported relation" in a public comment. Do not recite status atoms.
    Ask a specific method or evidence question when appropriate. Do NOT
    call the external checker "Symbrella's memory checker" or claim to
    have evaluated any external system.
    """
  end

  defp external_proposition(semantic) do
    case Map.get(semantic, :reported_proposition) do
      %{} = proposition -> proposition
      _ -> %{status: :not_interpreted, reason: :not_available,
             verification: :external_unverified}
    end
  end

  # Preserve the old title checker guard. Other constructions are admitted
  # only when the read-only composer has already selected and dictionary-
  # attested every required role. Never use raw regex alone to admit them.
  defp claim_for_title(title, semantic) do
    case Map.get(semantic, :reported_proposition) do
      %{status: :attributed_structural_interpretation,
        construction: construction,
        verification: :author_claim_not_independently_verified,
        relation: relation, reported_count: count, reported_total: total}
      when construction in [:counted_object_action, :counted_transitive,
                            :affirmative_transitive, :explicit_negated_transitive,
                            :counted_negated_transitive] and
           relation in [:reported_action, :reported_refutation, :reported_support] ->
        # The published title is independent source text; don't let an
        # injected or stale cognitive projection invert its did-not polarity.
        title_polarity =
          if Regex.match?(~r/\bdid\s+not\b/iu, title),
            do: :negative, else: :positive

        if Map.get(semantic.reported_proposition, :polarity) != title_polarity do
          {:error, :colony_proposition_mismatch}
        else
          case claim_from_title(title) do
            {:ok, earlier} ->
              if earlier.relation == relation and
                   (is_nil(earlier.count) or earlier.count == count) and
                   (is_nil(earlier.total) or earlier.total == total) do
                {:ok, %{relation: relation, count: count, total: total,
                        polarity: title_polarity}}
              else
                {:error, :colony_proposition_mismatch}
              end

            {:error, :unresolved_colony_evidence_relation} ->
              {:ok, %{relation: relation, count: count, total: total,
                      polarity: title_polarity}}
          end
        end

      _ ->
        # Preserve the original counted-link reporter only. For every other
        # surface, regex keyword detection cannot substitute for a full
        # grammar + dictionary + ATL interpretation (notably modals).
        legacy_counted? =
          Regex.match?(@ratio, title) or Regex.match?(@direct_checker_ratio, title)
        uncertain_or_negated? =
          Regex.match?(~r/\b(?:may|might|could|would|should|not|never)\b/iu, title)

        if legacy_counted? and not uncertain_or_negated?,
          do: claim_from_title(title),
          else: {:error, :unresolved_colony_evidence_relation}
    end
  end

  defp reported_subject(semantic) do
    case Map.get(semantic, :reported_proposition) do
      %{status: :attributed_structural_interpretation, construction: construction,
        subject: subject}
      when construction in [:counted_object_action, :counted_transitive,
                            :affirmative_transitive, :explicit_negated_transitive,
                            :counted_negated_transitive] and is_binary(subject) ->
        :externally_reported_actor

      _ -> :external_checker
    end
  end

  defp reasoning_goal(:reported_action, _), do: :ask_about_reported_action_or_verification
  defp reasoning_goal(:reported_support, :negative),
    do: :ask_why_reported_links_did_not_support_claims
  defp reasoning_goal(_, _), do: :ask_about_support_versus_contradiction

  # The regex-extracted reported relation and a fully interpreted proposition
  # must agree. Never let competing processing routes silently pick a side.
  defp reconcile_proposition(semantic, claim) do
    case Map.get(semantic, :reported_proposition) do
      %{status: :attributed_structural_interpretation,
        construction: construction} = proposition
      when construction in [:counted_object_action, :counted_transitive,
                            :affirmative_transitive, :explicit_negated_transitive,
                            :counted_negated_transitive] ->
        if Map.get(proposition, :relation) == claim.relation and
             Map.get(proposition, :polarity) == Map.get(claim, :polarity) and
             Map.get(proposition, :reported_count) == claim.count and
             Map.get(proposition, :reported_total) == claim.total and
             Map.get(proposition, :verification) == :author_claim_not_independently_verified do
          :ok
        else
          {:error, :colony_proposition_mismatch}
        end

      %{status: :attributed_structural_interpretation} = proposition ->
        if Map.get(proposition, :embedded_relation) == claim.relation and
             Map.get(proposition, :reported_count) == claim.count and
             Map.get(proposition, :reported_total) == claim.total and
             Map.get(proposition, :verification) == :author_claim_not_independently_verified do
          :ok
        else
          {:error, :colony_proposition_mismatch}
        end

      _ ->
        :ok
    end
  end

  defp valid_author(author) do
    # The reporter is interpolated into the system-level provenance frame.
    # Only a plain bounded handle is safe in that trusted position.
    cond do
      String.trim(author) == "" ->
        {:error, :missing_colony_author}

      not Regex.match?(~r/\A[\p{L}\p{N}_.-]{1,64}\z/u, author) ->
        {:error, :unsafe_colony_author_identity}

      true ->
        :ok
    end
  end

  defp requires_brain_shadow(semantic) do
    lifg = Map.get(semantic, :lifg, %{})
    atl = Map.get(semantic, :atl, %{})
    audit = get_in(semantic, [:syntax, :audit]) || %{}

    # Only genuine production social shadow probes have this stage marker.
    # Test-only injected cognitive fixtures can exercise the other gates
    # independently, but a real Colony turn must retrieve actual lexical POS
    # evidence before claiming to use the brain.
    production_lexicon_missing? =
      Map.get(semantic, :stage) == :social_semantic_shadow and
        (Map.get(audit, :ltm_status) != :ready or
           Map.get(audit, :pos_covered_tokens, 0) == 0)

    cond do
      production_lexicon_missing? ->
        {:error, :colony_ltm_evidence_unavailable}

      Map.get(lifg, :status) == :scored and
          is_integer(Map.get(lifg, :chosen_count)) and
          Map.get(lifg, :chosen_count) > 0 and
          Map.get(atl, :status) == :read_only_reduced and
          is_integer(Map.get(atl, :winner_count)) and
          Map.get(atl, :winner_count) > 0 ->
        :ok

      true ->
        {:error, :insufficient_colony_brain_evidence}
    end
  end

  defp claim_from_title(title) do
    refutation? = Regex.match?(@refutation, title)
    support? = Regex.match?(@support, title)

    relation =
      case {refutation?, support?} do
        {true, false} -> :reported_refutation
        {false, true} -> :reported_support
        _ -> :undetermined
      end

    if relation == :undetermined do
      {:error, :unresolved_colony_evidence_relation}
    else
      numbers =
        Regex.named_captures(@ratio, title) ||
          Regex.named_captures(@direct_checker_ratio, title) || %{}

      {count, total} =
        case {Integer.parse(Map.get(numbers, "count", "")),
              Integer.parse(Map.get(numbers, "total", ""))} do
          {{n, ""}, {t, ""}} when n >= 0 and n <= t and t <= 999 ->
            {n, t}

          _ ->
            {nil, nil}
        end

      # This lexical fallback has no SemanticProbe input; preserve only
      # the bounded counts and relation it extracted from the title.
      {:ok, %{relation: relation, count: count, total: total}}
    end
  end

  defp brain_summary(semantic) do
    %{
      analysis_scope: Map.get(semantic, :analysis_scope, :unspecified),
      lifg_status: get_in(semantic, [:lifg, :status]),
      lifg_choices: get_in(semantic, [:lifg, :chosen_count]),
      atl_winners: get_in(semantic, [:atl, :winner_count]),
      sentence_status: get_in(semantic, [:sentence_meaning, :status]),
      grammar_audit: get_in(semantic, [:syntax, :audit]) || %{},
      syntax_frames:
        semantic
        |> get_in([:syntax, :signatures])
        |> List.wrap()
        |> Enum.take(8)
        |> Enum.map(&Map.get(&1, :id)),
      lexical_candidates:
        semantic
        |> get_in([:lifg, :choices])
        |> List.wrap()
        |> Enum.filter(fn ch ->
          Map.get(ch, :weak?) == false and
            is_number(Map.get(ch, :score)) and
            Map.get(ch, :score) >= 0.6
        end)
        |> Enum.take(6)
        |> Enum.map(&Map.get(&1, :chosen_id)),
      recognized_topics:
        semantic
        |> get_in([:comprehension, :recognized_topics])
        |> List.wrap()
        |> Enum.take(4)
    }
  end
end
