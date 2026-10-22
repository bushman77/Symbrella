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
         {:ok, claim} <- claim_from_title(title) do
      frame = %{
        version: 1,
        source: :colony_external,
        trust: :untrusted_external,
        post_id: id,
        reporter: author,
        subject: :external_checker,
        checker_owner: :not_verified,
        report_status: :author_claim_not_independently_verified,
        evidence_relation: claim.relation,
        reported_link_count: claim.count,
        reported_total_links: claim.total,
        # This is the author's reported relation, not Symbrella's conclusion.
        original_title: String.slice(title, 0, 300),
        reasoning_goal: :ask_about_support_versus_contradiction,
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
    Subject: an external checker's evidence links, not Symbrella's checker.
    Ownership of the checker: NOT independently verified.
    Reported relation: #{frame.evidence_relation}.
    Reported number of matching links: #{inspect(frame.reported_link_count)}
    Reported total links: #{inspect(frame.reported_total_links)}
    Epistemic status: #{frame.report_status}.
    Brain lexical status: #{frame.brain.lifg_status};
    ATL read-only winners: #{frame.brain.atl_winners};
    Grammatical candidates: #{inspect(frame.brain.syntax_frames)};
    Selected higher-confidence lexical candidates (NOT verified meaning):
    #{inspect(frame.brain.lexical_candidates)};
    Sentence comprehension: #{frame.brain.sentence_status}.
    Response objective: ask how source evidence is classified as support vs
    contradiction. Do NOT call it "Symbrella's memory checker" or
    claim to have evaluated the external system.
    """
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
      numbers = Regex.named_captures(@ratio, title) || %{}

      {count, total} =
        case {Integer.parse(Map.get(numbers, "count", "")),
              Integer.parse(Map.get(numbers, "total", ""))} do
          {{n, ""}, {t, ""}} when n >= 0 and n <= t and t <= 999 ->
            {n, t}

          _ ->
            {nil, nil}
        end

      {:ok, %{relation: relation, count: count, total: total}}
    end
  end

  defp brain_summary(semantic) do
    %{
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
