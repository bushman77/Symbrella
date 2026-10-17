defmodule Core.Brain.WM.VerifiedSemantics do
  @moduledoc """
  Core-owned, session-scoped WM representation of an independently reverified
  population comparison. This is an attentional pointer to persisted proofs,
  never a factual authority or a replacement for comprehension or LIFG.

  Admission reruns the MemoryBoundary proof checks from the original names
  and year. Neither LLM output nor earlier assistant prose can authorize the
  payload. Only bounded symbolic metadata crosses into the global Brain WM.
  """

  alias Core.Brain.Runtime
  alias Core.Cognition.MemoryBoundary
  alias Core.Response.{AuthoritativeAnswerPlan, ResponseFrame}

  @max_age_ms 20 * 60 * 1_000
  @max_entity_chars 100
  @candidate_score 0.85

  @type act :: :verified_answer | :evidence_followup

  @doc """
  Admit the interpretation of a VERIFIED comparison to Brain WM.

  The caller supplies only a session, question identity and IDs to compare.
  Core must independently rederive the conclusion from persisted source proofs
  before recording even a reference to those proofs in working memory.
  """
  @spec admit(term(), term(), term(), term(), act()) :: :admitted | :rejected | :disabled
  def admit(session, subjects, year, ids, act) do
    cond do
      not enabled?() ->
        :disabled

      not valid_request?(session, subjects, year, ids, act) ->
        emit(:rejected, :invalid_request, 0)
        :rejected

      true ->
        admit_verified(session, subjects, year, Enum.sort(ids), act)
    end
  end

  @doc "Read the session's current interpretation pointer, not a factual assertion."
  @spec snapshot(term()) :: map() | nil
  def snapshot(session) when is_binary(session) and byte_size(session) in 1..128 do
    with true <- Runtime.pid_alive?(Brain),
         %{wm: wm} when is_list(wm) <- Runtime.apply_if_exported(
           Brain, :snapshot_wm, [], nil),
         id <- frame_id(session),
         %{payload: %{kind: :verified_semantic_frame, context: context}} = item <-
           Enum.find(wm, &(Map.get(&1, :id) == id)),
         ts when is_integer(ts) <- Map.get(item, :last_bump, Map.get(item, :ts)),
         now <- System.system_time(:millisecond),
         true <- ts <= now and now - ts <= @max_age_ms do
      context
    else
      _ -> nil
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  def snapshot(_), do: nil

  @doc "Deterministic session-specific Brain WM identifier, suitable for tests."
  @spec frame_id(term()) :: String.t()
  def frame_id(session) do
    digest =
      :crypto.hash(:sha256, :erlang.term_to_binary({:verified_semantic_wm, session}))
      |> Base.encode16(case: :lower)
      |> binary_part(0, 32)

    "verified_semantics:#{digest}"
  end

  defp admit_verified(session, [first, last] = names, year, expected_ids, act) do
    frame = %{
      type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified
    }

    request = %{
      session_id: session,
      user_text: "Which had the larger population in #{year}, #{first} or #{last}?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer},
      mood: %{}
    }

    verified = MemoryBoundary.recall(request)
    reviewed = Map.put(verified, :response_frame, ResponseFrame.build(verified))

    with %{kind: :verified_population_comparison, year: ^year} = plan <-
           AuthoritativeAnswerPlan.build(reviewed),
         true <- AuthoritativeAnswerPlan.authorized?(
           plan, AuthoritativeAnswerPlan.render(plan, :brief), reviewed),
         %{kind: :population_comparison, status: :source_supported,
           year: ^year, subjects: ^names, claims: [_, _]} <-
           get_in(verified, [:web_evidence, :support]),
         {:ok, ^expected_ids, rule, intermediate} <- lineage(verified),
         true <- Runtime.pid_alive?(Brain) do
      context = %{
        schema: 1,
        kind: :verified_semantic_frame,
        conversation_act: act,
        relation: :population_comparison,
        subjects: names,
        entities: Enum.take(Enum.uniq([first, intermediate, last]) |> Enum.reject(&is_nil/1), 3),
        reference_year: year,
        reasoning_rule: rule,
        parent_proof_ids: expected_ids,
        proof_source: :consolidated_conclusions,
        verification: :source_supported,
        interpretation_status: :verified,
        # A WM item is a cue. All future uses must revalidate the proofs.
        factual_authority?: false,
        updated_at_ms: System.system_time(:millisecond)
      }

      candidate = %{
        id: frame_id(session),
        lemma: "verified population interpretation",
        source: :verified_semantics,
        score: @candidate_score,
        reason: :source_revalidated_symbolic_comparison,
        kind: :verified_semantic_frame,
        context: context
      }

      # Per-call gates for this single typed context candidate. Never modify
      # Brain's configured lexical WM thresholds or unrelated candidate gates.
      wm =
        Runtime.apply_if_exported(Brain, :focus, [
          [candidate],
          [
            gate_threshold: 0.0, boost_threshold: 0.0,
            boost_threshold_pref: 0.0, block_threshold: 0.0,
            evidence_floor: 0.0
          ]
        ], :unavailable)

      admitted? =
        is_list(wm) and Enum.any?(wm, &(Map.get(&1, :id) == candidate.id))

      emit(if(admitted?, do: :admitted, else: :rejected),
        if(admitted?, do: :revalidated_proof, else: :wm_admission_unavailable),
        length(expected_ids))

      if admitted?, do: :admitted, else: :rejected
    else
      _ ->
        reject_and_defocus(session, :proof_revalidation_failed, length(expected_ids))
    end
  rescue
    _ ->
      reject_and_defocus(session, :proof_revalidation_unavailable, length(expected_ids))
  catch
    :exit, _ ->
      reject_and_defocus(session, :proof_revalidation_unavailable, length(expected_ids))
  end

  defp reject_and_defocus(session, reason, proof_count) do
    # If an earlier pointer survives but its proofs no longer revalidate,
    # remove that stale reference from WM rather than retaining a misleading
    # :source_supported semantic cue.
    if Runtime.pid_alive?(Brain) do
      _ = Runtime.apply_if_exported(Brain, :defocus, [frame_id(session)], :unavailable)
    end

    emit(:rejected, reason, proof_count)
    :rejected
  rescue
    _ ->
      emit(:rejected, reason, proof_count)
      :rejected
  catch
    :exit, _ ->
      emit(:rejected, reason, proof_count)
      :rejected
  end

  defp lineage(%{composition: %{
           rule: :transitive_population_order,
           parent_proof_ids: [a, b],
           intermediate_subject: middle
         }})
       when is_integer(a) and is_integer(b) and a > 0 and b > 0 and a != b and
            is_binary(middle),
       do: {:ok, Enum.sort([a, b]), :transitive_population_order, middle}

  defp lineage(%{consolidation: %{id: id}}) when is_integer(id) and id > 0,
    do: {:ok, [id], :direct_population_comparison, nil}

  defp lineage(_), do: :invalid

  defp valid_request?(session, [a, b], year, ids, act)
       when is_binary(session) and byte_size(session) in 1..128 and
            is_binary(a) and is_binary(b) and a != b and
            is_integer(year) and year in 1900..2100 and
            is_list(ids) and act in [:verified_answer, :evidence_followup] do
    names_valid? =
      Enum.all?([a, b], fn name ->
        byte_size(name) in 3..@max_entity_chars and
          String.trim(name) == name and name != ""
      end)

    ids_valid? =
      length(ids) in 1..2 and Enum.all?(ids, &(is_integer(&1) and &1 > 0)) and
        length(Enum.uniq(ids)) == length(ids)

    names_valid? and ids_valid?
  end

  defp valid_request?(_, _, _, _, _), do: false

  defp enabled? do
    case Application.get_env(:core, __MODULE__, []) do
      config when is_list(config) -> Keyword.get(config, :enabled?, false) == true
      _ -> false
    end
  end

  defp emit(decision, reason, proof_count) do
    :telemetry.execute(
      [:core, :wm, :verified_semantics],
      %{candidates: 1, admitted: if(decision == :admitted, do: 1, else: 0)},
      %{decision: decision, reason: reason, proof_count: proof_count,
        admission_path: :core_revalidated_proofs, factual_authority?: false}
    )
  end
end
