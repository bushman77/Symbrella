defmodule Core.Response.VerifiedAnswerReceipt do
  @moduledoc """
  A bounded, same-session cue to explain the immediately preceding verified
  population answer. The receipt is NOT factual authority: each follow-up
  must re-derive the exact comparison through MemoryBoundary and check that
  its sources, values, year and proof identifiers have not changed.

  A receipt is created only after an authorized Core rendering and backed by
  immutable persisted proof IDs. Arbitrary LLM replies, user-provided URLs
  and assistant chat transcripts cannot create evidence receipts.
  """

  alias Core.Cognition.MemoryBoundary
  alias Core.Response.{AuthoritativeAnswerPlan, LlmChatHistory, PopulationComparison, ResponseFrame}
  alias Core.Response.LlmChatHistoryOwner
  alias Db.ConsolidatedConclusion

  @table :core_verified_answer_receipts
  @ttl_ms 10 * 60 * 1_000
  @max_sessions 256

  @doc "Recognize a narrow evidence question referencing the prior answer."
  @spec evidence_followup?(term()) :: boolean()
  def evidence_followup?(text) when is_binary(text) do
    normalized = text |> String.trim() |> String.replace(~r/\s+/u, " ")

    Regex.match?(
      ~r/\A(?:how do you know(?: that| this| those figures| those numbers)?|what evidence supports (?:your conclusion|that|this)|what (?:are|were) (?:your|the) sources?|where did you get (?:those|these|the) (?:numbers|figures)|can you (?:show|explain) (?:your|the) evidence)\s*[?.!]*\z/iu,
      normalized
    )
  end

  def evidence_followup?(_), do: false

  @doc """
  Store the authorized answer's identity, never the answer or its claims
  as a shortcut to factual authority. Proofs are independently rechecked on
  every explanation.
  """
  @spec record(term(), map(), map(), String.t(), term()) :: :ok
  def record(session_id, context, plan, answer, capture_result)
      when is_binary(session_id) and byte_size(session_id) in 1..128 and
           is_map(context) and is_map(plan) and is_binary(answer) do
    support = get_in(context, [:web_evidence, :support])
    names = PopulationComparison.comparison_subjects(context)
    ids = proof_ids(context, capture_result)

    with %{kind: :population_comparison, status: :source_supported,
           year: year, claims: [_, _] = claims, subjects: ^names} <- support,
         true <- length(names) == 2 and Enum.all?(names, &is_binary/1),
         true <- is_integer(year) and year in 1900..2100,
         true <- length(ids) in 1..2,
         true <- AuthoritativeAnswerPlan.authorized?(plan, answer, context),
         normalized when is_list(normalized) <- normalized_claims(claims) do
      LlmChatHistoryOwner.ensure_table!()
      timestamp = System.monotonic_time(:millisecond)
      receipt = %{
        subjects: names, year: year, claims: normalized,
        proof_ids: ids, answer_hash: digest(String.trim(answer))
      }

      :ets.insert(@table, {session_id, timestamp, receipt})
      prune(timestamp)
    end

    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  def record(_, _, _, _, _), do: :ok

  @doc "Invalidate the previous proof cue when another non-symbolic answer is shown."
  @spec clear(term()) :: :ok
  def clear(session_id) when is_binary(session_id) and byte_size(session_id) in 1..128 do
    if :ets.whereis(@table) != :undefined, do: :ets.delete(@table, session_id)
    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  def clear(_), do: :ok

  @doc """
  Return an evidence explanation if (and only if) the latest assistant turn
  matches the authorized receipt. A stale/changed proof fails closed.
  """
  @spec explain(term(), term()) :: :not_applicable | {:ok, String.t()} |
          {:unavailable, String.t()}
  def explain(user_text, session_id) do
    if evidence_followup?(user_text) do
      case receipt(session_id) do
        {:ok, saved} ->
          if most_recent_answer_matches?(session_id, saved.answer_hash) do
            case revalidate(saved, session_id) do
              {:ok, text} -> {:ok, text}
              _ ->
                {:unavailable,
                 "I can't currently revalidate the source-backed evidence for my previous " <>
                   "population comparison, so I shouldn't invent an explanation."}
            end
          else
            :not_applicable
          end

        _ ->
          :not_applicable
      end
    else
      :not_applicable
    end
  end

  defp proof_ids(%{composition: %{parent_proof_ids: [a, b]}}, _)
       when is_integer(a) and is_integer(b) and a > 0 and b > 0 and a != b,
       do: Enum.sort([a, b])

  defp proof_ids(%{consolidation: %{id: id}}, _) when is_integer(id) and id > 0,
    do: [id]

  defp proof_ids(_, {:ok, %{id: id}}) when is_integer(id) and id > 0, do: [id]
  defp proof_ids(_, _), do: []

  defp normalized_claims(claims) do
    claims
    |> Enum.map(fn
      %{subject: name, value: value, census_year: year,
        source_url: url, source_type: type} when is_binary(name) and
        is_integer(value) and value > 0 and is_integer(year) and is_binary(url) ->
        %{subject: name, value: value, year: year, url: url, type: type}

      _ -> nil
    end)
    |> case do
      [a, b] when is_map(a) and is_map(b) -> [a, b]
      _ -> nil
    end
  end

  defp receipt(session_id) when is_binary(session_id) and byte_size(session_id) in 1..128 do
    LlmChatHistoryOwner.ensure_table!()

    case :ets.lookup(@table, session_id) do
      [{^session_id, timestamp, saved}] when is_integer(timestamp) and is_map(saved) ->
        age = System.monotonic_time(:millisecond) - timestamp
        if age >= 0 and age <= @ttl_ms, do: {:ok, saved}, else: :expired

      _ -> :missing
    end
  rescue
    _ -> :missing
  catch
    :exit, _ -> :missing
  end

  defp receipt(_), do: :missing

  defp most_recent_answer_matches?(session_id, hash) do
    case List.last(LlmChatHistory.messages(session_id, 1)) do
      %{"role" => "assistant", "content" => answer} when is_binary(answer) ->
        digest(String.trim(answer)) == hash

      _ -> false
    end
  end

  defp revalidate(%{subjects: [a, c] = names, year: year} = saved, session_id) do
    frame = %{
      type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified
    }

    request = %{
      session_id: session_id,
      user_text: "Which had the larger population in #{year}, #{a} or #{c}?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}, mood: %{}
    }

    recalled = MemoryBoundary.recall(request)
    with normalized when is_list(normalized) <-
           normalized_claims(get_in(recalled, [:web_evidence, :support, :claims])),
         true <- normalized == saved.claims,
         %{kind: :verified_population_comparison, year: ^year} = plan <-
           recalled
           |> Map.put(:response_frame, ResponseFrame.build(recalled))
           |> AuthoritativeAnswerPlan.build(),
         true <- is_binary(AuthoritativeAnswerPlan.render(plan, :brief)),
         true <- proof_ids(recalled, nil) == saved.proof_ids,
         {:ok, proof_rows} <- load_proofs(saved.proof_ids, year) do
      explanation(proof_rows, recalled, plan, names)
    else
      _ -> {:error, :no_longer_verified}
    end
  rescue
    _ -> {:error, :revalidation_unavailable}
  catch
    :exit, _ -> {:error, :revalidation_unavailable}
  end

  defp load_proofs(ids, year) do
    proofs = Enum.map(ids, &Db.get(ConsolidatedConclusion, &1))

    if Enum.all?(proofs, fn
         %ConsolidatedConclusion{kind: "population_comparison",
           provenance: "source_supported_symbolic_derivation",
           reference_year: ^year, premises: [_, _]} -> true
         _ -> false
       end),
       do: {:ok, proofs},
       else: {:error, :invalid_proof}
  end

  defp explanation([proof], %{consolidation: %{id: id}}, plan, names)
       when proof.id == id do
    [a, c] = names

    {:ok,
     "I used a source-backed #{plan.year} population comparison stored in " <>
       "Symbrella's verified symbolic memory. " <>
       "The independently recorded figures for #{a} and #{c} support the " <>
       "conclusion. I rechecked the stored premises instead of asking the " <>
       "language model to estimate the numbers.\n" <>
       proof_line(proof) <>
       "\nThe larger population was #{plan.winner}, a difference of " <>
       number(plan.difference) <> ". These are dated figures, not current estimates."}
  end

  defp explanation([first, second], %{composition: %{parent_proof_ids: ids,
       intermediate_subject: middle, rule: :transitive_population_order}}, plan, [a, c])
       when is_binary(middle) do
    if Enum.sort([first.id, second.id]) == ids and
         Enum.any?([first, second], &has_subjects?(&1, a, middle)) and
         Enum.any?([first, second], &has_subjects?(&1, middle, c)) do
      {:ok,
       "I reached that conclusion by combining two independently verified " <>
         "population comparisons for #{plan.year}, not by retrieving a direct " <>
         "#{a}-versus-#{c} comparison:\n" <>
         proof_line(first) <> "\n" <> proof_line(second) <>
         "\nBoth comparisons agree on #{middle}'s population. Core rechecked " <>
         "their dates, source observations and ordering before applying " <>
         "transitive reasoning. The resulting difference between #{a} and " <>
         "#{c} is #{number(plan.difference)}. These are historical figures."}
    else
      {:error, :inconsistent_lineage}
    end
  end

  defp explanation(_, _, _, _), do: {:error, :unsupported_lineage}

  defp has_subjects?(%{premises: premises}, a, b),
    do: Enum.sort(Enum.map(premises, & &1["subject"])) == Enum.sort([a, b])

  defp proof_line(%{premises: [a, b]}) do
    "#{a["subject"]}: #{number(a["value"])}; " <>
      "#{b["subject"]}: #{number(b["value"])}. " <>
      "Sources: #{a["source_url"]}, #{b["source_url"]}"
  end

  defp number(v) when is_integer(v) do
    v
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/.{3}(?=.)/u, "\\0,")
    |> String.reverse()
  end

  defp digest(s), do: :crypto.hash(:sha256, s)

  defp prune(now) do
    :ets.tab2list(@table)
    |> Enum.each(fn {session, timestamp, _receipt} ->
      if timestamp > now or now - timestamp > @ttl_ms,
        do: :ets.delete(@table, session)
    end)

    entries = :ets.tab2list(@table)

    if length(entries) > @max_sessions do
      entries
      |> Enum.sort_by(&elem(&1, 1))
      |> Enum.take(length(entries) - @max_sessions)
      |> Enum.each(fn {session, _, _} -> :ets.delete(@table, session) end)
    end
  end
end
