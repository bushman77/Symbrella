defmodule Core.Curiosity.EvidenceAcquisition do
  @moduledoc """
  Opt-in, one-gap acquisition for a persisted missing population reasoning link.

  A validated A-B proof can justify looking for B-C when the unresolved
  question is A-C. The planner never searches for arbitrary text, uses only
  the fixed bounded geographic provider route, and does not invent claims.
  Both new source observations must pass Core authorization. The shared B
  claim must match the persisted parent exactly before consolidation.

  Only the supervised Investigator invokes this in normal operation.
  """
  alias Core.Response.{AuthoritativeAnswerPlan, Consolidation, PopulationComparison, ResponseFrame}
  alias Core.Tools.WebRetrieval
  alias Db.{CognitiveIssue, ConsolidatedConclusion, ConsolidatedConclusions}

  # Test fixtures may inject a retriever only in the test build. Normal
  # supervised runtime always uses the real, fixed-route evidence resolver.
  @allow_test_retriever Code.ensure_loaded?(Mix) and Mix.env() == :test

  @max_year 2100
  @allowed_name ~r/\A[\p{L}\p{M}\p{N}\s,'’\-.]+\z/u

  @type plan :: %{
    issue_id: pos_integer(),
    parent_id: pos_integer(),
    year: integer(),
    subjects: [String.t()],
    bridge_claim: map()
  }

  @doc "Derive precisely one missing bridge pair using a revalidated stored proof."
  @spec plan(CognitiveIssue.t()) :: {:ok, plan()} | {:error, atom()}
  def plan(%CognitiveIssue{} = issue) do
    names = Map.get(issue.evidence || %{}, "subjects")
    parent_ids = Map.get(issue.evidence || %{}, "stored_proof_ids")

    with true <- issue.kind == "missing_reasoning_link" and
                   issue.next_action == "find_linking_premise" and
                   issue.status in ["open", "investigating"],
         true <- is_integer(issue.id) and issue.id > 0,
         true <- is_integer(issue.reference_year) and
                   issue.reference_year in 1900..@max_year,
         true <- valid_names?(names),
         true <- pair_key(names) == issue.subject_key,
         [parent_id] <- parent_ids,
         true <- is_integer(parent_id) and parent_id > 0,
         %ConsolidatedConclusion{
           id: ^parent_id,
           kind: "population_comparison",
           provenance: "source_supported_symbolic_derivation",
           reference_year: year,
           premises: [a, b]
         } = parent <- Db.get(ConsolidatedConclusion, parent_id),
         true <- year == issue.reference_year,
         true <- is_map(a) and is_map(b),
         true <- parent_fingerprint_valid?(parent),
         [common] <- Enum.filter([a, b], &(&1["subject"] in names)),
         [middle] <- Enum.filter([a, b], &(&1["subject"] not in names)),
         true <- valid_name?(middle["subject"]),
         [other] <- Enum.reject(names, &(&1 == common["subject"])),
         :ok <- revalidate_parent(parent, [common["subject"], middle["subject"]], year),
         bridge_pair <- [middle["subject"], other],
         true <- not stored_bridge?(bridge_pair, year) do
      {:ok, %{issue_id: issue.id, parent_id: parent_id, year: year,
              subjects: bridge_pair, bridge_claim: middle}}
    else
      _ -> {:error, :no_unique_valid_missing_bridge}
    end
  rescue
    _ -> {:error, :planner_unavailable}
  catch
    :exit, _ -> {:error, :planner_unavailable}
  end

  def plan(_), do: {:error, :invalid_issue}

  @doc """
  Bounded fetch and gated capture. An unsuccessful attempt writes no facts.
  The optional retriever is for offline tests; the supervised worker supplies
  no override and always uses WebRetrieval with the fixed provider routing.
  """
  @spec acquire(CognitiveIssue.t(), keyword()) :: map()
  def acquire(issue, opts \\ [])

  def acquire(%CognitiveIssue{} = issue, opts) when is_list(opts) do
    with {:ok, request} <- plan(issue),
         true <- still_open?(issue.id),
         ctx <- context(request.subjects, request.year),
         retrieved <- retrieve(ctx, opts) do
      result = validate_and_capture(issue, request, retrieved)

      Map.put(result, :provider_attempts, provider_attempts(retrieved))
    else
      {:error, reason} -> %{status: :unresolved, issue_id: issue.id, reason: reason}
      _ -> %{status: :unresolved, issue_id: issue.id,
             reason: :retrieval_or_authorization_failed}
    end
  rescue
    _ -> %{status: :unresolved, issue_id: issue.id, reason: :acquisition_unavailable}
  catch
    :exit, _ -> %{status: :unresolved, issue_id: issue.id, reason: :acquisition_unavailable}
  end

  def acquire(_, _), do: %{status: :unresolved, reason: :invalid_issue}

  defp validate_and_capture(issue, request, retrieved) do
    with %{resolution: :source_supported} <- Map.get(retrieved, :web_evidence),
         derived <- PopulationComparison.attach(retrieved),
         %{kind: :population_comparison, status: :source_supported,
           year: year, subjects: subjects, claims: claims} <-
           get_in(derived, [:web_evidence, :support]),
         true <- year == request.year and subjects == request.subjects,
         true <- bridge_matches?(request.bridge_claim, claims),
         framed <- Map.put(derived, :response_frame, ResponseFrame.build(derived)),
         %{kind: :verified_population_comparison} = answer <-
           AuthoritativeAnswerPlan.build(framed),
         true <- AuthoritativeAnswerPlan.authorized?(
           answer, AuthoritativeAnswerPlan.render(answer, :brief), framed),
         true <- still_open?(issue.id),
         false <- stored_bridge?(request.subjects, request.year),
         {:ok, proof} <- Consolidation.capture(framed, answer) do
      %{status: :captured, issue_id: issue.id, proof_id: proof.id}
    else
      {:error, reason} -> %{status: :unresolved, issue_id: issue.id, reason: reason}
      _ -> %{status: :unresolved, issue_id: issue.id,
             reason: :retrieval_or_authorization_failed}
    end
  end

  # Log only a closed vocabulary of provider identifiers and status codes.
  # Provider excerpts, raw search queries and rejected URLs stay transient.
  defp provider_attempts(%{web_evidence: %{attempts: attempts}})
      when is_list(attempts) do
    attempts
    |> Enum.take(4)
    |> Enum.map(fn attempt ->
      %{provider: provider_name(Map.get(attempt, :provider)),
        status: provider_status(Map.get(attempt, :status))}
    end)
  end

  defp provider_attempts(_), do: []

  defp provider_name(Core.Tools.WebRetrieval.Wikidata), do: "wikidata"
  defp provider_name(Core.Tools.WebRetrieval.StatisticsCanadaPopulation),
    do: "statistics_canada"
  defp provider_name(_), do: "other"

  defp provider_status(status)
       when status in [:source_supported, :unavailable, :partial_support, :error],
       do: Atom.to_string(status)
  defp provider_status(_), do: "unknown"

  defp retrieve(context, opts) do
    case Keyword.get(opts, :retriever) do
      fun when is_function(fun, 1) and @allow_test_retriever -> fun.(context)
      _ -> WebRetrieval.enrich(context, enabled?: true, max_attempts: 2,
              timeout_ms: 3_500)
    end
  end

  # Match the canonical immutable parent fingerprint before treating its
  # subjects as a reason to query an external source. Consolidation.recall
  # independently re-derives the stored comparison immediately afterward.
  defp parent_fingerprint_valid?(%ConsolidatedConclusion{
         premises: [a, b], pair_key: key, fingerprint: fingerprint,
         derivation: derivation
       }) when is_map(a) and is_map(b) do
    names = [a["subject"], b["subject"]]

    if Enum.all?(names, &is_binary/1) and is_map(derivation) and
         key == pair_key(names) do
      canonical = Enum.sort_by([a, b], &String.downcase(&1["subject"]))

      expected =
        :crypto.hash(:sha256, :erlang.term_to_binary({canonical, derivation}))
        |> Base.encode16(case: :lower)

      fingerprint == expected
    else
      false
    end
  end

  defp parent_fingerprint_valid?(_), do: false

  defp revalidate_parent(parent, subjects, year) do
    recalled = Consolidation.recall(context(subjects, year))
    case {Map.get(recalled, :consolidation),
          get_in(recalled, [:web_evidence, :support])} do
      {%{id: id, origin: :stored_symbolic_premises,
         reference_year: ^year},
       %{kind: :population_comparison, status: :source_supported,
         year: ^year, subjects: ^subjects}} when id == parent.id ->
        :ok

      _ -> {:error, :unverified_parent}
    end
  end

  defp bridge_matches?(stored, claims) when is_list(claims) do
    Enum.any?(claims, fn claim ->
      is_map(claim) and
        Enum.all?(
          ["subject", "value", "census_year", "reference_year",
           "source_url", "source_type", "measurement_type", "source_table"],
          fn key ->
            stored[key] == normalized_claim_field(claim, key)
          end
        )
    end)
  end

  defp bridge_matches?(_, _), do: false

  defp normalized_claim_field(claim, key) do
    atom_key = String.to_existing_atom(key)
    case Map.get(claim, atom_key) do
      val when is_atom(val) and not is_nil(val) -> Atom.to_string(val)
      val -> val
    end
  end

  defp stored_bridge?(subjects, year) do
    ConsolidatedConclusions.recent(pair_key(subjects))
    |> Enum.any?(&(&1.reference_year == year))
  end

  defp still_open?(id) do
    case Db.get(CognitiveIssue, id) do
      %CognitiveIssue{status: status, kind: "missing_reasoning_link",
                      next_action: "find_linking_premise"}
          when status in ["open", "investigating"] -> true
      _ -> false
    end
  end

  defp context(names, year) do
    frame = %{
      type: :location_question, domain: :geography, relation: :population,
      comparison: :greater, subjects: names, grounding_status: :unverified
    }

    %{
      session_id: "internal-curiosity-evidence",
      user_text: "Which had the larger population in #{year}, " <>
        Enum.join(names, " or ") <> "?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}, mood: %{}
    }
  end

  defp valid_names?([a, b]), do: valid_name?(a) and valid_name?(b) and a != b
  defp valid_names?(_), do: false

  defp valid_name?(name) when is_binary(name),
    do: byte_size(name) in 3..100 and Regex.match?(@allowed_name, name)
  defp valid_name?(_), do: false

  defp pair_key(subjects) do
    subjects
    |> Enum.map(&(String.downcase(String.trim(&1))))
    |> Enum.sort()
    |> Enum.join("|")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end
end
