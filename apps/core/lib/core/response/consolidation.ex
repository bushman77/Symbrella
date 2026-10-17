defmodule Core.Response.Consolidation do
  @moduledoc """
  First cortical-consolidation bridge: retain a Core-derived comparison as a
  durable, source-backed set of premises, NOT as an earlier assistant reply.

  The hippocampus continues to own conversational episodes. This separate
  semantic store makes completed, source-supported deductions reusable after
  a BEAM restart. A future replay worker can promote eligible hippocampal
  episodes through the *same* gate; replay cannot validate a claim by itself.

  Narrow v1 scope: two public-place population claims from a compatible
  reference year. No personal memories, LLM-authored facts or generic prose.
  Opt-in using :core, :symbolic_consolidation_enabled? after DB migration.
  """
  require Logger

  alias Core.Response.{AuthoritativeAnswerPlan, PopulationComparison, ResponseFrame}
  alias Core.Reasoning.PopulationChain
  alias Brain.Hippocampus.SemanticAssociations
  alias Db.{CognitiveIssues, ConsolidatedConclusions}

  @source_types [:wikidata, :wikipedia, :statistics_canada]
  @measurement_types [nil, :official_estimate, :dated_population_statement, :census]
  @freshness ~r/\b(?:latest|current|currently|today|now|this\s+year|most\s+recent|up\s+to\s+date|as\s+of\s+now)\b/iu

  @doc """
  Called only AFTER the authoritative symbolic output passes exact
  authorization. Recheck the proof, normalize premises, then persist them.
  The generated answer is never stored as source knowledge.
  """
  @spec capture(map(), map()) :: {:ok, term()} | {:error, term()} | :disabled
  def capture(context, plan) when is_map(context) and is_map(plan) do
    if enabled?() do
      with %{kind: :verified_population_comparison} <- plan,
           %{kind: :verified_population_comparison} <- AuthoritativeAnswerPlan.build(context),
           true <- AuthoritativeAnswerPlan.authorized?(
             plan, AuthoritativeAnswerPlan.render(plan, :brief), context),
           %{kind: :population_comparison, status: :source_supported,
             claims: [a, b], year: year} <- get_in(context, [:web_evidence, :support]),
           false <- consolidated_origin?(context),
           {:ok, [a1, b1]} <- normalize_claims([a, b]),
           true <- a1["census_year"] == year and b1["census_year"] == year,
           subjects <- [a1["subject"], b1["subject"]],
           true <- subjects == PopulationComparison.comparison_subjects(context),
           proof <- derivation([a1, b1]),
           true <- matches_plan?(proof, plan),
           attrs <- attributes([a1, b1], proof),
           {:ok, row} <- ConsolidatedConclusions.store(attrs) do
        # A fresh, validated proof can settle a specifically missing-year
        # issue. Conflicting proofs require a separate adjudication step.
        :ok = CognitiveIssues.resolve_missing_year(row)
        # Optional hippocampal association indexes the *durable* proof ID,
        # never a generated answer. Failure to index cannot invalidate the
        # already committed Core derivation.
        if Application.get_env(:core, :semantic_associative_recall_enabled?, false) do
          _ = SemanticAssociations.remember(
            row.fingerprint, [a1["subject"], b1["subject"]]
          )
        end

        {:ok, row}
      else
        _ -> {:error, :not_authorized_for_consolidation}
      end
    else
      :disabled
    end
  rescue
    error ->
      Logger.warning("[Consolidation] capture unavailable: #{Exception.message(error)}")
      {:error, :consolidation_store_unavailable}
  catch
    :exit, reason ->
      Logger.warning("[Consolidation] capture exit: #{inspect(reason)}")
      {:error, :consolidation_store_unavailable}
  end

  def capture(_, _), do: {:error, :invalid_context}

  @doc """
  Reconstruct the original premise claims and RE-DERIVE the conclusion.
  Neither the stored prose nor stored derivation is accepted as authority.

  Refuse real-time requests and historical year mismatches. If competing
  stored results disagree for the same year, fail closed and let retrieval
  resolve the conflict instead of choosing a convenient winner.
  """
  @spec recall(map()) :: map()
  def recall(context) when is_map(context) do
    if enabled?() do
      names = PopulationComparison.comparison_subjects(context)
      user_text = Map.get(context, :user_text, "")

      cond do
        length(names) != 2 -> context
        is_map(Map.get(context, :web_evidence)) -> context
        not is_binary(user_text) -> context
        Regex.match?(@freshness, user_text) -> context
        true -> recall_comparison(context, names, user_text)
      end
    else
      context
    end
  rescue
    error ->
      Logger.warning("[Consolidation] recall unavailable: #{Exception.message(error)}")
      context
  catch
    :exit, reason ->
      Logger.warning("[Consolidation] recall exit: #{inspect(reason)}")
      context
  end

  def recall(other), do: other

  defp recall_comparison(context, names, user_text) do
    desired_year =
      case Regex.scan(~r/\b(?:19|20)\d{2}\b/u, user_text) |> List.flatten() |> Enum.uniq() do
        [] -> :unspecified
        [text] -> String.to_integer(text)
        _ -> :ambiguous
      end

    if desired_year == :ambiguous do
      context
    else
      remembered = ConsolidatedConclusions.recent(pair_key(names))

      candidates =
        if is_integer(desired_year),
          do: Enum.filter(remembered, &(&1.reference_year == desired_year)),
          else: remembered

      case candidates do
        [] ->
          # No direct A-vs-C proof: try composing two independently stored
          # source-backed comparisons A-vs-B and B-vs-C before giving up.
          composed = PopulationChain.recall(context, names, desired_year)

          cond do
            is_map(Map.get(composed, :web_evidence)) or
                is_map(Map.get(composed, :consolidation_issue)) ->
              composed

            is_integer(desired_year) and remembered != [] ->
              open_issue(context, names, :missing_dated_evidence, desired_year,
                %{stored_years: Enum.map(remembered, & &1.reference_year) |> Enum.uniq()})

            true ->
              context
          end

        [first | _] ->
          same_year = Enum.filter(candidates, &(&1.reference_year == first.reference_year))

          if unambiguous?(same_year) do
            recall_unambiguous(context, names, first)
          else
            open_issue(context, names, :contradictory_premises, first.reference_year,
              %{stored_proof_ids: Enum.map(same_year, & &1.id)})
          end
      end
    end
  end

  defp recall_unambiguous(context, names, first) do
    with true <- valid_premise_record?(first),
               {:ok, stored_claims} <- rehydrate_claims(first.premises),
               {:ok, claims} <- reorder_claims(stored_claims, names),
               true <- Enum.map(claims, & &1.subject) == names,
               true <- Enum.all?(claims, &(&1.census_year == first.reference_year)),
               evidence <- evidence_for(names, claims),
               derived <- context |> Map.put(:web_evidence, evidence) |> PopulationComparison.attach(),
               %{kind: :population_comparison} = proof <- get_in(derived, [:web_evidence, :support]),
               true <- valid_stored_proof?(first.derivation, proof),
               frame <- ResponseFrame.build(derived),
               %{kind: :verified_population_comparison} <- AuthoritativeAnswerPlan.build(
                 Map.put(derived, :response_frame, frame)) do
            Map.put(derived, :consolidation, %{
              id: first.id,
              kind: :population_comparison,
              origin: :stored_symbolic_premises,
              reference_year: first.reference_year
            })
          else
      _ -> open_issue(context, names, :invalid_derivation, first.reference_year,
        %{stored_proof_ids: [first.id]})
    end
  end

  defp valid_premise_record?(row) do
    row.provenance == "source_supported_symbolic_derivation" and
      row.kind == "population_comparison" and row.source_urls ==
        Enum.map(row.premises, & &1["source_url"])
  end

  defp open_issue(context, names, kind, reference_year, evidence) do
    subject_key = pair_key(names)
    fingerprint =
      :crypto.hash(:sha256,
        :erlang.term_to_binary({subject_key, kind, reference_year}))
      |> Base.encode16(case: :lower)

    next_action =
      case kind do
        :contradictory_premises -> "recheck_sources"
        :missing_dated_evidence -> "find_dated_sources"
        :invalid_derivation -> "review_derivation"
      end

    attrs = %{
      fingerprint: fingerprint,
      kind: Atom.to_string(kind),
      status: "open",
      subject_key: subject_key,
      reference_year: reference_year,
      priority: if(kind == :contradictory_premises, do: 0.9, else: 0.65),
      next_action: next_action,
      evidence: Map.merge(%{subjects: names, relation: "population_comparison"}, evidence)
    }

    case CognitiveIssues.open(attrs) do
      {:ok, issue} ->
        # An unresolved issue is visible to future Curiosity prioritization
        # but must not be treated as an answer or claim of factual support.
        Map.put(context, :consolidation_issue, %{
          id: issue.id, kind: kind, status: :open, next_action: next_action
        })

      _ -> context
    end
  end

  defp unambiguous?(rows) when is_list(rows) and rows != [] do
    [first | _] = rows
    Enum.all?(rows, fn row ->
      row.derivation == first.derivation and
        claim_values(row.premises) == claim_values(first.premises)
    end)
  end

  defp unambiguous?(_), do: false

  defp claim_values(claims) when is_list(claims) do
    claims
    |> Enum.map(fn c ->
      {c["subject"], c["value"], c["census_year"],
       c["measurement_type"], c["source_type"], c["source_table"]}
    end)
    |> Enum.sort()
  end

  defp claim_values(_), do: nil

  defp valid_stored_proof?(stored, proof) do
    stored == %{
      "outcome" => Atom.to_string(proof.outcome),
      "winner" => proof.winner,
      "difference" => proof.difference,
      "reference_year" => proof.year
    }
  end

  defp matches_plan?(proof, plan) do
    proof["outcome"] == Atom.to_string(plan.outcome) and
      proof["winner"] == plan.winner and
      proof["difference"] == plan.difference and
      proof["reference_year"] == plan.year
  end

  defp evidence_for(names, claims) do
    %{
      status: :retrieved,
      resolution: :source_supported,
      relation: :population,
      provider: :consolidated_symbolic_memory,
      evidence_origin: :consolidated_symbolic_memory,
      support: %{kind: :population_group, status: :source_supported,
                 subjects: names, results: claims},
      sources: Enum.map(claims, fn claim ->
        %{title: claim.subject, url: claim.source_url,
          provider: claim.source_type,
          excerpt: "Dated population statement (#{claim.census_year}): #{claim.value}"}
      end),
      attempts: [], attempted_count: 0
    }
  end

  defp reorder_claims(claims, names) do
    ordered = Enum.map(names, fn name ->
      Enum.find(claims, &(&1.subject == name))
    end)

    if length(ordered) == 2 and Enum.all?(ordered, &is_map/1) do
      {:ok, ordered}
    else
      {:error, :subject_mismatch}
    end
  end

  defp rehydrate_claims([a, b]) do
    with {:ok, first} <- rehydrate_claim(a),
         {:ok, second} <- rehydrate_claim(b) do
      {:ok, [first, second]}
    end
  end

  defp rehydrate_claims(_), do: {:error, :invalid_premises}

  defp rehydrate_claim(%{} = c) do
    with source when source in @source_types <- source_atom(c["source_type"]),
         measurement when measurement in @measurement_types <-
           measurement_atom(c["measurement_type"]),
         true <- valid_basic?(c) do
      {:ok, %{
        subject: c["subject"],
        value: c["value"],
        census_year: c["census_year"],
        source_url: c["source_url"],
        source_type: source,
        measurement_type: measurement,
        reference_year: c["reference_year"],
        source_table: c["source_table"],
        status: :source_supported
      }}
    else
      _ -> {:error, :invalid_premise}
    end
  end

  defp rehydrate_claim(_), do: {:error, :invalid_premise}

  defp normalize_claims([a, b]) do
    with {:ok, first} <- normalize_claim(a),
         {:ok, second} <- normalize_claim(b) do
      {:ok, [first, second]}
    end
  end

  defp normalize_claims(_), do: {:error, :invalid_premises}

  defp normalize_claim(%{} = c) do
    source = Map.get(c, :source_type)
    measure = Map.get(c, :measurement_type)

    candidate = %{
      "subject" => Map.get(c, :subject),
      "value" => Map.get(c, :value),
      "census_year" => Map.get(c, :census_year),
      "source_url" => Map.get(c, :source_url),
      "source_type" => if(source in @source_types, do: Atom.to_string(source), else: nil),
      "measurement_type" => if(is_atom(measure) and not is_nil(measure) and
        measure in @measurement_types, do: Atom.to_string(measure), else: nil),
      "reference_year" => Map.get(c, :reference_year),
      "source_table" => Map.get(c, :source_table)
    }

    if valid_basic?(candidate) and source in @source_types and
         measure in @measurement_types,
      do: {:ok, candidate},
      else: {:error, :invalid_source_claim}
  end

  defp normalize_claim(_), do: {:error, :invalid_source_claim}

  defp valid_basic?(c) do
    is_binary(c["subject"]) and byte_size(c["subject"]) in 3..100 and
      is_integer(c["value"]) and c["value"] > 0 and
      is_integer(c["census_year"]) and c["census_year"] in 1900..2100 and
      is_binary(c["source_url"]) and byte_size(c["source_url"]) <= 500 and
      String.starts_with?(c["source_url"], "https://") and
      (is_nil(c["source_table"]) or is_binary(c["source_table"])) and
      (is_nil(c["reference_year"]) or is_integer(c["reference_year"]))
  end

  defp source_atom("wikidata"), do: :wikidata
  defp source_atom("wikipedia"), do: :wikipedia
  defp source_atom("statistics_canada"), do: :statistics_canada
  defp source_atom(_), do: :invalid

  defp measurement_atom(nil), do: nil
  defp measurement_atom("official_estimate"), do: :official_estimate
  defp measurement_atom("dated_population_statement"), do: :dated_population_statement
  defp measurement_atom("census"), do: :census
  defp measurement_atom(_), do: :invalid

  defp derivation([a, b]) do
    {winner, outcome} =
      cond do
        a["value"] == b["value"] -> {nil, "equal"}
        a["value"] > b["value"] -> {a["subject"], "greater"}
        true -> {b["subject"], "greater"}
      end

    %{
      "outcome" => outcome,
      "winner" => winner,
      "difference" => abs(a["value"] - b["value"]),
      "reference_year" => a["census_year"]
    }
  end

  defp attributes(claims, derived) do
    names = Enum.map(claims, & &1["subject"])
    canonical = Enum.sort_by(claims, &String.downcase(&1["subject"]))

    fingerprint =
      :crypto.hash(:sha256, :erlang.term_to_binary({canonical, canonical_proof(canonical)}))
      |> Base.encode16(case: :lower)

    %{
      fingerprint: fingerprint,
      pair_key: pair_key(names),
      kind: "population_comparison",
      reference_year: derived["reference_year"],
      premises: claims,
      derivation: derived,
      source_urls: Enum.map(claims, & &1["source_url"]),
      provenance: "source_supported_symbolic_derivation"
    }
  end

  defp canonical_proof([a, b]), do: derivation([a, b])

  defp pair_key(names) do
    names
    |> Enum.map(&(String.trim(&1) |> String.downcase()))
    |> Enum.sort()
    |> Enum.join("|")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end

  defp consolidated_origin?(context) do
    get_in(context, [:web_evidence, :evidence_origin]) in [
      :consolidated_symbolic_memory,
      :composed_symbolic_premises
    ]
  end

  defp enabled?, do: Application.get_env(:core, :symbolic_consolidation_enabled?, false) == true
end
