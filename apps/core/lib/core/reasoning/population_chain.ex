defmodule Core.Reasoning.PopulationChain do
  @moduledoc """
  Bounded, two-premise compositional reasoning over persisted Core proofs.

  Example: supported A > B and B > C for the *same* reporting year and
  compatible measurement imply A > C. Both parents are independently
  revalidated; the bridging B claim must agree exactly, including source.

  This never reasons from assistant prose or unverified chat messages.
  It does not invent a source, turn a conjecture into a fact, or persist
  a derived conclusion as though it had been retrieved independently.
  """
  alias Core.Response.{AuthoritativeAnswerPlan, PopulationComparison, ResponseFrame}
  alias Db.{CognitiveIssues, ConsolidatedConclusion, ConsolidatedConclusions}

  @source_types ~w(wikidata wikipedia statistics_canada)
  @measurement_types [nil, "official_estimate", "dated_population_statement", "census"]

  @spec recall(map(), [String.t()], :unspecified | integer()) :: map()
  def recall(context, [first, last] = subjects, requested_year)
      when is_map(context) and is_binary(first) and is_binary(last) and
             (requested_year == :unspecified or is_integer(requested_year)) do
    if Application.get_env(:core, :symbolic_composition_enabled?, false) do
      case {ConsolidatedConclusions.for_subject(first),
            ConsolidatedConclusions.for_subject(last)} do
        {{:ok, []}, {:ok, []}} ->
          context

        {{:ok, left}, {:ok, right}} ->
          derive_from_rows(context, subjects, left, right, requested_year)

        _ ->
          issue(context, subjects, requested_year, :missing_reasoning_link,
            [], %{reason: "candidate_limit_exceeded"})
      end
    else
      context
    end
  rescue
    _ -> context
  catch
    :exit, _ -> context
  end

  def recall(context, _, _), do: context

  defp derive_from_rows(context, subjects, left, right, requested_year) do
    all_rows = Enum.uniq_by(left ++ right, & &1.id)
    invalid = Enum.reject(all_rows, &valid_parent?/1)
    valid_left = Enum.filter(left, &valid_parent?/1)
    valid_right = Enum.filter(right, &valid_parent?/1)

    paths =
      for a <- valid_left, c <- valid_right, a.id != c.id,
          first_claim <- [claim_for(a, hd(subjects))], not is_nil(first_claim),
          last_claim <- [claim_for(c, List.last(subjects))], not is_nil(last_claim),
          bridge_a <- [other_claim(a, hd(subjects))], not is_nil(bridge_a),
          bridge_c <- [other_claim(c, List.last(subjects))], not is_nil(bridge_c),
          bridge_a["subject"] == bridge_c["subject"],
          bridge_a["subject"] not in subjects do
        classify_path(a, c, first_claim, bridge_a, bridge_c, last_claim)
      end

    eligible =
      case requested_year do
        year when is_integer(year) ->
          Enum.filter(paths, &(&1.year == year))

        :unspecified ->
          latest =
            case Enum.map(paths, & &1.year) do
              [] -> nil
              years -> Enum.max(years)
            end
          if is_integer(latest), do: Enum.filter(paths, &(&1.year == latest)), else: []
      end

    ids = Enum.map(all_rows, & &1.id)

    cond do
      invalid != [] ->
        issue(context, subjects, requested_year, :invalid_derivation,
          Enum.map(invalid, & &1.id), %{reason: "unverified_parent_record"})

      eligible == [] ->
        issue(context, subjects, requested_year, :missing_reasoning_link,
          ids, %{reason: "no_shared_intermediate_premise"})

      Enum.any?(eligible, &(&1.status == :conflict)) ->
        issue(context, subjects, hd(eligible).year, :contradictory_premises,
          ids, %{reason: "incompatible_intermediate_evidence"})

      true ->
        supported = Enum.filter(eligible, &(&1.status == :supported))

        if supported == [] do
          issue(context, subjects, hd(eligible).year, :missing_reasoning_link,
            ids, %{reason: "no_valid_transitive_order"})
        else
          signatures =
            supported
            |> Enum.map(&signature/1)
            |> Enum.uniq()

          if length(signatures) == 1 do
            supported
            |> Enum.sort_by(&{hd(&1.parent_ids), List.last(&1.parent_ids)})
            |> hd()
            |> attach_proof(context, subjects)
          else
            issue(context, subjects, hd(supported).year, :contradictory_premises,
              ids, %{reason: "multiple_incompatible_transitive_proofs"})
          end
        end
    end
  end

  defp classify_path(left, right, a, middle_a, middle_b, c) do
    same_year? =
      a["census_year"] == middle_a["census_year"] and
        a["census_year"] == middle_b["census_year"] and
        a["census_year"] == c["census_year"]

    comparable? =
      Enum.uniq(Enum.map([a, middle_a, middle_b, c], & &1["measurement_type"])) |> length() == 1 and
        Enum.uniq(Enum.map([a, middle_a, middle_b, c], & &1["source_type"])) |> length() == 1 and
        (a["measurement_type"] != "official_estimate" or
           length(Enum.uniq(Enum.map([a, middle_a, middle_b, c],
             & &1["source_table"]))) == 1)

    bridge_consistent? = middle_a == middle_b

    monotonic? =
      (a["value"] > middle_a["value"] and middle_a["value"] > c["value"]) or
        (a["value"] < middle_a["value"] and middle_a["value"] < c["value"])

    status =
      cond do
        not same_year? -> :incompatible
        not comparable? -> :incompatible
        not bridge_consistent? -> :conflict
        not monotonic? -> :not_transitive
        true -> :supported
      end

    %{
      status: status, year: a["census_year"], first: a, last: c,
      middle: middle_a, parent_ids: Enum.sort([left.id, right.id])
    }
  end

  defp signature(path) do
    {
      path.year,
      path.first["value"],
      path.last["value"],
      path.first["source_type"],
      path.last["source_type"],
      path.first["measurement_type"],
      path.last["measurement_type"],
      path.first["source_table"],
      path.last["source_table"]
    }
  end

  defp attach_proof(path, context, subjects) do
    with {:ok, [a, c]} <- to_claims([path.first, path.last]),
         evidence <- %{
           status: :retrieved,
           resolution: :source_supported,
           relation: :population,
           provider: :composed_symbolic_memory,
           evidence_origin: :composed_symbolic_premises,
           support: %{
             kind: :population_group, status: :source_supported,
             subjects: subjects, results: [a, c]
           },
           sources: Enum.map([a, c], fn claim ->
             %{title: claim.subject, url: claim.source_url,
               provider: claim.source_type,
               excerpt: "Dated population statement (#{claim.census_year}): #{claim.value}"}
           end),
           attempted_count: 0,
           attempts: []
         },
         derived <- context |> Map.put(:web_evidence, evidence) |> PopulationComparison.attach(),
         %{kind: :population_comparison} <- get_in(derived, [:web_evidence, :support]),
         frame <- ResponseFrame.build(derived),
         %{kind: :verified_population_comparison} <- AuthoritativeAnswerPlan.build(
           Map.put(derived, :response_frame, frame)) do
      :ok = CognitiveIssues.resolve_reasoning_link(
        subject_key(subjects), path.year, path.parent_ids
      )

      :telemetry.execute(
        [:core, :reasoning, :composition],
        %{count: 1},
        %{rule: :transitive_population_order, status: :supported,
          parent_proof_ids: path.parent_ids, reference_year: path.year}
      )

      Map.put(derived, :composition, %{
        rule: :transitive_population_order,
        intermediate_subject: path.middle["subject"],
        parent_proof_ids: path.parent_ids,
        reference_year: path.year,
        origin: :stored_independent_proofs
      })
    else
      _ ->
        issue(context, subjects, path.year, :invalid_derivation,
          path.parent_ids, %{reason: "final_authorization_failed"})
    end
  end

  defp claim_for(row, subject),
    do: Enum.find(row.premises, &(&1["subject"] == subject))

  defp other_claim(row, subject),
    do: Enum.find(row.premises, &(&1["subject"] != subject))

  defp to_claims([a, b]) do
    {:ok, Enum.map([a, b], fn c ->
      %{
        subject: c["subject"], value: c["value"],
        census_year: c["census_year"], source_url: c["source_url"],
        source_type: source_type(c["source_type"]),
        measurement_type: measurement_type(c["measurement_type"]),
        source_table: c["source_table"], reference_year: c["reference_year"],
        status: :source_supported
      }
    end)}
  end

  defp source_type("wikidata"), do: :wikidata
  defp source_type("wikipedia"), do: :wikipedia
  defp source_type("statistics_canada"), do: :statistics_canada
  defp measurement_type(nil), do: nil
  defp measurement_type("official_estimate"), do: :official_estimate
  defp measurement_type("dated_population_statement"), do: :dated_population_statement
  defp measurement_type("census"), do: :census

  # Validate BOTH parent derivations, source claim shapes, their original
  # fingerprint and the exact canonical storage contract used by Consolidation.
  defp valid_parent?(%ConsolidatedConclusion{} = row) do
    case row.premises do
      [a, b] when is_map(a) and is_map(b) ->
        claims = [a, b]

        if Enum.all?(claims, &valid_claim?/1) do
          canonical = Enum.sort_by(claims, &String.downcase(&1["subject"]))
          expected = derivation(a, b)

          digest =
            :crypto.hash(:sha256, :erlang.term_to_binary({
              canonical, derivation(hd(canonical), List.last(canonical))
            }))
            |> Base.encode16(case: :lower)

          row.kind == "population_comparison" and
            row.provenance == "source_supported_symbolic_derivation" and
            a["subject"] != b["subject"] and
            a["census_year"] == b["census_year"] and
            a["measurement_type"] == b["measurement_type"] and
            a["source_type"] == b["source_type"] and
            (a["measurement_type"] != "official_estimate" or
               a["source_table"] == b["source_table"]) and
            row.reference_year == a["census_year"] and
            row.derivation == expected and
            row.source_urls == Enum.map(claims, & &1["source_url"]) and
            row.fingerprint == digest
        else
          false
        end

      _ -> false
    end
  end

  defp valid_parent?(_), do: false

  defp valid_claim?(c) do
    is_binary(c["subject"]) and byte_size(c["subject"]) in 3..100 and
      is_integer(c["value"]) and c["value"] > 0 and
      is_integer(c["census_year"]) and c["census_year"] in 1900..2100 and
      c["source_type"] in @source_types and
      c["measurement_type"] in @measurement_types and
      is_binary(c["source_url"]) and
      String.starts_with?(c["source_url"], "https://") and
      (is_nil(c["source_table"]) or is_binary(c["source_table"]))
  end

  defp derivation(a, b) do
    {winner, outcome} =
      cond do
        a["value"] == b["value"] -> {nil, "equal"}
        a["value"] > b["value"] -> {a["subject"], "greater"}
        true -> {b["subject"], "greater"}
      end

    %{
      "outcome" => outcome, "winner" => winner,
      "difference" => abs(a["value"] - b["value"]),
      "reference_year" => a["census_year"]
    }
  end

  defp subject_key(subjects) do
    subjects
    |> Enum.map(&(String.downcase(String.trim(&1))))
    |> Enum.sort()
    |> Enum.join("|")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end

  defp issue(context, subjects, year, kind, proof_ids, details) do
    [a, c] = subjects
    subject_key = subject_key(subjects)

    fingerprint =
      :crypto.hash(:sha256, :erlang.term_to_binary({
        subject_key, "population_chain", kind, year
      }))
      |> Base.encode16(case: :lower)

    action =
      case kind do
        :missing_reasoning_link -> "find_linking_premise"
        :contradictory_premises -> "recheck_sources"
        :invalid_derivation -> "review_derivation"
      end

    attrs = %{
      fingerprint: fingerprint, subject_key: subject_key,
      reference_year: if(is_integer(year), do: year, else: nil),
      kind: Atom.to_string(kind), status: "open",
      priority: if(kind == :contradictory_premises, do: 0.9, else: 0.65),
      next_action: action,
      evidence: Map.merge(%{
        subjects: [a, c],
        reasoning_rule: "transitive_population_order",
        stored_proof_ids: proof_ids
      }, details)
    }

    case CognitiveIssues.open(attrs) do
      {:ok, record} ->
        Map.put(context, :consolidation_issue, %{
          id: record.id, kind: kind, status: :open, next_action: action
        })

      _ -> context
    end
  end
end
