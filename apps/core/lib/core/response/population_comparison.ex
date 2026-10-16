defmodule Core.Response.PopulationComparison do
  @moduledoc """
  Evaluate a population comparison only from independently sourced counts.

  The current semantic frame supplies subjects + comparison operator, not
  factual values. Claims come from current retrieval or the same-session,
  bounded VerifiedPopulation ledger. We only order figures from the SAME
  reference year; unequal dates cannot establish a time-aligned ranking.
  """

  alias Core.Response.VerifiedPopulation

  @spec recall(map()) :: map()
  def recall(%{} = context) do
    case comparison_subjects(context) do
      [_, _] = names ->
        case VerifiedPopulation.recall(Map.get(context, :session_id), names) do
          %{} = evidence -> Map.put(context, :web_evidence, evidence)
          _ -> context
        end

      _ -> context
    end
  end

  @spec attach(map()) :: map()
  def attach(%{} = context) do
    case {comparison_subjects(context), Map.get(context, :web_evidence)} do
      {[_, _] = names, %{support: %{kind: :population_group,
                                    subjects: stored, results: results}} = evidence}
      when is_list(results) and stored == names and length(results) == 2 ->
        verified? =
          Enum.all?(results, &valid_claim?/1) and
            Enum.map(results, & &1.subject) == names

        cond do
          verified? and same_year?(results) ->
            [first, second] = results

            {outcome, winner, loser, difference} =
              cond do
                first.value > second.value ->
                  {:greater, first.subject, second.subject, first.value - second.value}
                second.value > first.value ->
                  {:greater, second.subject, first.subject, second.value - first.value}
                true ->
                  {:equal, nil, nil, 0}
              end

            proof = %{
              kind: :population_comparison,
              status: :source_supported,
              metric: :population,
              comparator: :greater_than,
              outcome: outcome,
              subjects: names,
              claims: Enum.map(results, &Map.take(&1,
                [:subject, :value, :census_year, :source_url, :source_type,
                 :measurement_type, :reference_year, :source_table])),
              year: first.census_year,
              winner: winner,
              loser: loser,
              difference: difference
            }

            Map.put(context, :web_evidence,
              evidence
              |> Map.put(:resolution, :source_supported)
              |> Map.put(:support, proof)
              |> Map.put(:comparison_status, :evaluated))

          verified? ->
            Map.put(context, :web_evidence,
              Map.put(evidence, :comparison_status, :different_reporting_years))

          true ->
            Map.put(context, :web_evidence,
              Map.put(evidence, :comparison_status, :insufficient_verified_values))
        end

      _ -> context
    end
  end

  def attach(other), do: other

  @spec comparison_subjects(map()) :: list()
  def comparison_subjects(context) when is_map(context) do
    frame = frame(context)

    if Map.get(frame, :type) == :location_question and
         Map.get(frame, :relation) == :population and
         Map.get(frame, :comparison) == :greater do
      names = Map.get(frame, :subjects, [])

      if is_list(names) and length(names) == 2 and
           Enum.all?(names, &(is_binary(&1) and String.trim(&1) != "")) and
           length(Enum.uniq(names)) == 2,
        do: names,
        else: []
    else
      []
    end
  end

  def comparison_subjects(_), do: []

  defp valid_claim?(%{
         subject: subject, status: :source_supported, value: value,
         census_year: year, source_url: url
       })
       when is_binary(subject) and is_integer(value) and value > 0 and
            is_integer(year) and year >= 1900 and is_binary(url),
       do: String.starts_with?(url, "https://")

  defp valid_claim?(_), do: false

  defp same_year?([a, b]) do
    a.census_year == b.census_year and
      Map.get(a, :measurement_type) == Map.get(b, :measurement_type) and
      compatible_sources?(a, b)
  end

  # A census-year Wikidata claim and a same-year official population estimate
  # are not interchangeable observations. A mixture is not comparable by
  # default, even if its numerical reference years happen to match.
  defp compatible_sources?(a, b) do
    case {Map.get(a, :measurement_type), Map.get(b, :measurement_type)} do
      {:official_estimate, :official_estimate} ->
        Map.get(a, :source_type) == Map.get(b, :source_type) and
          Map.get(a, :source_table) == Map.get(b, :source_table)

      _ -> true
    end
  end

  defp frame(context) do
    case get_in(context, [:features, :symbolic_frame]) do
      %{type: :location_question} = value -> value
      _ ->
        case get_in(context, [:symbolic_frame, :event]) do
          %{type: :location_question} = value -> value
          _ -> %{}
        end
    end
  end
end
