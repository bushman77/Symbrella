defmodule SymbrellaWeb.SocialUnderstanding do
  @moduledoc """
  Conservative, read-only synthesis of existing semantic shadow outputs.

  Never turns grammatical candidates, lexical matches, or external claims
  into verified meanings. No model calls, memory writes, or external actions.
  """

  def build(segments) when is_list(segments) do
    analyzed = Enum.filter(segments, &(&1.status == :analyzed))
    supported = Enum.filter(analyzed, &supported_meaning?/1)
    propositions = Enum.filter(analyzed, &supported_proposition?/1)
    questions = Enum.filter(supported, &(get_in(&1, [:result, :sentence_meaning, :speech_act]) == :request_information))

    findings =
      []
      |> maybe_add(analyzed != [], %{
        title: "Lexical analysis completed",
        detail: "LIFG and ATL generated candidate word senses and constituents for the sampled text.",
        basis: "Semantic shadow stage outputs",
        level: :structural
      })
      |> maybe_add(propositions != [], %{
        title: "Attributed statement structure detected",
        detail: "A grammatical proposition was extracted, but remains an unverified statement by its external author.",
        basis: "ReportedProposition structural interpretation",
        level: :attributed
      })
      |> maybe_add(questions != [], %{
        title: "Information-request structure detected",
        detail: "A question was compositionally interpreted from the current grammar and ATL evidence; no answer or external facts are established.",
        basis: "SentenceMeaning speech act and resolved structure",
        level: :provisional
      })
      |> maybe_add(supported != [], %{
        title: "Sentence interpretation available",
        detail: "The sentence-meaning stage returned an interpretation; its source remains unverified.",
        basis: "SentenceMeaning stage status",
        level: :provisional
      })

    gaps =
      []
      |> maybe_add(analyzed == [], "No successful cognitive probe results were available.")
      |> maybe_add(supported == [], "Sentence-level meaning has not been established by this probe.")
      |> maybe_add(propositions == [], "No supported attributed proposition was established.")
      |> maybe_add(Enum.any?(analyzed, &get_in(&1, [:result, :acc_shadow, :uncertain?])),
        "Lexical ambiguity was flagged in at least one sample.")
      |> maybe_add(Enum.any?(analyzed, &(get_in(&1, [:result, :syntax, :audit, :token_limit_reached?]) == true)),
        "At least one sample exceeded the probe's token limit.")
      |> maybe_add(Enum.any?(segments, &(&1.status != :analyzed)),
        "One or more sampled segments could not be analyzed.")

    %{
      status: if(supported != [], do: :provisional_interpretation, else: :meaning_unresolved),
      headline: if(supported != [],
        do: "Partial sentence interpretation available",
        else: "Words and grammar inspected; conversation meaning remains unresolved"),
      explanation: "This report distinguishes candidate linguistic structure from an understanding of the conversation. It does not infer the author's intention from word matches alone.",
      findings: findings,
      gaps: gaps,
      interpreted_segments: length(supported),
      attributed_propositions: length(propositions),
      information_requests: length(questions)
    }
  end

  defp supported_meaning?(segment) do
    get_in(segment, [:result, :sentence_meaning, :status]) in
      [:structurally_interpreted, :interpreted, :understood, :resolved]
  end

  defp supported_proposition?(segment) do
    get_in(segment, [:result, :reported_proposition, :status]) ==
      :attributed_structural_interpretation
  end

  defp maybe_add(items, true, item), do: items ++ [item]
  defp maybe_add(items, _, _), do: items
end
