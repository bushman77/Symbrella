# mix run scripts/diagnose_live_geographic_distance.exs
#
# READ-ONLY geographical provider diagnosis. This intentionally performs
# bounded live HTTP requests to configured reference/coordinate providers.
# It does NOT call the LLM or record factual claims in hippocampal memory,
# WM, dictionary choices, or synaptic learning.

alias Core.Semantic.EventFrames
alias Core.Tools.WebRetrieval
alias Core.Tools.WebRetrieval.{Grounding, CanadianGeoNames, Wikidata}

questions = [
  "how far from maple ridge is from kamloops",
  "how far is maple ridge bc from kamloops bc"
]

IO.puts("\n=== Live geographic evidence diagnosis ===")
config = Application.get_env(:core, :web_retrieval, [])
IO.inspect(%{
  configured_retrieval_enabled?:
    Keyword.get(config, :enabled?, false),
  coordinate_provider_override?: Keyword.has_key?(config, :coordinate_provider),
  max_attempts: Keyword.get(config, :max_attempts, 3)
}, label: "Runtime policy (non-secret)")

for question <- questions do
  frame = EventFrames.location_question(question)
  context = %{
    user_text: question,
    features: %{intent: :ask, confidence: 0.9, symbolic_frame: frame},
    symbolic_frame: %{event: frame}
  }

  route = WebRetrieval.route_for(context)
  result = WebRetrieval.enrich(context, enabled?: true)
  evidence = Map.get(result, :web_evidence, %{})

  attempts =
    evidence
    |> Map.get(:attempts, [])
    |> Enum.map(&Map.take(&1, [
      :kind, :provider, :query, :status, :reason, :source_count
    ]))

  support = Map.get(evidence, :support)
  support_summary =
    if is_map(support),
      do: Map.take(support, [
        :kind, :status, :method, :subject, :target, :distance_km,
        :coordinate_provider, :sources
      ]),
      else: nil

  IO.inspect(%{
    question: question,
    parsed_roles: Map.take(frame || %{}, [
      :relation, :subject, :target, :grounding_status
    ]),
    route: route,
    evidence_status: Map.get(evidence, :status),
    evidence_resolution: Map.get(evidence, :resolution),
    attempts: attempts,
    support: support_summary,
    accepted_by_grounding?: Grounding.available_support?(result),
    corrected_answer:
      if(Grounding.available_support?(result),
        do: Grounding.align("", result), else: :no_verified_distance)
  }, label: "Live resolution", pretty: true, limit: :infinity)
end

# The resolver trace alone cannot reveal whether a failed first lookup
# arose from a place ambiguity vs an API transport failure if its provider
# failed before returning an error tuple. Direct calls preserve error codes.
for provider <- [CanadianGeoNames, Wikidata] do
  result = provider.proximity("Maple Ridge, BC", "Kamloops, BC",
    timeout_ms: 6_000)

  details =
    case result do
      {:ok, support} ->
        {:ok, Map.take(support, [
          :kind, :status, :method, :distance_km,
          :subject, :target, :sources
        ])}

      {:error, reason} ->
        {:error, reason}

      other ->
        {:unrecognized_result, inspect(other, limit: 5, printable_limit: 200)}
    end

  IO.inspect(details, label: "Direct #{inspect(provider)}", pretty: true,
    limit: :infinity)
end

IO.puts("""
This live diagnostic performed HTTP requests; its results are source/provider
evidence, not conversation memory. Numeric conclusions are authorized only
when both named entities are validated and Grounding accepts the proof.
""")
