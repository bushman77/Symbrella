# mix run scripts/probe_distance_question_grounding.exs
#
# READ ONLY: exercises the distance utterance's actual Core semantic
# parser, retrieval router and response review with synthetic evidence.
# Does not contact Wikidata or NRCan; no model, memory or synapse writes.

alias Core.Semantic.EventFrames
alias Core.Tools.WebRetrieval.RequestRouter
alias Core.Response.ReflectionLoop

question = "how far from maple ridge is from kamloops"
draft =
  "According to the information provided, Kamloops is approximately " <>
    "30 kilometers (19 miles) from Maple Ridge. Wikipedia"

frame = EventFrames.location_question(question)
context = %{
  user_text: question,
  features: %{intent: :ask, conf: 0.81, symbolic_frame: frame},
  symbolic_frame: %{event: frame}
}

route = RequestRouter.route(context)
{:ok, unsupported_reply, unsupported_review} =
  ReflectionLoop.review(question, draft, context)

# Artificial 229.6 km proof exercises the final formatting contract.
# Numeric content is simulated, NOT a new external measurement.
simulated_sources = [
  %{title: "Maple Ridge", provider: :wikidata,
    url: "https://www.wikidata.org/wiki/Q16740"},
  %{title: "Kamloops", provider: :wikidata,
    url: "https://www.wikidata.org/wiki/Q473209"}
]

with_source =
  Map.put(context, :web_evidence, %{
    status: :retrieved,
    resolution: :source_supported,
    support: %{
      kind: :proximity, status: :source_supported,
      subject: "maple ridge", target: "kamloops",
      method: :haversine, distance_km: 229.6,
      sources: simulated_sources
    },
    sources: simulated_sources
  })

{:ok, cited_reply, cited_review} =
  ReflectionLoop.review(question, draft, with_source)

IO.puts("\n=== Distance intent and numeric grounding (READ ONLY) ===")
IO.inspect(%{
  original_question: question,
  source_frame: frame,
  routing: route,
  raw_model_draft: draft,
  unsupported_issues: unsupported_review.issues,
  unsupported_outcome: unsupported_reply,
  simulated_evidence_issues: cited_review.issues,
  simulated_evidence_outcome: cited_reply,
  external_requests?: false,
  memory_or_synapse_writes?: false
}, pretty: true, limit: :infinity)

unless is_map(frame) and frame.relation == :proximity and
    frame.subject == "maple ridge" and frame.target == "kamloops" and
    is_map(route) and route.relation == :proximity and
    :geographic_claim_unverified in unsupported_review.issues and
    not String.contains?(unsupported_reply, "30 kilometers") and
    :geographic_claim_unverified in cited_review.issues and
    String.contains?(cited_reply, "229.6 km") and
    String.contains?(cited_reply, "straight line") do
  raise "Distance intent/grounding probe failed"
end

IO.puts("PASS: malformed wording still requests sourced distance; unsupported 30 km cannot pass.")
