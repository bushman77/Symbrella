# Read-only regression for the Kamloops BC → Kamloops article identity gate.
# mix run scripts/probe_place_location.exs
# Synthetic Wikipedia-shaped evidence only; does not call the web,
# write episodic memory, or affect LIFG/synaptic weights.

alias Core.Response.ReflectionLoop
alias Core.Tools.WebRetrieval.{PlaceLocation, RequestRouter}

source = %{
  title: "Kamloops",
  url: "https://en.wikipedia.org/wiki/Kamloops",
  provider: :wikipedia,
  excerpt: "Kamloops (KAM-loops) is a city in south-central British Columbia, Canada, at the confluence of the North and South Thompson Rivers, which join to become the Thompson River in Kamloops, and east of Kamloops Lake."
}

unrelated = %{
  title: "Kamloops Indian Residential School",
  url: "https://en.wikipedia.org/wiki/Kamloops_Indian_Residential_School",
  provider: :wikipedia,
  excerpt: "Kamloops Indian Residential School was a former school in British Columbia, Canada."
}

question = "where is kamloops bc"
route = RequestRouter.route(%{user_text: question})

context = %{
  user_text: question,
  web_evidence: %{
    status: :retrieved, resolution: :reference_sources,
    request_type: :general_search,
    query: "kamloops bc",
    sources: [unrelated, source]
  }
}

answer = PlaceLocation.answer(context)

{:ok, published, reflection} =
  ReflectionLoop.review(question, "Kamloops is in British Columbia.", context)

wrong_province =
  context
  |> put_in([:web_evidence, :query], "kamloops ab")
  |> Map.put(:user_text, "where is kamloops ab")
  |> PlaceLocation.answer()

IO.puts("\n=== Kamloops source-matched reference response (READ ONLY) ===")
IO.inspect(%{
  request_route: route,
  exact_article_match: answer,
  reflection_status: reflection.status,
  issues: reflection.issues,
  corrected_response: published,
  wrong_province: wrong_province,
  made_external_requests?: false,
  wrote_brain_or_memory?: false
}, pretty: true)

if match?({:ok, _}, answer) and
     :location_reference_unaligned in reflection.issues and
     wrong_province == :no_support do
  IO.puts("PASS: the qualified city is source-matched, while wrong provinces fail closed.")
else
  raise "Kamloops source-matched location probe failed"
end
