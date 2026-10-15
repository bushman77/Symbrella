defmodule Core.Tools.PlaceLocationTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval.PlaceLocation
  alias Core.Response.ReflectionLoop

  @maple %{
    provider: :wikipedia,
    title: "Maple Ridge, British Columbia",
    url: "https://en.wikipedia.org/wiki/Maple_Ridge%2C_British_Columbia",
    excerpt: "Maple Ridge is a city in British Columbia, Canada. " <>
      "It is located east of Vancouver. Its population is 90,990."
  }

  defp context(question, query, sources) do
    %{
      user_text: question,
      features: %{intent: :ask, confidence: 0.81},
      web_evidence: %{
        status: :retrieved,
        resolution: :reference_sources,
        request_type: :general_search,
        relation: nil,
        query: query,
        sources: sources,
        support: nil
      }
    }
  end

  test "matching article lead answers a static place question with attribution" do
    ctx = context("where is maple ridge bc", "maple ridge bc", [
      @maple,
      %{provider: :wikipedia, title: "Maple Ridge East",
        url: "https://en.wikipedia.org/wiki/Maple_Ridge_East",
        excerpt: "Maple Ridge East is a provincial electoral district in British Columbia."}
    ])

    assert {:ok, answer} = PlaceLocation.answer(ctx)
    assert answer =~ "Maple Ridge is a city in British Columbia, Canada."
    assert answer =~ @maple.url
    refute answer =~ "90,990"
    refute answer =~ "Maple Ridge East"

    {:ok, ^answer, review} =
      ReflectionLoop.review(ctx.user_text, "I cannot establish the place.", ctx)

    assert review.status == :repair
    assert :location_reference_unaligned in review.issues

    # Even a draft that cites the source must not smuggle in another claim.
    {:ok, ^answer, review2} =
      ReflectionLoop.review(ctx.user_text,
        "Maple Ridge is 18 miles from Kamloops. Source: " <> @maple.url, ctx)

    assert review2.status == :repair

    {:ok, ^answer, accepted} = ReflectionLoop.review(ctx.user_text, answer, ctx)
    assert accepted.status == :accept
  end

  test "source identity, province, and introductory location statement are mandatory" do
    ctx = context("Where is Maple Ridge, BC?", "Maple Ridge, BC", [@maple])
    assert match?({:ok, _}, PlaceLocation.answer(ctx))

    off_topic = Map.put(@maple, :title, "Maple Ridge East")
    assert :no_support == PlaceLocation.answer(%{ctx | web_evidence:
      %{ctx.web_evidence | sources: [off_topic]}})

    misidentified = %{@maple | title: "Maple Ridge, Alberta",
      url: "https://en.wikipedia.org/wiki/Maple_Ridge%2C_Alberta"}
    assert :no_support == PlaceLocation.answer(%{ctx | web_evidence:
      %{ctx.web_evidence | sources: [misidentified]}})

    ungrounded = %{@maple | excerpt:
      "Maple Ridge has a population of 90,990. It is somewhere in Canada."}
    assert :no_support == PlaceLocation.answer(%{ctx | web_evidence:
      %{ctx.web_evidence | sources: [ungrounded]}})

    not_wikipedia = %{@maple | provider: :other}
    assert :no_support == PlaceLocation.answer(%{ctx | web_evidence:
      %{ctx.web_evidence | sources: [not_wikipedia]}})

    assert :no_support == PlaceLocation.answer(context(
      "Where is my sister right now?", "my sister right now", [@maple]))

    assert :no_support == PlaceLocation.answer(%{ctx | web_evidence:
      %{ctx.web_evidence | status: :unavailable}})
  end

  test "the generic reference-review path stays cautious for unrelated subjects" do
    ctx = context("Who invented the transistor?", "transistor invention", [@maple])
    assert :no_support == PlaceLocation.answer(ctx)

    {:ok, final, reflection} =
      ReflectionLoop.review(ctx.user_text, "I'll look that up for you.", ctx)

    assert reflection.status == :repair
    assert :search_not_answered in reflection.issues
    assert final =~ "I found reference sources"
    refute final =~ "Maple Ridge is a city"
  end

  test "ordinary unqualified city questions work with exact article titles" do
    page = %{provider: :wikipedia, title: "Victoria",
      url: "https://en.wikipedia.org/wiki/Victoria",
      excerpt: "Victoria is the capital city of British Columbia, Canada. Later material."}
    ctx = context("Where is Victoria?", "Victoria", [page])

    assert {:ok, answer} = PlaceLocation.answer(ctx)
    assert answer =~ "Victoria is the capital city of British Columbia, Canada."
    assert answer =~ page.url
  end
end
