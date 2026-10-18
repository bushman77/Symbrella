defmodule Core.Tools.WebRetrieval.PlaceLocationTest do
  use ExUnit.Case, async: true

  alias Core.Response.ReflectionLoop
  alias Core.Tools.WebRetrieval.PlaceLocation

  @kamloops_lead "Kamloops (KAM-loops) is a city in south-central British Columbia, Canada, at the confluence of the North and South Thompson Rivers, which join to become the Thompson River in Kamloops, and east of Kamloops Lake."
  @kamloops_source %{
    title: "Kamloops",
    url: "https://en.wikipedia.org/wiki/Kamloops",
    provider: :wikipedia,
    excerpt: @kamloops_lead
  }
  @school_source %{
    title: "Kamloops Indian Residential School",
    url: "https://en.wikipedia.org/wiki/Kamloops_Indian_Residential_School",
    provider: :wikipedia,
    excerpt: "Kamloops Indian Residential School was a former residential school in British Columbia, Canada."
  }

  defp context(place, sources) do
    %{
      user_text: "where is #{place}",
      web_evidence: %{
        status: :retrieved,
        resolution: :reference_sources,
        request_type: :general_search,
        query: place,
        sources: sources
      }
    }
  end

  test "a legitimate unqualified article answers a province-qualified Kamloops question" do
    for place <- ["kamloops bc", "Kamloops, BC", "Kamloops, British Columbia"] do
      assert {:ok, answer} = PlaceLocation.answer(context(place, [
        @kamloops_source, @school_source
      ]))

      assert answer ==
        "According to Wikipedia, Kamloops is a city in south-central " <>
        "British Columbia, Canada. Source: https://en.wikipedia.org/wiki/Kamloops"
      refute answer =~ "Residential School"
    end
  end

  test "the valid Kamloops article answers even when an irrelevant search hit comes first" do
    assert {:ok, answer} =
      PlaceLocation.answer(context("kamloops bc", [
        @school_source, @kamloops_source
      ]))
    assert answer =~ "Kamloops is a city in south-central British Columbia, Canada."
  end

  test "normal exact titled place articles continue to pass" do
    article = %{
      provider: :wikipedia,
      title: "Maple Ridge, British Columbia",
      url: "https://en.wikipedia.org/wiki/Maple_Ridge%2C_British_Columbia",
      excerpt: "Maple Ridge is a city in British Columbia, Canada. It lies east of Vancouver."
    }

    assert {:ok, answer} =
      PlaceLocation.answer(context("Maple Ridge BC", [article]))

    assert answer =~ "Maple Ridge is a city in British Columbia, Canada."
    assert answer =~ "https://en.wikipedia.org/wiki/Maple_Ridge%2C_British_Columbia"
  end

  test "unqualified title never overrides a different requested province" do
    assert :no_support =
      PlaceLocation.answer(context("kamloops ab", [@kamloops_source]))

    alberta = %{@kamloops_source |
      excerpt: "Kamloops is a city in Alberta, Canada."}

    assert :no_support =
      PlaceLocation.answer(context("kamloops bc", [alberta]))
  end

  test "an unrelated title, non-Wikipedia host, and unsupported excerpt are rejected" do
    wrong_host = %{@kamloops_source |
      url: "https://example.com/wiki/Kamloops"}
    wrong_article = %{@kamloops_source |
      url: "https://en.wikipedia.org/wiki/Other_Kamloops"}
    non_location = %{@kamloops_source |
      excerpt: "Kamloops is a beautiful destination with many attractions."}
    numeric_lead = %{@kamloops_source |
      excerpt: "Kamloops is 17 km from a city in British Columbia, Canada."}

    for source <- [
      @school_source, wrong_host, wrong_article, non_location, numeric_lead
    ] do
      assert :no_support =
        PlaceLocation.answer(context("kamloops bc", [source]))
    end
  end

  test "the actual response review repairs an uncited Kamloops draft to the cited location" do
    ctx = context("kamloops bc", [@kamloops_source, @school_source])

    {:ok, response, reflection} =
      ReflectionLoop.review(ctx.user_text,
        "Kamloops is in the province of British Columbia.", ctx)

    assert reflection.status == :repair
    assert :reference_uncited in reflection.issues
    assert :location_reference_unaligned in reflection.issues
    assert reflection.applied?
    assert response ==
      "According to Wikipedia, Kamloops is a city in south-central " <>
      "British Columbia, Canada. Source: https://en.wikipedia.org/wiki/Kamloops"
    refute response =~ "couldn't establish a complete answer"
    refute response =~ "Residential School"
  end

  test "if the geographic article lacks source-supported lead, only references are returned" do
    ctx = context("kamloops bc", [@school_source])

    assert :no_support = PlaceLocation.answer(ctx)

    {:ok, answer, reflection} =
      ReflectionLoop.review(ctx.user_text, "Kamloops is in British Columbia.", ctx)

    assert :reference_uncited in reflection.issues
    assert answer =~ "I found reference sources"
    refute answer =~ "According to Wikipedia, Kamloops is a city"
  end
end
