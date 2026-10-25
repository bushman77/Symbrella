defmodule Core.Tools.WebRetrieval.PlaceScopeTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.EventFrames
  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.{Grounding, PlaceScope}

  defmodule EmptyReference do
    def search(_, _), do: {:ok, []}
  end

  # This is an injected offline coordinate source. The sample number is
  # synthetic; the test is about source identity and routing only.
  defmodule ProvinceAwareCoords do
    def proximity("maple ridge, BC" = subject, "kamloops, BC" = target, _opts) do
      {:ok, %{
        kind: :proximity, status: :source_supported,
        method: :haversine, distance_km: 228.8,
        subject: subject, target: target,
        coordinate_provider: :natural_resources_canada,
        sources: [
          %{title: "Maple Ridge", provider: :natural_resources_canada,
            url: "https://geogratis.gc.ca/services/geoname/en/geonames/JAJYR"},
          %{title: "Kamloops", provider: :natural_resources_canada,
            url: "https://geogratis.gc.ca/services/geoname/en/geonames/JAFNW"}
        ]
      }}
    end

    def proximity(_, _, _), do: {:error, :ambiguous_place}
  end

  defp context(question \\ "how far from maple ridge is from kamloops") do
    frame = EventFrames.location_question(question)

    %{
      user_text: question,
      features: %{intent: :ask, confidence: 0.9, symbolic_frame: frame},
      symbolic_frame: %{event: frame}
    }
  end

  @prior_location [
    %{"role" => "user", "content" => "where is Kamloops BC"},
    %{"role" => "assistant", "content" => "Kamloops is in BC."}
  ]

  test "recent user-qualified named city scopes provider lookup, not cognition" do
    original = context()
    scoped = PlaceScope.for_lookup(original, @prior_location)

    assert get_in(scoped, [:features, :symbolic_frame, :subject]) ==
      "maple ridge, BC"
    assert get_in(scoped, [:features, :symbolic_frame, :target]) ==
      "kamloops, BC"
    assert scoped.geographic_lookup_scope == %{
      status: :user_qualified_hint, region: "BC",
      verified?: false, provider_must_verify?: true
    }

    assert get_in(original, [:features, :symbolic_frame, :subject]) ==
      "maple ridge"
    assert get_in(original, [:features, :symbolic_frame, :target]) ==
      "kamloops"
    refute Map.has_key?(original, :geographic_lookup_scope)
  end

  test "retrieval uses prior user jurisdiction hint but final support is provider-verified" do
    original = context()

    result = WebRetrieval.enrich(original,
      enabled?: true,
      provider: EmptyReference,
      coordinate_provider: ProvinceAwareCoords,
      history: @prior_location
    )

    assert result.web_evidence.resolution == :source_supported
    assert result.web_evidence.support.subject == "maple ridge, BC"
    assert result.web_evidence.support.target == "kamloops, BC"
    assert result.web_evidence.lookup_scope == %{
      status: :user_qualified_hint, region: "BC",
      verified?: false, provider_must_verify?: true
    }

    assert get_in(result, [:features, :symbolic_frame, :subject]) ==
      "maple ridge"
    assert get_in(result, [:features, :symbolic_frame, :target]) ==
      "kamloops"
    assert Grounding.available_support?(result)

    output = Grounding.align("They are 30 km apart.", result)
    assert output =~ "228.8 km"
    assert output =~ "straight line"
    assert output =~ "geogratis.gc.ca"
    assert Grounding.supports?(output, result)
  end

  test "unqualified standalone names abstain rather than assume BC" do
    original = context()

    result = WebRetrieval.enrich(original,
      enabled?: true,
      provider: EmptyReference,
      coordinate_provider: ProvinceAwareCoords,
      history: []
    )

    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.support == nil
    refute Map.has_key?(result.web_evidence, :lookup_scope)
    refute Grounding.available_support?(result)
  end

  test "assistant claims, unrelated city, and conflicting provinces cannot create a hint" do
    original = context()

    for history <- [
      [%{"role" => "assistant", "content" => "where is Kamloops BC"}],
      [%{"role" => "user", "content" => "where is Victoria BC"}],
      [%{"role" => "user", "content" => "where is Kamloops BC"},
       %{"role" => "user", "content" => "where is Kamloops Alberta"}],
      [%{"role" => "user", "content" => "Kamloops is probably in BC"}],
      [%{"role" => "user", "content" => "where is Kamloops BC"},
       %{"role" => "user", "content" => "where is Victoria BC"},
       %{"role" => "user", "content" => "where is Vancouver BC"},
       %{"role" => "user", "content" => "where is Toronto ON"}]
    ] do
      assert PlaceScope.for_lookup(original, history) == original
    end
  end

  test "explicitly qualified current roles are never overwritten by history" do
    original = context("how far is maple ridge bc from kamloops bc")
    assert PlaceScope.for_lookup(original, @prior_location) == original
    assert WebRetrieval.route_for(original).relation == :proximity
  end
end
