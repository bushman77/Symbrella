defmodule Core.Tools.EvidenceResolverTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.Wikidata
  alias Core.Tools.WebRetrieval.Grounding
  alias Core.Response.ReflectionLoop

  defmodule OffTopicWikipedia do
    def search(_query, _opts) do
      {:ok, [
        %{provider: :wikipedia, title: "Economic history of Vancouver",
          url: "https://en.wikipedia.org/wiki/Economic_history_of_Vancouver",
          excerpt: "Vancouver has a long economic history."}
      ]}
    end
  end

  defmodule PopulationOnRetry do
    def search(query, _opts) do
      excerpt =
        if String.contains?(query, "2021 census population"),
          do: "Maple Ridge's population in 2021 was 90,990.",
          else: "Maple Ridge is a Canadian city."

      {:ok, [
        %{provider: :wikipedia, title: "Maple Ridge, British Columbia",
          url: "https://en.wikipedia.org/wiki/Maple_Ridge,_British_Columbia",
          excerpt: excerpt}
      ]}
    end
  end

  defmodule SuccessfulCoords do
    def proximity(subject, target, _opts) do
      # Synthetic coordinates measure a constant (38.5 km) for test
      # determinism. The two *entity identities* must still match the
      # exact subject and target independently bound by Core.
      named_target = target |> String.split(",", parts: 2) |> hd()
      named_target = String.trim(named_target) |> String.downcase()
      {target_title, target_url} =
        if named_target == "kamloops",
          do: {"Kamloops", "https://www.wikidata.org/wiki/Q473209"},
          else: {"Vancouver", "https://www.wikidata.org/wiki/Q24639"}

      {:ok, %{kind: :proximity, status: :source_supported,
        subject: subject, target: target, method: :haversine,
        distance_km: 38.5,
        sources: [
          %{title: "Maple Ridge", provider: :wikidata,
            url: "https://www.wikidata.org/wiki/Q16740"},
          %{title: target_title, provider: :wikidata, url: target_url}
        ]}}
    end
  end

  defmodule FailedCoords do
    def proximity(_, _, _), do: {:error, :entity_mismatch}
  end

  defmodule FailedPopulation do
    def population(_, _), do: {:error, :dated_population_unavailable}
  end

  defp context(relation) do
    frame = %{
      type: :location_question, subject: "Maple Ridge, BC",
      target: "Vancouver", relation: relation, grounding_status: :unverified
    }
    %{session_id: "evidence-test",
      user_text: "Is it close to Vancouver?",
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}}
  end

  test "tries structured coordinates after irrelevant Wikipedia results" do
    result = WebRetrieval.enrich(context(:proximity),
      enabled?: true,
      provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords
    )
    evidence = result.web_evidence

    assert evidence.status == :retrieved
    assert evidence.resolution == :source_supported
    assert evidence.attempted_count == 2
    assert Enum.map(evidence.attempts, & &1.status) ==
      [:retrieved_unsupported, :source_supported]
    assert evidence.support.kind == :proximity
    assert Enum.take(evidence.sources, 2) |> Enum.map(& &1.provider) == [:wikidata, :wikidata]

    {:ok, final, review} =
      ReflectionLoop.review("Is it close to Vancouver?",
        "No, it is 100 km away.", result)

    assert review.status == :repair
    assert final =~ "38.5 km"
    assert final =~ "straight line"
    assert final =~ "www.wikidata.org/wiki/Q16740"
    assert final =~ "www.wikidata.org/wiki/Q24639"
    refute final =~ "100 km"
    assert Grounding.supports?(final, result)
  end

  test "direct distance question routes to coordinate support and overrides a guessed number" do
    alias Core.Semantic.EventFrames

    question = "how far is Maple Ridge, BC from Kamloops?"
    frame = EventFrames.attach(%{sentence: question}).symbolic_frame

    assert frame.subject == "Maple Ridge, BC"
    assert frame.target == "Kamloops"

    ctx = %{
      user_text: question,
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame},
      symbolic_frame: %{event: frame}
    }

    with_evidence =
      WebRetrieval.enrich(ctx,
        enabled?: true,
        provider: OffTopicWikipedia,
        coordinate_provider: SuccessfulCoords
      )

    assert with_evidence.web_evidence.resolution == :source_supported
    assert with_evidence.web_evidence.support.method == :haversine

    {:ok, answer, review} =
      ReflectionLoop.review(question, "Maple Ridge is 10 miles from Kamloops.", with_evidence)

    assert review.status == :repair
    assert answer =~ "38.5 km"
    assert answer =~ "straight line"
    refute answer =~ "10 miles"
  end

  test "actual repeated-from phrasing is routed to coordinates, not an unsourced 30 km assertion" do
    alias Core.Semantic.EventFrames
    alias Core.Tools.WebRetrieval.RequestRouter

    question = "how far from maple ridge is from kamloops"
    frame = EventFrames.location_question(question)

    assert frame.relation == :proximity
    assert frame.subject == "maple ridge"
    assert frame.target == "kamloops"
    assert frame.grounding_status == :unverified

    ctx = %{
      user_text: question,
      features: %{intent: :ask, conf: 0.82, symbolic_frame: frame},
      symbolic_frame: %{event: frame}
    }

    assert %{relation: :proximity, request_type: :geographic_proximity} =
      RequestRouter.route(ctx)

    # The 38.5 km stub is synthetic and intentionally not the true
    # Maple Ridge/Kamloops distance; this tests proof enforcement only.
    evidence = WebRetrieval.enrich(ctx,
      enabled?: true,
      provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords
    )
    assert evidence.web_evidence.resolution == :source_supported
    assert evidence.web_evidence.support.subject == "maple ridge"
    assert evidence.web_evidence.support.target == "kamloops"

    {:ok, corrected, review} =
      ReflectionLoop.review(question,
        "According to the information provided, Kamloops is approximately 30 kilometers (19 miles) from Maple Ridge. Wikipedia",
        evidence)

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert corrected =~ "38.5 km"
    assert corrected =~ "straight line"
    assert Grounding.supports?(corrected, evidence)

    # Even a successful coordinate-provider status is insufficient when
    # its second named entity is Vancouver rather than Kamloops.
    corrupted =
      update_in(evidence, [:web_evidence, :support, :sources], fn [origin, target] ->
        [origin, %{target | title: "Vancouver",
          url: "https://www.wikidata.org/wiki/Q24639"}]
      end)

    refute Grounding.available_support?(corrupted)

    {:ok, wrong_source_answer, wrong_source_review} =
      ReflectionLoop.review(question,
        "Kamloops is approximately 30 kilometers from Maple Ridge.",
        corrupted)

    assert :geographic_claim_unverified in wrong_source_review.issues
    assert wrong_source_answer =~ "don't have verified distance"
    refute wrong_source_answer =~ "30 kilometers"

    refute corrected =~ "30 kilometers"
    refute corrected =~ "19 miles"

    no_coords = WebRetrieval.enrich(ctx,
      enabled?: true,
      provider: OffTopicWikipedia,
      coordinate_provider: FailedCoords,
      max_attempts: 2
    )
    assert no_coords.web_evidence.support == nil
    assert Enum.any?(no_coords.web_evidence.attempts, fn attempt ->
      attempt.kind == :coordinates and
        attempt.status == :unavailable and
        attempt.reason == :entity_mismatch
    end)

    {:ok, rejected, refusal_review} =
      ReflectionLoop.review(question,
        "Kamloops is approximately 30 kilometers (19 miles) from Maple Ridge. Wikipedia",
        no_coords)

    assert refusal_review.status == :repair
    assert :geographic_claim_unverified in refusal_review.issues
    assert rejected =~ "don't have verified distance"
    refute rejected =~ "30 kilometers"
    refute rejected =~ "19 miles"
  end

  test "source-backed proximity admits comma and space qualified Canadian place names" do
    alias Core.Semantic.EventFrames

    pairs = [
      {"how far is Maple Ridge, BC from Kamloops, BC",
       "Maple Ridge, BC", "Kamloops, BC"},
      {"how far is maple ridge bc from kamloops bc",
       "maple ridge bc", "kamloops bc"},
      {"how far is maple ridge from kamloops",
       "maple ridge", "kamloops"}
    ]

    for {question, subject, target} <- pairs do
      frame = EventFrames.location_question(question)
      assert frame.subject == subject
      assert frame.target == target

      sources = [
        %{title: "Maple Ridge", provider: :wikidata,
          url: "https://www.wikidata.org/wiki/Q16740"},
        %{title: "Kamloops", provider: :wikidata,
          url: "https://www.wikidata.org/wiki/Q473209"}
      ]

      context = %{
        user_text: question,
        features: %{symbolic_frame: frame},
        symbolic_frame: %{event: frame},
        web_evidence: %{
          status: :retrieved, resolution: :source_supported,
          support: %{kind: :proximity, status: :source_supported,
            method: :haversine, distance_km: 229.6,
            subject: subject, target: target, sources: sources}
        }
      }

      assert Grounding.available_support?(context)
      answer = Grounding.align("The cities are 30 km apart.", context)
      assert answer =~ "229.6 km"
      assert answer =~ "straight line"
      assert Grounding.supports?(answer, context)
      refute Grounding.supports?(answer <> " It is 30 km by road.", context)
      refute Grounding.supports?("30 km straight line " <> answer, context)
    end
  end

  test "an unverified refusal is upgraded when evidence is source-supported" do
    context = WebRetrieval.enrich(context(:proximity),
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords)

    {:ok, final, reflection} =
      ReflectionLoop.review("Is it close to Vancouver?",
        "I can't verify the distance.", context)

    assert reflection.status == :repair
    assert final =~ "38.5 km"
  end

  test "population lookup falls back to a dated census excerpt when Wikidata is unavailable" do
    result = WebRetrieval.enrich(context(:population),
      enabled?: true, provider: PopulationOnRetry, coordinate_provider: FailedPopulation)

    assert result.web_evidence.resolution == :source_supported
    assert result.web_evidence.attempted_count == 2
    assert result.web_evidence.support.value == 90_990
    assert result.web_evidence.support.census_year == 2021
  end

  test "Kamloops population uses latest dated Wikidata statement, not an invented 17,800" do
    alias Core.Semantic.EventFrames

    question = "what is the population of Kamloops, BC"
    frame = EventFrames.location_question(question)
    assert frame.relation == :population
    assert frame.subject == "Kamloops, BC"

    statement = fn amount, year ->
      %{
        "mainsnak" => %{"datavalue" => %{"value" => %{"amount" => amount}}},
        "qualifiers" => %{"P585" => [
          %{"datavalue" => %{"value" =>
            %{"time" => "+#{year}-01-01T00:00:00Z"}}}
        ]},
        "rank" => "normal"
      }
    end

    fake_get = fn _url, opts ->
      case opts[:params][:titles] do
        "Kamloops" ->
          {:ok, response} = entity(
            "Q473209", "Kamloops", "city in British Columbia, Canada",
            50.6761, -120.341
          )

          with_claims =
            put_in(response,
              [:body, "entities", "Q473209", "claims", "P1082"],
              [
                %{"mainsnak" => %{"datavalue" => %{"value" =>
                  %{"amount" => "+17800"}}}},
                statement.("+90280", "2016"),
                statement.("+97902", "2021")
              ])

          {:ok, with_claims}

        _ ->
          {:ok, %{status: 200, body: %{"entities" =>
            %{"-1" => %{"id" => "-1", "missing" => ""}}}}}
      end
    end

    assert {:ok, statement_result} =
             Wikidata.population("Kamloops, BC", fetch: fake_get)

    assert statement_result.value == 97_902
    assert statement_result.display == "97,902"
    assert statement_result.census_year == 2021
    assert statement_result.source_url == "https://www.wikidata.org/wiki/Q473209"

    context = %{
      user_text: question,
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame}
    }

    enriched = WebRetrieval.enrich(context,
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: Wikidata,
      coordinate_opts: [fetch: fake_get]
    )

    assert enriched.web_evidence.resolution == :source_supported
    assert enriched.web_evidence.attempted_count == 1
    assert enriched.web_evidence.support.value == 97_902

    {:ok, final, review} = ReflectionLoop.review(question,
      "The population of Kamloops, BC is approximately 17,800. Sources: " <>
        "https://www.wikidata.org/wiki/Q473209", enriched)

    assert review.status == :repair
    assert final =~ "97,902"
    assert final =~ "2021"
    assert final =~ "https://www.wikidata.org/wiki/Q473209"
    refute final =~ "17,800"
    refute final =~ "2026 census"

    assert Grounding.supports?(final, enriched)

    paraphrase =
      "Kamloops had 97,902 residents in 2021, according to Wikidata. Source: " <>
        "https://www.wikidata.org/wiki/Q473209"

    alternative =
      "The 2021 population of Kamloops, BC was 97,902 people. " <>
        "Source: https://www.wikidata.org/wiki/Q473209"

    for natural <- [paraphrase, alternative] do
      assert Grounding.supports?(natural, enriched)
      {:ok, ^natural, accepted} = ReflectionLoop.review(question, natural, enriched)
      assert accepted.status == :accept
      refute accepted.applied?
    end

    rejected = [
      "Kamloops had 17,800 residents in 2021. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops had 97,902 residents in 2026. Source: https://www.wikidata.org/wiki/Q473209",
      "Maple Ridge had 97,902 residents in 2021. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops had 97,902 residents in 2021, 120 km from Vancouver. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops has 97,902 residents currently, according to its 2021 count. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops had 97,902 residents in 2021 and is located in the Okanagan Valley. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops had 97,902 residents in 2021 in the Okanagan Valley. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops had 97,902 residents in 2021 and 17,800 before. Source: https://www.wikidata.org/wiki/Q473209",
      "Kamloops had 97,902 residents in 2021."
    ]

    for unsafe <- rejected do
      refute Grounding.supports?(unsafe, enriched), unsafe
      {:ok, repaired, reflection} = ReflectionLoop.review(question, unsafe, enriched)
      assert reflection.status == :repair
      assert :geographic_claim_unverified in reflection.issues
      assert Grounding.supports?(repaired, enriched)
    end
  end

  test "undated Wikidata population values never become verified counts" do
    fake_get = fn _url, opts ->
      case opts[:params][:titles] do
        "Kamloops" ->
          {:ok, response} = entity(
            "Q473209", "Kamloops", "city in British Columbia, Canada",
            50.6761, -120.341
          )

          {:ok, put_in(response, [:body, "entities", "Q473209", "claims", "P1082"], [
            %{"mainsnak" => %{"datavalue" => %{"value" =>
              %{"amount" => "+17800"}}}}
          ])}

        _ ->
          {:ok, %{status: 200, body: %{"entities" =>
            %{"-1" => %{"id" => "-1", "missing" => ""}}}}}
      end
    end

    assert {:error, :dated_population_unavailable} =
             Wikidata.population("Kamloops, BC", fetch: fake_get)
  end

  test "failed alternatives exhaust the budget; no unsupported claim is promoted" do
    result = WebRetrieval.enrich(context(:proximity),
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: FailedCoords, max_attempts: 2)

    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.attempted_count == 2
    assert result.web_evidence.support == nil

    {:ok, final, reflection} =
      ReflectionLoop.review("Is it close to Vancouver?",
        "They are definitely 100 km apart.", result)

    assert reflection.status == :repair
    assert final =~ "don't have verified distance"
    refute final =~ "100 km"
  end

  test "independent source claims are not inferred merely from a shared Wikimedia corpus" do
    result = WebRetrieval.enrich(context(:proximity),
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords)

    assert result.web_evidence.resolution == :source_supported
    refute result.web_evidence.resolution == :corroborated
    assert length(result.web_evidence.support.sources) == 2
  end

  defp wikidata_reply(title) do
    case title do
      "Maple Ridge, British Columbia" ->
        entity("Q16740", "Maple Ridge", "city in British Columbia, Canada", 49.2167, -122.6)

      "Vancouver, British Columbia" ->
        {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}

      "Vancouver" ->
        entity("Q24639", "Vancouver", "city in British Columbia, Canada", 49.2827, -123.1207)

      other ->
        flunk("Unexpected title: #{other}")
    end
  end

  defp entity(id, label, description, lat, lon) do
    {:ok, %{status: 200, body: %{
      "entities" => %{id => %{
        "id" => id,
        "labels" => %{"en" => %{"value" => label}},
        "descriptions" => %{"en" => %{"value" => description}},
        "claims" => %{"P625" => [
          %{"mainsnak" => %{"datavalue" => %{"value" => %{
            "latitude" => lat, "longitude" => lon,
            "globe" => "http://www.wikidata.org/entity/Q2"
          }}}}
        ]}
      }}
    }}}
  end

  test "Wikidata resolves unpunctuated BC qualifiers without weakening entity validation" do
    fake_get = fn url, opts ->
      assert url == "https://www.wikidata.org/w/api.php"

      case opts[:params][:titles] do
        "Maple Ridge, British Columbia" ->
          wikidata_reply("Maple Ridge, British Columbia")

        "Kamloops, British Columbia" ->
          {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}

        "Kamloops" ->
          entity("Q473209", "Kamloops", "city in British Columbia, Canada",
            50.6745, -120.3273)

        other ->
          flunk("Unexpected Wikidata entity query: #{other}")
      end
    end

    assert {:ok, proof} =
      Wikidata.proximity("maple ridge bc", "kamloops bc", fetch: fake_get)

    assert proof.kind == :proximity
    assert proof.distance_km > 0
    assert proof.method == :haversine
  end

  test "quoted live distance question uses coordinate evidence instead of model's mileage guess" do
    alias Core.Semantic.EventFrames

    question = "How far is Maple Ridge, BC from Kamloops, BC?”"
    frame = EventFrames.location_question(question)
    assert frame.subject == "Maple Ridge, BC"
    assert frame.target == "Kamloops, BC"

    fake_get = fn url, opts ->
      assert url == "https://www.wikidata.org/w/api.php"

      case opts[:params][:titles] do
        "Maple Ridge, British Columbia" ->
          entity("Q16740", "Maple Ridge", "city in British Columbia, Canada",
            49.2167, -122.6)

        "Kamloops, British Columbia" ->
          {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}

        "Kamloops" ->
          entity("Q473209", "Kamloops", "city in British Columbia, Canada",
            50.6745, -120.3273)

        other ->
          flunk("Unexpected Wikidata coordinate title: #{other}")
      end
    end

    context = %{
      user_text: question,
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame}
    }

    enriched =
      WebRetrieval.enrich(context,
        enabled?: true,
        provider: OffTopicWikipedia,
        coordinate_provider: Wikidata,
        coordinate_opts: [fetch: fake_get]
      )

    support = enriched.web_evidence.support
    assert enriched.web_evidence.resolution == :source_supported
    assert support.status == :source_supported
    assert support.method == :haversine
    assert_in_delta support.distance_km, 229.6, 0.2
    assert Enum.map(support.sources, & &1.url) == [
      "https://www.wikidata.org/wiki/Q16740",
      "https://www.wikidata.org/wiki/Q473209"
    ]

    {:ok, final, review} =
      ReflectionLoop.review(question,
        "Maple Ridge, BC is approximately 120 miles away from Kamloops, BC.",
        enriched)

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert final =~ "229.6 km"
    assert final =~ "straight line"
    assert final =~ "https://www.wikidata.org/wiki/Q473209"
    refute final =~ "120 miles"
  end

  test "qualified fallback never accepts a same-name city in a different jurisdiction" do
    fake_get = fn _url, opts ->
      case opts[:params][:titles] do
        "Maple Ridge, British Columbia" ->
          wikidata_reply("Maple Ridge, British Columbia")

        "Kamloops, British Columbia" ->
          {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}

        "Kamloops" ->
          entity("Q999999", "Kamloops", "city in another province, Canada",
            50.6745, -120.3273)

        other ->
          flunk("Unexpected Wikidata title: #{other}")
      end
    end

    assert {:error, :place_not_verified} =
             Wikidata.proximity("Maple Ridge, BC", "Kamloops, BC", fetch: fake_get)
  end

  test "a recently named city's identity is sourced and unsupported geographic prose is replaced" do
    alias Core.Semantic.EventFrames

    question = "what is kamloops?"
    frame = %{
      type: :location_question,
      relation: :identity,
      subject: "Kamloops, BC",
      grounding_status: :unverified
    }

    fake_get = fn _url, opts ->
      case opts[:params][:titles] do
        "Kamloops, British Columbia" ->
          {:ok, %{status: 200, body: %{"entities" =>
            %{"-1" => %{"id" => "-1", "missing" => ""}}}}}

        "Kamloops" ->
          entity("Q473209", "Kamloops", "city in British Columbia, Canada",
            50.6761, -120.341)

        other ->
          flunk("Unexpected city page: #{other}")
      end
    end

    context = %{
      user_text: question,
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame}
    }

    enriched = WebRetrieval.enrich(context,
      enabled?: true,
      coordinate_opts: [fetch: fake_get]
    )

    evidence = enriched.web_evidence
    assert evidence.relation == :identity
    assert evidence.request_type == :geographic_identity
    assert evidence.resolution == :source_supported
    assert evidence.support.kind == :identity
    assert evidence.support.title == "Kamloops"
    assert evidence.support.description == "city in British Columbia, Canada"

    {:ok, corrected, review} =
      ReflectionLoop.review(question,
        "Kamloops is in the Okanagan Valley, about 120 km from Kamloops.",
        enriched)

    assert review.status == :repair
    assert corrected ==
      "Wikidata describes Kamloops as “city in British Columbia, Canada”. " <>
        "Source: https://www.wikidata.org/wiki/Q473209"
    refute corrected =~ "Okanagan Valley"
    refute corrected =~ "120 km"

    {:ok, ^corrected, accepted} = ReflectionLoop.review(question, corrected, enriched)
    assert accepted.status == :accept
  end

  test "identity lookup rejects wrong province and cannot validate invented locations" do
    question = "what is maple ridge?"
    frame = %{
      type: :location_question, relation: :identity,
      subject: "Maple Ridge, BC", grounding_status: :unverified
    }

    fake_get = fn _url, opts ->
      case opts[:params][:titles] do
        "Maple Ridge, British Columbia" ->
          entity("Q999999", "Maple Ridge", "city in another province, Canada",
            49.2, -122.6)

        "Maple Ridge" ->
          entity("Q999999", "Maple Ridge", "city in another province, Canada",
            49.2, -122.6)

        other ->
          flunk("Unexpected page: #{other}")
      end
    end

    context = %{
      user_text: question,
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame}
    }

    enriched = WebRetrieval.enrich(context,
      enabled?: true,
      coordinate_opts: [fetch: fake_get],
      max_attempts: 1
    )

    assert enriched.web_evidence.support == nil

    {:ok, response, review} =
      ReflectionLoop.review(question,
        "Maple Ridge is in the Okanagan Valley, 120 km north of Kamloops.",
        enriched)

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert response =~ "couldn't verify its geographic identity"
    refute response =~ "Okanagan Valley"
  end

  test "Wikidata provider validates two city entities and computes straight-line distance" do
    fake_get = fn url, opts ->
      assert url == "https://www.wikidata.org/w/api.php"
      assert opts[:params][:action] == "wbgetentities"
      assert opts[:params][:sites] == "enwiki"
      wikidata_reply(opts[:params][:titles])
    end

    assert {:ok, proof} =
      Wikidata.proximity("Maple Ridge, BC", "Vancouver", fetch: fake_get)

    assert proof.method == :haversine
    assert proof.status == :source_supported
    assert_in_delta proof.distance_km, 38.5, 0.3
    assert Enum.map(proof.sources, & &1.url) == [
      "https://www.wikidata.org/wiki/Q16740",
      "https://www.wikidata.org/wiki/Q24639"
    ]
  end

  test "Wikidata rejects the wrong city rather than treating search matches as verified" do
    fake_get = fn _url, opts ->
      title = opts[:params][:titles]
      case title do
        "Maple Ridge, British Columbia" -> wikidata_reply(title)
        "Vancouver, British Columbia" ->
          {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}
        "Vancouver" ->
          entity("Q200", "Vancouver", "city in Washington, United States", 45.6, -122.67)
      end
    end

    assert {:error, :place_not_verified} =
      Wikidata.proximity("Maple Ridge, BC", "Vancouver", fetch: fake_get)
  end

  test "malformed geographic coordinates are rejected" do
    fake_get = fn _url, opts ->
      title = opts[:params][:titles]
      case title do
        "Maple Ridge, British Columbia" ->
          entity("Q16740", "Maple Ridge", "city in British Columbia, Canada", 190.0, -122.6)
        _ ->
          wikidata_reply(title)
      end
    end

    assert {:error, :place_not_verified} =
      Wikidata.proximity("Maple Ridge, BC", "Vancouver", fetch: fake_get)
  end
end
