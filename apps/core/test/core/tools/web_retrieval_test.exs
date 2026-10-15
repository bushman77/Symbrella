defmodule Core.Tools.WebRetrievalTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.Wikipedia
  alias Core.Tools.WebRetrieval.Grounding
  alias Core.Response.ReflectionLoop
  alias Core.Response.LlmPrompt

  defmodule StubProvider do
    def search(_query, _opts) do
      {:ok, [
        %{
          provider: :wikipedia,
          title: "Maple Ridge, British Columbia",
          url: "https://en.wikipedia.org/wiki/Maple_Ridge%2C_British_Columbia",
          excerpt: "In the 2021 Census of Population, Maple Ridge had a population of 90,990 living in 33,103 dwellings."
        }
      ]}
    end
  end

  defmodule IrrelevantRailgunPages do
    def search(_query, _opts) do
      {:ok,
       [
         %{
           provider: :wikipedia, title: "Railgun",
           url: "https://en.wikipedia.org/wiki/Railgun",
           excerpt: "A railgun is an electromagnetic projectile launcher."
         },
         %{
           provider: :wikipedia, title: "Plasma railgun",
           url: "https://en.wikipedia.org/wiki/Plasma_railgun",
           excerpt: "Plasma railguns use electrical acceleration."
         },
         %{
           provider: :wikipedia, title: "A Certain Scientific Railgun",
           url: "https://en.wikipedia.org/wiki/A_Certain_Scientific_Railgun",
           excerpt: "A fictional television series about superpowers."
         }
       ]}
    end
  end

  defmodule UnrelatedAnimePage do
    def search(_query, _opts) do
      {:ok,
       [
         %{
           provider: :wikipedia,
           title: "A Certain Scientific Railgun season 1",
           url: "https://en.wikipedia.org/wiki/A_Certain_Scientific_Railgun_season_1",
           excerpt: "2009 Japanese animated television series, based on a manga, featuring a railgun."
         }
       ]}
    end
  end

  defmodule JapaneseTechnicalReference do
    def search(_query, _opts) do
      {:ok,
       [
         %{
           provider: :wikipedia,
           title: "Japanese railgun research programme",
           url: "https://en.wikipedia.org/wiki/Japanese_railgun_research_programme",
           excerpt: "Japanese navy researchers tested a railgun prototype."
         }
       ]}
    end
  end

  defmodule FocusedRailgunPage do
    def search(_query, _opts) do
      {:ok,
       [
         %{
           provider: :wikipedia, title: "Japan railgun programme",
           url: "https://en.wikipedia.org/wiki/Japan_railgun_programme",
           excerpt: "A reference excerpt about Japan's electromagnetic railgun research."
         }
       ]}
    end
  end

  defp context do
    %{
      session_id: "test-session",
      user_text: "What is its population?",
      features: %{
        selected_action: :answer_user,
        intent: :ask,
        comprehension: %{degraded?: true},
        symbolic_frame: %{
          type: :location_question,
          subject: "Maple Ridge, BC",
          relation: :population,
          grounding_status: :unverified
        }
      },
      symbolic_frame: %{
        event: %{
          type: :location_question,
          subject: "Maple Ridge, BC",
          relation: :population,
          grounding_status: :unverified
        }
      },
      comprehension: %{degraded?: true},
      decision: %{action: :answer}
    }
  end

  test "only explicit or supported geographic questions trigger remote lookup" do
    assert WebRetrieval.query_for(context()) == {"Maple Ridge, BC population census", :population}

    assert WebRetrieval.query_for(%{user_text: "My sister is meeting me tomorrow"}) == nil
    assert WebRetrieval.query_for(%{user_text: "Hello Symbrella"}) == nil

    assert WebRetrieval.query_for(%{user_text: "Search the web for Elixir 1.18 release notes"}) ==
             {"Elixir 1.18 release notes", nil}
  end

  test "disabled retrieval does not call the provider" do
    assert WebRetrieval.enrich(context(), enabled?: false, provider: StubProvider) == context()
  end

  test "bounded retrieval extracts source-backed census fact" do
    result = WebRetrieval.enrich(context(), enabled?: true, provider: StubProvider)

    assert result.web_evidence.status == :retrieved
    assert result.web_evidence.query == "Maple Ridge, BC population census"
    assert length(result.web_evidence.sources) == 1
    assert result.web_evidence.support.value == 90_990
    assert result.web_evidence.support.census_year == 2021
    assert result.web_evidence.support.status == :source_supported
  end

  test "prompt exposes bounded external evidence as untrusted material" do
    result = WebRetrieval.enrich(context(), enabled?: true, provider: StubProvider)
    prompt = LlmPrompt.build_system_prompt(result)

    assert prompt =~ "WEB EVIDENCE"
    assert prompt =~ "external untrusted excerpts"
    assert prompt =~ "90,990"
    assert prompt =~ "Source URL:"
  end

  test "reflection replaces an unsupported number with cited retrieved evidence" do
    result = WebRetrieval.enrich(context(), enabled?: true, provider: StubProvider)

    {:ok, final, review} =
      ReflectionLoop.review("What is its population?", "Its population is 13,000.", result)

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert review.draft_text == "Its population is 13,000."
    assert final =~ "90,990"
    assert final =~ "https://en.wikipedia.org/wiki/"
    refute final =~ "13,000"
  end

  test "source-backed exact number with citation is not rejected as unverified" do
    result = WebRetrieval.enrich(context(), enabled?: true, provider: StubProvider)
    sourced = Grounding.align("Anything", result)
    assert Grounding.supports?(sourced, result)

    {:ok, ^sourced, review} = ReflectionLoop.review("What is its population?", sourced, result)
    refute :geographic_claim_unverified in review.issues
  end

  test "unavailable search cannot validate a hallucinated answer" do
    result = Map.put(context(), :web_evidence, %{status: :unavailable, sources: [], support: nil})
    {:ok, final, review} =
      ReflectionLoop.review("What is its population?", "The population is 13,000.", result)

    assert review.status == :repair
    refute final =~ "13,000"
  end

  test "a named research subject rejects generic references without the named focus" do
    query = %{user_text: "What do you know about Japan's railgun development?"}

    result =
      WebRetrieval.enrich(query,
        enabled?: true, provider: IrrelevantRailgunPages, max_attempts: 1)

    assert result.web_evidence.query == "Japan's railgun development"
    assert result.web_evidence.request_type == :general_search
    assert result.web_evidence.sources == []
    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.status == :not_found

    draft = "A railgun uses electromagnetic force to accelerate a projectile."
    {:ok, final, reflection} =
      ReflectionLoop.review(query.user_text, draft, result)

    assert reflection.status == :repair
    assert :named_reference_missing in reflection.issues
    assert final =~ "Japan's railgun development"
    assert final =~ "did not establish that subject"
    refute final == draft
  end

  test "Japanese anime railgun result does not pass the Japan research relevance gate" do
    query = %{user_text: "What do you know about Japan's railgun development?"}

    result =
      WebRetrieval.enrich(query,
        enabled?: true, provider: UnrelatedAnimePage, max_attempts: 1)

    assert result.web_evidence.sources == []
    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.status == :not_found

    draft =
      "Japan has developed railguns since the 1960s at several notable institutions."

    {:ok, final, review} = ReflectionLoop.review(query.user_text, draft, result)
    assert review.status == :repair
    assert :named_reference_missing in review.issues
    assert final =~ "did not establish that subject"
    refute final =~ "since the 1960s"
  end

  test "Japanese technical source can qualify without exact Japan noun" do
    query = %{user_text: "What do you know about Japan's railgun development?"}

    result =
      WebRetrieval.enrich(query,
        enabled?: true, provider: JapaneseTechnicalReference, max_attempts: 1)

    assert result.web_evidence.status == :retrieved
    assert result.web_evidence.resolution == :reference_sources
    assert [%{title: "Japanese railgun research programme"}] =
             result.web_evidence.sources
  end

  test "unsupported reference narration is repaired to bounded source excerpts" do
    query = %{user_text: "What do you know about Japan's railgun development?"}

    result =
      WebRetrieval.enrich(query,
        enabled?: true, provider: FocusedRailgunPage, max_attempts: 1)

    draft =
      "Japan's railguns were developed in the 1960s by fictional institutions. " <>
        "The research has advanced substantially since then."

    {:ok, final, review} = ReflectionLoop.review(query.user_text, draft, result)
    assert review.status == :repair
    assert :reference_uncited in review.issues
    assert final =~ "https://en.wikipedia.org/wiki/Japan_railgun_programme"
    refute final =~ "1960s"
    refute final =~ "fictional institutions"
  end

  test "reference results about the named research subject are retained" do
    query = %{user_text: "What do you know about Japan's railgun development?"}

    result =
      WebRetrieval.enrich(query,
        enabled?: true, provider: FocusedRailgunPage, max_attempts: 1)

    assert result.web_evidence.resolution == :reference_sources
    assert result.web_evidence.status == :retrieved
    assert [%{title: "Japan railgun programme"}] = result.web_evidence.sources
    assert result.web_evidence.support == nil
  end

  test "non-possessive general searches still return bounded reference excerpts" do
    result =
      WebRetrieval.enrich(%{user_text: "Search for railgun technology"},
        enabled?: true, provider: IrrelevantRailgunPages, max_attempts: 1)

    assert result.web_evidence.resolution == :reference_sources
    assert length(result.web_evidence.sources) == 3
  end

  test "Wikipedia retains the article lead for ordinary location questions" do
    intro = "Maple Ridge is a city in British Columbia, Canada. It lies east of Vancouver."
    details = "Other text about the city. " <> String.duplicate("History. ", 30) <>
      "Its population in 2021 was 90,990."

    fake_get = fn url, _opts ->
      case url do
        "https://en.wikipedia.org/w/rest.php/v1/search/page" ->
          {:ok, %{status: 200, body: %{"pages" => [
            %{"title" => "Maple Ridge, British Columbia", "description" => "City in Canada",
              "excerpt" => "City east of Vancouver"}
          ]}}}

        "https://en.wikipedia.org/w/api.php" ->
          {:ok, %{status: 200, body: %{"query" => %{"pages" => %{
            "42" => %{"extract" => intro <> " " <> details}
          }}}}}

        other -> flunk("Unexpected URL: #{other}")
      end
    end

    assert {:ok, [ordinary]} = Wikipedia.search("maple ridge bc", fetch: fake_get)
    assert String.starts_with?(ordinary.excerpt, intro)

    assert {:ok, [population]} = Wikipedia.search("Maple Ridge 2021 population",
      fetch: fake_get)
    assert population.excerpt =~ "population in 2021 was 90,990"
    refute String.starts_with?(population.excerpt, intro)
  end

  test "Wikimedia adapter searches and fetches article via fixed hosts without network" do
    fake_get = fn url, _opts ->
      case url do
        "https://en.wikipedia.org/w/rest.php/v1/search/page" ->
          {:ok, %{status: 200, body: %{"pages" => [
            %{"title" => "Maple Ridge, British Columbia", "description" => "City in BC", "excerpt" => "Municipality near Vancouver"}
          ]}}}

        "https://en.wikipedia.org/w/api.php" ->
          {:ok, %{status: 200, body: %{"query" => %{"pages" => %{
            "42" => %{"extract" => "In the 2021 Census of Population, Maple Ridge had a population of 90,990 living in 33,103 residences."}
          }}}}}

        other -> flunk("Unexpected URL: #{other}")
      end
    end

    assert {:ok, [source]} = Wikipedia.search("Maple Ridge population", fetch: fake_get)
    assert source.title == "Maple Ridge, British Columbia"
    assert source.provider == :wikipedia
    assert source.url =~ "en.wikipedia.org"
    assert source.excerpt =~ "population of 90,990"
  end
end
