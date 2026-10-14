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
