defmodule Core.Response.GroundedRevisionFlowTest do
  use ExUnit.Case, async: false

  alias Core.Response.GroundedRevision
  alias Core.Response.LlmSynthesis

  @question "What is the population of Kamloops, BC?"
  @url "https://www.wikidata.org/wiki/Q473209"
  @right "Kamloops had 97,902 residents in 2021, according to Wikidata. Source: " <>
           @url

  defmodule VerifiedPopulation do
    def population(subject, _opts) when subject == "Kamloops, BC" do
      {:ok, %{
        kind: :population, status: :source_supported,
        subject: "Kamloops, BC", value: 97_902, display: "97,902",
        year: 2021, census_year: 2021, source_type: :wikidata,
        source_title: "Kamloops",
        source_url: "https://www.wikidata.org/wiki/Q473209",
        sources: [%{
          title: "Kamloops", provider: :wikidata,
          url: "https://www.wikidata.org/wiki/Q473209"
        }]
      }}
    end
  end

  defmodule FakeLlm do
    use GenServer

    def start_link({test_pid, mode}),
      do: GenServer.start_link(__MODULE__, {test_pid, mode}, name: __MODULE__)

    def chat(messages, opts),
      do: GenServer.call(__MODULE__, {:chat, messages, opts})

    @impl true
    def init(state), do: {:ok, state}

    @impl true
    def handle_call({:chat, messages, opts}, _from, {test_pid, mode} = state) do
      send(test_pid, {:grounded_chat, messages, opts})
      system = hd(messages)["content"]
      second_pass? = String.contains?(system, "Your earlier draft did not pass factual review")

      answer =
        cond do
          not second_pass? and mode == :good_first ->
            "Kamloops had 97,902 residents in 2021, according to Wikidata. " <>
              "Source: https://www.wikidata.org/wiki/Q473209"

          not second_pass? ->
            "Kamloops has 17,800 residents right now."

          second_pass? and mode == :successful_revision ->
            "Kamloops had 97,902 residents in 2021, according to Wikidata. " <>
              "Source: https://www.wikidata.org/wiki/Q473209"

          second_pass? and mode == :failed_revision ->
            "Kamloops has 17,800 residents."

          second_pass? and mode == :error_revision ->
            nil

          true ->
            nil
        end

      reply = if is_binary(answer), do: {:ok, %{content: answer}},
               else: {:error, :test_model_error}

      {:reply, reply, state}
    end
  end

  setup do
    keys = [:llm_client, :web_retrieval, :llm_synthesis]
    old = Map.new(keys, &{&1, Application.get_env(:core, &1)})
    Application.put_env(:core, :llm_client, FakeLlm)
    Application.put_env(:core, :web_retrieval,
      enabled?: true, coordinate_provider: VerifiedPopulation,
      max_attempts: 2, timeout_ms: 1_000)

    Application.put_env(:core, :llm_synthesis,
      Keyword.merge(old[:llm_synthesis] || [],
        grounded_revision_enabled?: true, grounded_revision_max_tokens: 220))

    on_exit(fn ->
      Enum.each(keys, fn key ->
        case Map.fetch!(old, key) do
          nil -> Application.delete_env(:core, key)
          value -> Application.put_env(:core, key, value)
        end
      end)
    end)
    :ok
  end

  defp run_generation do
    session_id = "grounded-revision-#{System.unique_integer([:positive])}"
    frame = %{
      type: :location_question, relation: :population,
      subject: "Kamloops, BC", grounding_status: :unverified,
      antecedent_source: :explicit_user_question
    }

    LlmSynthesis.generate(@question,
      %{intent: :ask, conf: 0.93, session_id: session_id, symbolic_frame: frame},
      %{action: :answer_user, tone: :warm, mode: :collaborator},
      %{tone_hint: :warm}, nil)
  end

  test "one revisited LLM draft can produce a natural verified answer" do
    start_supervised!({FakeLlm, {self(), :successful_revision}})

    assert {:ok, reply, reflection} = run_generation()
    assert reply == @right
    assert reflection.status == :accept
    assert reflection.grounded_revision.attempted?
    assert reflection.grounded_revision.outcome == :accepted
    assert reflection.grounded_revision.source == :model_revision
    assert :geographic_claim_unverified in reflection.grounded_revision.prior_issues

    assert_receive {:grounded_chat, first_messages, first_opts}
    assert_receive {:grounded_chat, second_messages, second_opts}
    assert Keyword.fetch!(first_opts, :max_tokens) == 220
    assert Keyword.fetch!(second_opts, :max_tokens) == 220

    initial_system = hd(first_messages)["content"]
    revision_system = hd(second_messages)["content"]

    assert initial_system =~ "CURRENT CONVERSATIONAL FRAME"
    assert revision_system =~ "Your earlier draft did not pass factual review"
    assert revision_system =~ "count=97902"
    assert revision_system =~ "year=2021"
    assert revision_system =~ @url
    refute revision_system =~ "17,800"
    refute revision_system =~ "Wikidata lists a dated population statement for"
    assert List.last(second_messages)["content"] == @question
    refute_receive {:grounded_chat, _, _}, 20
  end

  test "incorrect revision preserves the source-backed deterministic fallback" do
    start_supervised!({FakeLlm, {self(), :failed_revision}})

    assert {:ok, reply, reflection} = run_generation()
    assert reflection.status == :repair
    assert reflection.grounded_revision.outcome == :rejected
    assert reflection.grounded_revision.source == :core_fallback
    assert :geographic_claim_unverified in reflection.grounded_revision.candidate_issues
    assert reply =~ "97,902"
    assert reply =~ "2021"
    refute reply =~ "17,800"

    assert_receive {:grounded_chat, _, _}
    assert_receive {:grounded_chat, _, _}
    refute_receive {:grounded_chat, _, _}, 20
  end

  test "successful initial LLM answer needs no second model call" do
    start_supervised!({FakeLlm, {self(), :good_first}})

    assert {:ok, @right, reflection} = run_generation()
    assert reflection.status == :accept
    refute Map.has_key?(reflection, :grounded_revision)
    assert_receive {:grounded_chat, _, _}
    refute_receive {:grounded_chat, _, _}, 20
  end

  test "failed revision preserves the grounded fallback" do
    start_supervised!({FakeLlm, {self(), :error_revision}})

    assert {:ok, reply, reflection} = run_generation()
    assert reflection.grounded_revision.outcome == :unavailable
    assert reply =~ "97,902"
    assert_receive {:grounded_chat, _, _}
    assert_receive {:grounded_chat, _, _}
  end

  test "unverified or unsafe evidence cannot invoke a second model pass" do
    context = %{
      response_frame: %{version: 1, evidence: %{kind: :population}},
      web_evidence: %{status: :not_found, resolution: :exhausted, support: nil}
    }
    refute GroundedRevision.eligible?(context,
      %{status: :repair, issues: [:geographic_claim_unverified]})

    supported = %{context | web_evidence: %{
      status: :retrieved, resolution: :source_supported,
      support: %{kind: :population, subject: "Kamloops, BC",
        value: 97_902, census_year: 2021, source_url: @url}
    }}

    refute GroundedRevision.eligible?(supported,
      %{status: :reject, issues: [:unsafe, :geographic_claim_unverified]})
    refute GroundedRevision.eligible?(supported,
      %{status: :repair, issues: [:leaked_hidden_context, :geographic_claim_unverified]})
    assert GroundedRevision.eligible?(supported,
      %{status: :repair, issues: [:geographic_claim_unverified]})
  end
end
