defmodule Core.Response.EvidenceRevisionFlowTest do
  use ExUnit.Case, async: false

  alias Core.Response.EvidenceRevision
  alias Core.Response.LlmSynthesis

  @question "What are your thoughts on the US Iran war currently happening?"

  defmodule DatedNews do
    def search(_query, _opts) do
      {:ok,
       [
         %{
           provider: :news_rss,
           title: "Diplomats discuss Strait of Hormuz proposal",
           publisher: "Example Wire",
           published_at: DateTime.to_iso8601(DateTime.utc_now()),
           url: "https://news.google.com/rss/articles/known-report-1",
           excerpt: "Dated headline: diplomatic discussions reported."
         },
         %{
           provider: :news_rss,
           title: "Shipping disruptions remain a regional concern",
           publisher: "Second Wire",
           published_at: DateTime.to_iso8601(DateTime.utc_now()),
           url: "https://news.google.com/rss/articles/known-report-2",
           excerpt: "Dated headline: disruptions were reported."
         }
       ]}
    end
  end

  defmodule MissingNews do
    def search(_, _), do: {:error, :feed_unavailable}
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
      send(test_pid, {:evidence_chat, messages, opts})

      system =
        messages
        |> Enum.find(&(&1["role"] == "system"))
        |> Map.fetch!("content")

      revising? = String.contains?(system, "EVIDENCE-ONLY REVISION")

      reply =
        cond do
          not revising? and mode == :good_first ->
            {:ok, %{content: valid_answer()}}

          not revising? ->
            {:ok, %{content:
              "I don't have live news access, so I can't give current updates."}}

          mode == :successful_revision ->
            {:ok, %{content: valid_answer()}}

          mode == :failed_revision ->
            {:ok, %{content: "I'm sorry, but I cannot confirm recent developments."}}

          mode == :echo_old_revision ->
            {:ok, %{content: old_revision_echo()}}

          mode == :echo_new_revision ->
            {:ok, %{content: List.last(messages)["content"]}}

          mode == :error_revision ->
            {:error, :upstream_timeout}

          true ->
            {:error, :unexpected_test_mode}
        end

      {:reply, reply, state}
    end

    defp old_revision_echo do
      day = Date.to_iso8601(Date.utc_today())

      "ORIGINAL QUESTION: What are your thoughts on the US Iran war currently happening? " <>
        "FAILED CHECKS: news_evidence_not_cited " <>
        "[1] PUBLISHER: Example Wire; DATE: #{day}; " <>
        "HEADLINE: Diplomats discuss Strait of Hormuz proposal; " <>
        "LINK: https://news.google.com/rss/articles/known-report-1 " <>
        "My assessment: civilian protection and de-escalation matter."
    end

    defp valid_answer do
      day = Date.to_iso8601(Date.utc_today())

      "Example Wire reported on #{day}: Diplomats discuss Strait of Hormuz proposal. " <>
        "Source: https://news.google.com/rss/articles/known-report-1\n" <>
        "My assessment: these are reports of ongoing diplomacy, not " <>
        "independent confirmation of a resolution. Civilian impacts and " <>
        "escalation risks are important to consider."
    end
  end

  setup do
    previous_client = Application.get_env(:core, :llm_client)
    previous_news = Application.get_env(:core, :web_retrieval)
    previous_config = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(:core, :llm_client, FakeLlm)

    Application.put_env(:core, :web_retrieval,
      enabled?: true, provider: Core.Tools.WebRetrieval.Wikipedia,
      news_provider: DatedNews, timeout_ms: 1_000, max_attempts: 2)

    Application.put_env(:core, :llm_synthesis,
      Keyword.merge(previous_config,
        evidence_revision_enabled?: true,
        evidence_revision_max_tokens: 360))

    on_exit(fn ->
      restore(:llm_client, previous_client)
      restore(:web_retrieval, previous_news)
      restore(:llm_synthesis, previous_config)
    end)

    :ok
  end

  defp restore(key, nil), do: Application.delete_env(:core, key)
  defp restore(key, value), do: Application.put_env(:core, key, value)

  defp run_generation do
    session_id = "news-revision-#{System.unique_integer([:positive])}"

    LlmSynthesis.generate(
      @question,
      %{intent: :ask, conf: 0.93, session_id: session_id},
      %{action: :answer_user, tone: :neutral},
      %{},
      nil
    )
  end

  test "retry succeeds with compact evidence and no earlier conversation" do
    start_supervised!({FakeLlm, {self(), :successful_revision}})

    id = "evidence-revision-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      Core.Telemetry.attach(
        id,
        [:core, :response, :complete],
        fn event, _measurements, metadata, _ ->
          send(parent, {:evidence_complete, event, metadata})
        end,
        nil
      )

    on_exit(fn -> Core.Telemetry.detach(id) end)

    assert {:ok, final, reflection} = run_generation()

    assert reflection.status == :accept
    assert reflection.evidence_revision.attempted?
    assert reflection.evidence_revision.outcome == :accepted
    assert reflection.evidence_revision.selected_source == :model_revision
    assert reflection.evidence_revision.initial_status == :repair
    assert :news_evidence_not_cited in reflection.evidence_revision.initial_issues
    assert final =~ "Example Wire reported"
    assert final =~ "My assessment:"
    assert final =~ "https://news.google.com/rss/articles/known-report-1"

    assert_receive {:evidence_complete, [:core, :response, :complete], metadata}
    assert metadata.reflection.evidence_revision.outcome == :accepted
    assert metadata.reflection.evidence_revision.selected_source == :model_revision
    assert metadata.reflection.evidence_revision.initial_draft =~ "don't have live news access"
    assert metadata.reflection.evidence_revision.candidate =~ "Example Wire"
    assert is_binary(metadata.reflection.evidence_revision.revision_prompt_sha256)

    assert_receive {:evidence_chat, primary_messages, primary_opts}
    assert_receive {:evidence_chat, revision_messages, retry_opts}
    assert Keyword.fetch!(primary_opts, :max_tokens) == 280
    assert Keyword.fetch!(primary_opts, :max_tokens) <
             Keyword.fetch!(retry_opts, :max_tokens)
    assert Keyword.fetch!(retry_opts, :max_tokens) == 360
    assert length(revision_messages) == 2
    assert Enum.map(revision_messages, & &1["role"]) == ["system", "user"]

    primary_system = hd(primary_messages)["content"]
    revision_system = hd(revision_messages)["content"]
    revision_user = List.last(revision_messages)["content"]

    assert String.length(revision_system) < String.length(primary_system)
    assert String.contains?(revision_system, "EVIDENCE-ONLY REVISION")
    assert String.contains?(revision_user, @question)
    assert String.contains?(revision_user, "Example Wire")
    assert String.contains?(revision_user, "known-report-1")
    refute String.contains?(revision_user, "I don't have live news access")
    refute_receive {:evidence_chat, _, _}, 20
  end

  test "second failed draft never replaces Core's grounded fallback" do
    start_supervised!({FakeLlm, {self(), :failed_revision}})

    assert {:ok, final, reflection} = run_generation()

    assert reflection.status == :repair
    assert reflection.evidence_revision.outcome == :rejected
    assert reflection.evidence_revision.selected_source == :core_fallback
    assert :news_evidence_not_cited in reflection.evidence_revision.candidate_issues

    assert final =~ "Recent dated headlines"
    assert final =~ "Example Wire"
    assert final =~ "known-report-1"
    refute final =~ "cannot confirm recent developments"

    assert_receive {:evidence_chat, _, _}
    assert_receive {:evidence_chat, _, _}
    refute_receive {:evidence_chat, _, _}, 20
  end

  test "copied old repair metadata never counts as an accepted cited answer" do
    start_supervised!({FakeLlm, {self(), :echo_old_revision}})

    assert {:ok, final, reflection} = run_generation()

    assert reflection.evidence_revision.outcome == :prompt_echo
    assert reflection.evidence_revision.selected_source == :core_fallback
    assert final =~ "Recent dated headlines"
    assert final =~ "Example Wire"
    refute final =~ "ORIGINAL QUESTION:"
    refute final =~ "FAILED CHECKS:"
    assert_receive {:evidence_chat, _, _}
    assert_receive {:evidence_chat, _, _}
  end

  test "verbatim new retry prompt is rejected without publishing source instructions" do
    start_supervised!({FakeLlm, {self(), :echo_new_revision}})

    assert {:ok, final, reflection} = run_generation()

    assert reflection.evidence_revision.outcome == :prompt_echo
    assert reflection.evidence_revision.selected_source == :core_fallback
    refute final =~ "Dated headline references"
    assert final =~ "Recent dated headlines"
  end

  test "retry input has a conversational question and no internal issue identifiers" do
    context = %{
      web_evidence: %{
        sources: [%{
          provider: :news_rss,
          publisher: "Example Wire",
          title: "Diplomats discuss Strait of Hormuz proposal",
          url: "https://news.google.com/rss/articles/known-report-1",
          published_at: DateTime.to_iso8601(DateTime.utc_now())
        }]
      }
    }

    messages = EvidenceRevision.messages(@question, context, %{issues: [:news_evidence_not_cited]})
    text = List.last(messages)["content"]

    assert String.starts_with?(text, @question)
    assert text =~ "Example Wire"
    assert text =~ "known-report-1"
    refute text =~ "FAILED CHECKS:"
    refute text =~ "ORIGINAL QUESTION:"
    refute EvidenceRevision.prompt_echo?(
             "Example Wire reported a diplomatic headline on the stated date.",
             messages
           )
    assert EvidenceRevision.prompt_echo?(text, messages)
  end

  test "model error on retry keeps first safe Core response" do
    start_supervised!({FakeLlm, {self(), :error_revision}})

    assert {:ok, final, reflection} = run_generation()

    assert reflection.evidence_revision.outcome == :client_error
    assert reflection.evidence_revision.selected_source == :core_fallback
    assert final =~ "Example Wire"
    assert_receive {:evidence_chat, _, _}
    assert_receive {:evidence_chat, _, _}
  end

  test "correct first draft causes no extra model call" do
    start_supervised!({FakeLlm, {self(), :good_first}})

    assert {:ok, final, reflection} = run_generation()
    assert reflection.status == :accept
    refute Map.has_key?(reflection, :evidence_revision)
    assert final =~ "Example Wire reported"

    assert_receive {:evidence_chat, _, _}
    refute_receive {:evidence_chat, _, _}, 20
  end

  test "unavailable news cannot trigger evidence retry" do
    start_supervised!({FakeLlm, {self(), :failed_revision}})

    Application.put_env(:core, :web_retrieval,
      enabled?: true, news_provider: MissingNews, timeout_ms: 1_000,
      max_attempts: 2)

    assert {:ok, _final, reflection} = run_generation()
    refute Map.has_key?(reflection, :evidence_revision)

    assert_receive {:evidence_chat, _, _}
    refute_receive {:evidence_chat, _, _}, 20
  end

  test "ineligible unsafe or unsupported contexts cannot spawn retry" do
    sources = [%{
      provider: :news_rss, title: "Valid dated news",
      publisher: "Example Wire", url: "https://news.google.com/rss/articles/123",
      published_at: DateTime.to_iso8601(DateTime.utc_now())
    }]
    context = %{
      web_evidence: %{relation: :current_events, status: :retrieved,
                      resolution: :headlines_available, sources: sources}
    }

    refute EvidenceRevision.eligible?(context,
      %{status: :reject, issues: [:unsafe, :news_evidence_not_cited]})

    refute EvidenceRevision.eligible?(context,
      %{status: :repair, issues: [:news_evidence_not_cited, :leaked_hidden_context]})

    refute EvidenceRevision.eligible?(
      %{web_evidence: %{relation: :current_events, status: :unavailable,
                        resolution: :exhausted, sources: []}},
      %{status: :repair, issues: [:news_evidence_not_cited]}
    )
  end
end
