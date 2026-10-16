defmodule Core.Response.AuthoritativeAnswerPlanTest do
  use ExUnit.Case, async: false

  alias Core.Response.{
    AuthoritativeAnswerPlan, PopulationComparison, ResponseFrame,
    VerifiedPopulation, LlmSynthesis
  }
  alias Core.Tools.WebRetrieval.Grounding

  defmodule FakeStyleLlm do
    use GenServer

    def start_link(test_pid),
      do: GenServer.start_link(__MODULE__, {test_pid, "BRIEF"}, name: __MODULE__)

    def init(state), do: {:ok, state}

    def set_reply(reply), do: GenServer.call(__MODULE__, {:reply, reply})

    def handle_call({:reply, reply}, _from, {test_pid, _}),
      do: {:reply, :ok, {test_pid, reply}}

    def chat(messages, opts), do: GenServer.call(__MODULE__, {:chat, messages, opts})

    def handle_call({:chat, messages, opts}, _from, {test_pid, reply} = state) do
      send(test_pid, {:style_llm_called, messages, opts})
      {:reply, {:ok, %{content: reply}}, state}
    end
  end

  @names ["Kamloops BC", "Maple Ridge BC"]
  @urls [
    "https://www.wikidata.org/wiki/Q473209",
    "https://www.wikidata.org/wiki/Q16740"
  ]

  setup do
    old_client = Application.get_env(:core, :llm_client)
    old_cfg = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(:core, :llm_client, FakeStyleLlm)
    Application.put_env(:core, :llm_synthesis,
      Keyword.put(old_cfg, :authoritative_style_selection_enabled?, false))

    start_supervised!({FakeStyleLlm, self()})

    on_exit(fn ->
      if is_nil(old_client),
        do: Application.delete_env(:core, :llm_client),
        else: Application.put_env(:core, :llm_client, old_client)

      Application.put_env(:core, :llm_synthesis, old_cfg)
    end)

    :ok
  end

  defp session_id, do: "authoritative-answer-#{System.unique_integer([:positive])}"

  defp event_frame do
    %{
      type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: @names, grounding_status: :unverified
    }
  end

  defp source_evidence(values \\ [97_902, 90_990], years \\ [2021, 2021]) do
    claims =
      Enum.zip([@names, values, years, @urls])
      |> Enum.map(fn {subject, value, year, url} ->
        %{
          subject: subject, status: :source_supported,
          value: value, census_year: year,
          source_type: :wikidata, source_url: url
        }
      end)

    %{
      status: :retrieved, resolution: :source_supported,
      relation: :population,
      support: %{
        kind: :population_group, status: :source_supported,
        subjects: @names, results: claims
      },
      sources: [],
      attempts: [],
      attempted_count: 2
    }
  end

  defp context(values \\ [97_902, 90_990], years \\ [2021, 2021]) do
    %{
      session_id: session_id(),
      user_text: "Which of those cities has the larger population?",
      features: %{intent: :unknown, symbolic_frame: event_frame()},
      symbolic_frame: %{event: event_frame()},
      decision: %{action: :answer},
      mood: %{},
      web_evidence: source_evidence(values, years)
    }
    |> PopulationComparison.attach()
    |> then(fn ctx -> Map.put(ctx, :response_frame, ResponseFrame.build(ctx)) end)
  end

  test "verified conclusion is constructed before, and independently of, any model reply" do
    ctx = context()
    plan = AuthoritativeAnswerPlan.build(ctx)
    assert plan.kind == :verified_population_comparison
    assert plan.winner == "Kamloops BC"
    assert plan.year == 2021
    assert plan.difference == 6912
    assert plan.provenance == :symbolic_verified_evidence

    for style <- [:brief, :detailed] do
      answer = AuthoritativeAnswerPlan.render(plan, style)
      assert Grounding.supports?(answer, ctx)
      assert AuthoritativeAnswerPlan.authorized?(plan, answer, ctx)
      assert answer =~ "Kamloops BC"
      assert answer =~ "Maple Ridge BC"
      assert answer =~ "97,902"
      assert answer =~ "90,990"
      assert answer =~ "6,912"
      assert answer =~ "2021"
      assert Enum.all?(@urls, &String.contains?(answer, &1))
    end

    wrong = "Maple Ridge BC has the larger population by 6,912. " <>
      "Sources: #{Enum.join(@urls, " and ")}."
    refute AuthoritativeAnswerPlan.authorized?(plan, wrong, ctx)
    refute AuthoritativeAnswerPlan.authorized?(plan,
      String.replace(AuthoritativeAnswerPlan.render(plan, :brief), "6,912", "6,913"), ctx)
    refute AuthoritativeAnswerPlan.authorized?(plan,
      String.replace(AuthoritativeAnswerPlan.render(plan, :brief), "2021", "2026"), ctx)
    refute AuthoritativeAnswerPlan.authorized?(plan,
      String.replace(AuthoritativeAnswerPlan.render(plan, :brief), hd(@urls), ""), ctx)
  end

  test "no authoritative plan exists without a complete comparable verified conclusion" do
    good = context()
    assert AuthoritativeAnswerPlan.build(good)

    unequal = context([97_902, 90_990], [2021, 2022])
    assert unequal.web_evidence.comparison_status == :different_reporting_years
    assert AuthoritativeAnswerPlan.build(unequal) == nil

    missing = context()
    tampered = put_in(missing, [:web_evidence, :support, :winner], "Maple Ridge BC")
    assert AuthoritativeAnswerPlan.build(tampered) == nil

    unsourced = Map.delete(good, :web_evidence)
    assert AuthoritativeAnswerPlan.build(unsourced) == nil

    irrelevant =
      good
      |> Map.put(:symbolic_frame, %{event: %{type: :smalltalk}})
      |> put_in([:features, :symbolic_frame], %{type: :smalltalk})

    assert AuthoritativeAnswerPlan.build(irrelevant) == nil
  end

  test "model output is a style selector, not a factual draft" do
    assert AuthoritativeAnswerPlan.select_style("DETAILED") == :detailed
    assert AuthoritativeAnswerPlan.select_style("BRIEF") == :brief
    assert AuthoritativeAnswerPlan.select_style(
      "Maple Ridge has the larger population: 90,990") == :brief

    messages = AuthoritativeAnswerPlan.style_selection_prompt(AuthoritativeAnswerPlan.build(context()))
    assert length(messages) == 2
    assert List.last(messages)["content"] == "Select BRIEF or DETAILED."
    refute inspect(messages) =~ "97,902"
    refute inspect(messages) =~ "Maple Ridge BC"
  end

  test "live synthesis publishes the Core plan without calling a free-form LLM" do
    sid = session_id()
    assert :ok = VerifiedPopulation.record(sid, source_evidence())
    features = %{intent: :unknown, session_id: sid, symbolic_frame: event_frame()}

    assert {:ok, answer, meta} =
      LlmSynthesis.generate(
        "Which of those cities has the larger population?",
        features, %{action: :answer}, %{}, :default
      )

    assert answer =~ "Kamloops BC"
    assert answer =~ "6,912"
    assert Enum.all?(@urls, &String.contains?(answer, &1))
    assert meta.authoritative_plan.status == :enforced
    assert meta.authoritative_plan.style_source == :core_default
    refute_receive {:style_llm_called, _, _}, 30
  end

  test "verified symbolic answer survives a disconnected LLM server" do
    sid = session_id()
    assert :ok = VerifiedPopulation.record(sid, source_evidence())
    Application.put_env(:core, :llm_client, MissingStyleLlm)

    features = %{intent: :unknown, session_id: sid, symbolic_frame: event_frame()}

    assert {:ok, answer, meta} =
      LlmSynthesis.generate(
        "Which of those cities has the larger population?",
        features, %{action: :answer}, %{}, :default
      )

    assert answer =~ "Kamloops BC"
    assert answer =~ "6,912"
    assert meta.authoritative_plan.status == :enforced
    refute_receive {:style_llm_called, _, _}, 30

    assert {:error, :llm_not_available} =
      LlmSynthesis.generate(
        "How can I refactor this code?", %{intent: :ask},
        %{action: :answer}, %{}, :default
      )
  end

  test "public response planner identifies symbolic proof as its answer source" do
    sid = session_id()
    assert :ok = VerifiedPopulation.record(sid, source_evidence())

    si = %{
      session_id: sid,
      intent: :unknown,
      confidence: 0.4,
      text: "Which of those cities has the larger population?",
      symbolic_frame: event_frame()
    }

    {_tone, answer, meta} = Core.Response.plan(si, %{})

    assert meta.response_source == :symbolic_verified
    assert meta.response_reflection.authoritative_plan.status == :enforced
    assert answer =~ "Kamloops BC"
    assert answer =~ "6,912"
    assert Enum.all?(@urls, &String.contains?(answer, &1))
    assert meta.curiosity_probe == nil
    refute_receive {:style_llm_called, _, _}, 30
  end

  test "even malicious or incorrect model wording cannot change the verified answer" do
    Application.put_env(:core, :llm_synthesis,
      Application.get_env(:core, :llm_synthesis)
      |> Keyword.put(:authoritative_style_selection_enabled?, true))

    sid = session_id()
    assert :ok = VerifiedPopulation.record(sid, source_evidence())

    features = %{intent: :unknown, session_id: sid, symbolic_frame: event_frame()}
    FakeStyleLlm.set_reply("Maple Ridge BC wins with 90,990 residents.")

    assert {:ok, answer, meta} =
      LlmSynthesis.generate(
        "Which of those cities has the larger population?",
        features, %{action: :answer}, %{}, :default
      )

    assert answer =~ "Kamloops BC"
    assert answer =~ "6,912"
    refute answer =~ "Maple Ridge BC wins"
    assert meta.authoritative_plan.style_source == :invalid_model_choice
    assert_receive {:style_llm_called, messages, opts}
    assert Keyword.get(opts, :max_tokens) == 8
    assert hd(messages)["content"] =~ "EXACTLY one word"
    refute hd(messages)["content"] =~ "2021"
    refute_receive {:style_llm_called, _, _}, 30
  end

  test "model may select a detailed authorized phrasing, never mutate its facts" do
    Application.put_env(:core, :llm_synthesis,
      Application.get_env(:core, :llm_synthesis)
      |> Keyword.put(:authoritative_style_selection_enabled?, true))

    sid = session_id()
    assert :ok = VerifiedPopulation.record(sid, source_evidence())
    FakeStyleLlm.set_reply("DETAILED")

    features = %{intent: :unknown, session_id: sid, symbolic_frame: event_frame()}
    assert {:ok, answer, meta} =
      LlmSynthesis.generate(
        "Which of those cities has the larger population?",
        features, %{action: :answer}, %{}, :default
      )

    assert answer =~ "Comparing sourced population statements:"
    assert answer =~ "Kamloops BC has the larger recorded population by 6,912"
    assert meta.authoritative_plan.selected_style == :detailed
    assert meta.authoritative_plan.style_source == :model_choice
    assert_receive {:style_llm_called, _, _}
  end
end
