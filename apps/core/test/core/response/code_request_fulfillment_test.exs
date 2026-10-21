defmodule Core.Response.CodeRequestFulfillmentTest do
  use ExUnit.Case, async: false

  alias Core.Response.{Attach, CodeRequest, Context, ReflectionLoop}
  alias Core.Syntax.RoleCompetition
  alias Core.SemanticInput

  @question "Can you write me a basic GenServer in a module called MyExample?"
  @deferral "Of course! A basic GenServer can be written for you in the module named \"MyExample\". However, since this is a complex task and I am not sure what specific functionality you require, would you mind providing some more details or guidance on how to proceed?"
  @example_code """
  Here's the GenServer:

  ```elixir
  defmodule MyExample do
    use GenServer
    def start_link(initial_state), do: GenServer.start_link(__MODULE__, initial_state)
    @impl true
    def init(initial_state), do: {:ok, initial_state}
  end
  ```
  """

  defmodule FakeLlm do
    use GenServer

    def start_link({owner, drafts}) do
      GenServer.start_link(__MODULE__, {owner, drafts}, name: __MODULE__)
    end

    def chat(messages, opts), do: GenServer.call(__MODULE__, {:chat, messages, opts})

    @impl true
    def init({owner, drafts}), do: {:ok, {owner, drafts}}

    @impl true
    def handle_call({:chat, messages, opts}, _from, {owner, [draft | rest]}) do
      send(owner, {:code_request_llm_chat, messages, opts})
      {:reply, {:ok, %{content: draft}}, {owner, rest}}
    end
  end

  setup do
    settings = [
      llm_client: Application.get_env(:core, :llm_client),
      response_prompt_profile: Application.get_env(:core, :response_prompt_profile),
      web_retrieval: Application.get_env(:core, :web_retrieval),
      agency_ledger_enabled?: Application.get_env(:core, :agency_ledger_enabled?)
    ]

    Application.put_env(:core, :llm_client, FakeLlm)
    Application.put_env(:core, :response_prompt_profile, :mythomax)
    Application.put_env(:core, :web_retrieval, enabled?: false)
    Application.put_env(:core, :agency_ledger_enabled?, false)

    on_exit(fn ->
      for {key, value} <- settings do
        if is_nil(value), do: Application.delete_env(:core, key),
          else: Application.put_env(:core, key, value)
      end
    end)

    :ok
  end

  defp cell(word, pos) do
    %{id: "#{word}|#{pos}|1", norm: word, pos: pos,
      semantic_atoms: [], activation: 0.5}
  end

  defp input(text \\ @question) do
    {tokens, _} =
      String.split(text, " ")
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {word, idx}, start ->
        stop = start + byte_size(word)
        {%{index: idx, phrase: word, span: {start, stop},
           kind: :word, n: 1, mw: false}, stop + 1}
      end)

    cells = [
      cell("can", "auxiliary"), cell("you", "pronoun"),
      cell("write", "verb"), cell("write", "noun"),
      cell("me", "pronoun"), cell("a", "determiner"),
      cell("basic", "adjective"), cell("basic", "noun"),
      cell("in", "preposition"), cell("module", "noun"),
      cell("called", "verb")
    ]

    %SemanticInput{
      sentence: text, tokens: tokens, active_cells: cells,
      source: :prod, intent: :ask, confidence: 0.7
    }
    |> RoleCompetition.attach()
    |> Map.put(:selected_action, :answer_user)
    |> Map.put(:session_id, "code-response-" <>
      Integer.to_string(System.unique_integer([:positive])))
  end

  defp response_context(si) do
    Context.from_response_parts(si.sentence,
      %{intent: :ask, session_id: si.session_id,
        selected_action: :answer_user,
        guardrail?: false, risk_bucket: :low,
        turn_context: Context.from_si(si)},
      %{tone: :neutral, mode: :scribe, action: :answer_user},
      %{}, %{})
  end

  test "witnessed request is actionable, rather than a vague option-offer" do
    struct = Context.from_si(input()).request_structure
    assert CodeRequest.eligible_structure?(struct, @question)

    decision = %{tone: :neutral, action: :offer_options, mode: :scribe}
    aligned = Core.Response.ActionAlignment.align(
      decision, :answer_user, @question, false, struct)
    assert aligned.action == :act_first
    assert Core.Response.ActionAlignment.align(
      decision, :answer_user, @question, true, struct) == decision
  end

  test "rejects reported non-answer but accepts delivered code" do
    context = response_context(input())
    assert CodeRequest.eligible?(context)
    assert CodeRequest.missing?(context, @deferral)
    refute CodeRequest.complete?(context, @deferral)
    assert CodeRequest.complete?(context, @example_code)

    {:ok, same, reflection} = ReflectionLoop.review(@question, @deferral, context)
    assert same == @deferral
    assert reflection.status == :repair
    assert :requested_code_missing in reflection.issues
    refute reflection.applied?

    expected_code = @example_code
    {:ok, ^expected_code, accepted} =
      ReflectionLoop.review(@question, @example_code, context)
    assert accepted.status == :accept
    refute :requested_code_missing in accepted.issues
  end

  test "one bounded LLM retry replaces actual observed live deferral with code" do
    start_supervised!({FakeLlm, {self(), [@deferral, @example_code]}})

    si = Attach.maybe_build_response_plan(input(), response: :auto)
    assert si.response_text =~ "defmodule MyExample do"
    assert si.response_text =~ "use GenServer"
    refute si.response_text =~ "would you mind providing"

    meta = si.response_meta
    assert meta.action == :act_first
    assert meta.agent_selected_action == :answer_user
    assert meta.response_reflection.status == :repair
    assert :requested_code_missing in meta.response_reflection.issues
    assert meta.response_reflection.code_request_repair.outcome == :accepted
    assert meta.response_reflection.code_request_repair.selected_source == :model_revision

    assert_receive {:code_request_llm_chat, first, _}
    assert hd(first)["content"] =~ "CURRENT-TURN REQUEST STRUCTURE"
    assert_receive {:code_request_llm_chat, second, opts}
    assert hd(second)["content"] =~ "Supply actual code"
    assert Keyword.get(opts, :max_tokens) >= 440
    refute_receive {:code_request_llm_chat, _, _}, 30
  end

  test "two deferrals trigger a bounded starter template without code execution" do
    start_supervised!({FakeLlm, {self(), [@deferral, @deferral]}})

    si = Attach.maybe_build_response_plan(input(), response: :auto)
    assert si.response_text =~ "defmodule MyExample do"
    assert si.response_text =~ "use GenServer"
    assert si.response_text =~ "def init(initial_state)"
    assert si.response_text =~ ~S(def start_link(initial_state \\ %{}) do)
    assert si.response_text =~ "not executed or tested"
    assert si.response_meta.response_reflection.code_request_repair.outcome == :fallback
    assert si.response_meta.response_reflection.code_request_repair.selected_source ==
             :bounded_template
    assert si.response_meta.response_reflection.code_request_repair.executed? == false

    assert_receive {:code_request_llm_chat, _, _}
    assert_receive {:code_request_llm_chat, _, _}
    refute_receive {:code_request_llm_chat, _, _}, 30
  end

  test "completed code is accepted with no retry" do
    start_supervised!({FakeLlm, {self(), [@example_code]}})
    si = Attach.maybe_build_response_plan(input(), response: :auto)
    assert si.response_text =~ "defmodule MyExample do"
    assert si.response_meta.response_reflection.status == :accept
    refute Map.has_key?(si.response_meta.response_reflection, :code_request_repair)
    assert_receive {:code_request_llm_chat, _, _}
    refute_receive {:code_request_llm_chat, _, _}, 30
  end

  test "fabricated structure and unsafe or non-code requests do not activate fallback" do
    ctx = response_context(input())
    for changes <- [
      %{features: Map.put(ctx.features, :guardrail?, true)},
      %{features: Map.put(ctx.features, :risk_bucket, :high)},
      %{features: Map.put(ctx.features, :selected_action, :ask_clarifying_question)},
      %{request_structure: Map.put(ctx.request_structure, :module_name, "MyExample;exit")},
      %{request_structure: Map.put(ctx.request_structure, :requested_object, "Report")},
      %{request_structure: Map.put(ctx.request_structure, :meaning_verified?, true)},
      %{user_text: "Write a book about Elixir"}
    ] do
      modified = Map.merge(ctx, changes)
      refute CodeRequest.eligible?(modified)
      refute CodeRequest.missing?(modified, @deferral)
      assert CodeRequest.fallback(modified) == nil
    end

    refute CodeRequest.eligible?(%{})
    refute CodeRequest.eligible_structure?(nil, @question)
  end
end
