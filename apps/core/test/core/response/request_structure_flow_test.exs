defmodule Core.Response.RequestStructureFlowTest do
  use ExUnit.Case, async: false

  alias Core.Response.{Context, LlmPrompt, RequestStructure}
  alias Core.Syntax.RoleCompetition
  alias Core.SemanticInput

  @question "Can you write me a basic GenServer in a module called MyExample?"

  defp cell(word, pos, tags \\ []) do
    %{id: "#{word}|#{pos}|1", norm: word, pos: pos,
      semantic_atoms: tags, activation: 0.5}
  end

  defp si(sentence \\ @question) do
    tokens =
      Regex.scan(~r/\S+/u, sentence, return: :index)
      |> Enum.with_index()
      |> Enum.map(fn {[{start, size}], index} ->
        %{index: index, phrase: binary_part(sentence, start, size),
          span: {start, start + size}, kind: :word, n: 1, mw: false}
      end)

    cells = [
      cell("can", "auxiliary"), cell("you", "pronoun"),
      cell("write", "verb", ["tag:ditransitive"]),
      cell("me", "pronoun"), cell("a", "determiner"),
      cell("basic", "adjective"), cell("basic", "noun"),
      cell("basic", "proper_noun"), cell("in", "preposition"),
      cell("module", "noun"), cell("called", "verb")
    ]

    %SemanticInput{sentence: sentence, tokens: tokens, active_cells: cells}
    |> RoleCompetition.attach()
  end

  test "real Core role adjudication survives response context and reaches standard prompt" do
    original = si()
    assert original.role_competition.roles[2].role == :predicate
    assert original.role_competition.roles[5].pos == "adjective"

    embedded = Context.from_si(original)
    assert %{
      version: 1, status: :grammatical_candidate,
      predicate: "write", recipient: "me",
      requested_object: "GenServer", object_modifier: "basic",
      module_name: "MyExample", meaning_verified?: false,
      code_semantics_verified?: false
    } = embedded.request_structure

    context = Context.from_response_parts(original.sentence,
      %{intent: :ask, session_id: "test-turn", turn_context: embedded},
      %{mode: :collaborator, action: :answer_user, tone: :neutral},
      %{}, %{wm_items: [], runtime_state: %{}})

    assert context.request_structure == embedded.request_structure
    prompt = LlmPrompt.build_system_prompt(context)

    assert prompt =~ "CURRENT-TURN REQUEST STRUCTURE"
    assert prompt =~ ~s(predicate="write")
    assert prompt =~ ~s(recipient="me")
    assert prompt =~ ~s(requested_object="GenServer")
    assert prompt =~ ~s(object_modifier="basic")
    assert prompt =~ ~s(module_name="MyExample")
    assert prompt =~ "original user text takes precedence"
    assert prompt =~ "do not mistake missing lexicon entries for an invalid request"
    assert prompt =~ "Never claim the requested module already exists"

    assert :ok == SemanticInput.validate_contract(original)
    assert original.role_competition.argument_compatibility != []
    assert Enum.all?(original.role_competition.argument_compatibility,
      &(&1.applied_to_lifg? == false))
  end

  test "the same current-turn evidence also reaches MythoMax's compact character card" do
    previous = Application.get_env(:core, :response_prompt_profile)

    on_exit(fn ->
      if is_nil(previous) do
        Application.delete_env(:core, :response_prompt_profile)
      else
        Application.put_env(:core, :response_prompt_profile, previous)
      end
    end)

    Application.put_env(:core, :response_prompt_profile, :mythomax)

    embedded = Context.from_si(si())
    context = Context.from_response_parts(@question,
      %{intent: :ask, turn_context: embedded},
      %{mode: :collaborator, action: :answer_user, tone: :warm},
      %{}, %{wm_items: [], runtime_state: %{}})

    prompt = LlmPrompt.build_system_prompt(context)
    assert prompt =~ "locally running neuro-symbolic conversational companion"
    assert prompt =~ "CURRENT-TURN REQUEST STRUCTURE"
    assert prompt =~ ~s(requested_object="GenServer")
    assert prompt =~ ~s(module_name="MyExample")
  end

  test "missing, contradictory, or fabricated evidence cannot be promoted to prompt structure" do
    source = si()

    bad_object =
      put_in(source.role_competition.argument_frames,
        [Map.put(hd(source.role_competition.argument_frames),
          :object_surface, "SomethingElse")])

    bad_name =
      put_in(source.role_competition.identifier_bindings,
        [Map.put(hd(source.role_competition.identifier_bindings),
          :container_name_surface, "DifferentName")])

    bad_role =
      put_in(source.role_competition.roles[2].evidence,
        [:auxiliary_subject_order])

    for invalid <- [bad_object, bad_role] do
      assert RequestStructure.from_si(invalid) == nil
      refute Map.has_key?(Context.from_si(invalid), :request_structure)
    end

    result = RequestStructure.from_si(bad_name)
    assert is_map(result)
    refute Map.has_key?(result, :module_name)

    # A response boundary may not infer a candidate request from raw text.
    assert RequestStructure.from_si(%{sentence: @question}) == nil
    refute LlmPrompt.build_system_prompt(%{features: %{intent: :ask}}) =~
      "CURRENT-TURN REQUEST STRUCTURE"
  end

  test "a normal question without a supported recipient-object structure abstains" do
    neutral = si("Can you write?")
    assert RequestStructure.from_si(neutral) == nil

    neutral_context = Context.from_si(neutral)
    refute Map.has_key?(neutral_context, :request_structure)
  end
end
