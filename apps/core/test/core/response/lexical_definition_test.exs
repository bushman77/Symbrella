defmodule Core.Response.LexicalDefinitionTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.{AttentionFallback, LexicalDefinition}

  setup do
    previous = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)
    on_exit(fn ->
      if previous == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, previous)
      end
    end)
    :ok
  end

  defp row(term, pos, sense, definition) do
    %{
      id: "#{term}|#{pos}|#{sense}", norm: term, pos: pos,
      definition: definition, token_index: 5, source: :ltm
    }
  end

  defp si(sentence, rows) do
    %{
      sentence: sentence, text: sentence, source: :user,
      intent: :define, keyword: "meaning", confidence: 0.625,
      session_id: "lexical-definition-#{System.unique_integer([:positive])}",
      active_cells: rows,
      wm_context: %{recognized_topics: ["meaning"]},
      lifg_choices: [%{
        token_index: 5, chosen_id: "poop|noun|1", margin: 0.0
      }]
    }
  end

  test "parses the exact lexical target, never the keyword meaning" do
    assert LexicalDefinition.requested_term("what is the meaning of poop") == "poop"
    assert LexicalDefinition.requested_term("What does poop mean?") == "poop"
    assert LexicalDefinition.requested_term("Define poop.") == "poop"
    assert LexicalDefinition.requested_term("What is the definition of poop?") == "poop"
    assert LexicalDefinition.requested_term("What is the meaning of poop and why?") == nil
    assert LexicalDefinition.requested_term("What is the meaning of") == nil
  end

  test "in live no-LLM routing, target gets LTM gloss, not attention's keyword" do
    sentence = "what is the meaning of poop"
    turn = si(sentence, [
      row("meaning", "adjective", 1, "Having a particular interpretation."),
      row("poop", "noun", 1, "Faeces or excrement."),
      row("poop", "noun", 2, "Information or news."),
      row("poop", "noun", 3, "The raised deck at the stern of a ship.")
    ])

    {_tone, answer, meta} = Response.plan(turn, %{})

    assert answer =~ "\"poop\" has multiple recorded meanings"
    assert answer =~ "Faeces or excrement."
    assert answer =~ "Information or news."
    refute answer =~ "what you're saying about meaning"
    assert meta.response_source == :symbolic_lexical_definition
    assert meta.response_reflection.requested_term == "poop"
    assert meta.response_reflection.selected_sense_verified? == false
    assert meta.response_reflection.llm_called? == false
    assert meta.curiosity_probe == nil
  end

  test "does not invent an unsupported definition from attention or another word's rows" do
    sentence = "what is the meaning of poop"
    turn = si(sentence, [
      row("meaning", "noun", 1, "A sense conveyed by words.")
    ])

    {_tone, answer, meta} = Response.plan(turn, %{})
    assert answer =~ "what \"poop\" means"
    assert answer =~ "don't have a usable dictionary definition"
    refute answer =~ "A sense conveyed by words."
    assert meta.response_reflection.lexicon_rows_used? == false
    assert meta.response_source == :symbolic_lexical_definition
  end

  test "cannot take lexical rows from another target or a non-LTM source" do
    sentence = "what is the meaning of poop"
    fake = Map.put(row("poop", "noun", 1, "Unverified placeholder."), :source, :user)
    turn = si(sentence, [fake])

    assert {:ok, text, %{status: :lexical_definition_missing}} =
      LexicalDefinition.generate(sentence, %{risk_bucket: :low},
        %{action: :answer}, turn)
    assert text =~ "don't have a usable dictionary definition"

    assert :not_applicable ==
      LexicalDefinition.generate(sentence, %{risk_bucket: :high},
        %{action: :answer}, turn)
    assert :not_applicable ==
      LexicalDefinition.generate(sentence, %{risk_bucket: :low},
        %{action: :answer}, %{turn | sentence: "different input"})
  end

  test "attention fallback abstains for definition intent and zero-salience focus" do
    sentence = "what is the meaning of poop"
    turn = si(sentence, [])
    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :define}, %{action: :answer}, turn)
  end

  test "admitted comprehension without attention goals cannot fabricate focus" do
    alias Core.Brain.WM.Comprehension, as: WMComprehension
    sid = "zero-salience-definition-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "what is the meaning of poop"

    turn =
      WMComprehension.store(%{
        source: :user, session_id: sid, sentence: sentence,
        intent: :define,
        comprehension: %{recognized_topics: ["meaning"], understood: [],
                         uncertain: ["poop"], degraded?: true},
        trace: []
      }, comprehension_wm?: true)

    assert turn.wm_context.attention.goal_terms == []
    assert turn.wm_context.attention.salience == 0.0
    assert WMComprehension.admitted_context(sid) == turn.wm_context

    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :ask_info}, %{action: :answer}, turn)
  end
end
