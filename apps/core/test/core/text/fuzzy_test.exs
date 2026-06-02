defmodule Core.Text.FuzzyTest do
  use ExUnit.Case, async: true

  alias Core.Text.Fuzzy

  test "repairs inspectable typo cues without exposing char grams" do
    fuzzy = Fuzzy.interpret("wahts my naem?")

    assert fuzzy.text == "whats my name?"
    assert :asking_for_user_name in fuzzy.aliases

    assert [
             %{original: "wahts", replacement: "whats", reason: :known_typo},
             %{original: "naem", replacement: "name", reason: :known_typo}
           ] = fuzzy.corrections

    assert Enum.any?(fuzzy.evidence, &(&1.role == :fuzzy_correction))
    assert Enum.any?(fuzzy.evidence, &(&1.role == :fuzzy_alias))
    refute Map.has_key?(fuzzy, :chargrams)
  end

  test "expands phrase aliases for identity queries" do
    fuzzy = Fuzzy.interpret("what do you call me")

    assert fuzzy.corrections == []
    assert fuzzy.aliases == [:asking_for_user_name]
    assert fuzzy.confidence >= 0.89
  end

  test "repairs remember directive typos" do
    fuzzy = Fuzzy.interpret("remeber that my favorite color is blue")

    assert fuzzy.text == "remember that my favorite color is blue"
    assert :memory_write in fuzzy.aliases
  end

  test "normalizes casual forms and common memory phrasing" do
    fuzzy = Fuzzy.interpret("plz rmeember that my colur is blue")

    assert fuzzy.text == "please remember that my color is blue"
    assert :memory_write in fuzzy.aliases

    assert Enum.any?(fuzzy.corrections, &(&1.original == "plz" and &1.replacement == "please"))

    assert Enum.any?(
             fuzzy.corrections,
             &(&1.original == "rmeember" and &1.replacement == "remember")
           )

    assert Enum.any?(fuzzy.corrections, &(&1.original == "colur" and &1.replacement == "color"))
  end

  test "adds inspectable aliases for fact, location, definition, translation, help, debug, and thanks" do
    cases = [
      {"wat is my favorite colur?", :fact_query, "what is my favorite color?"},
      {"wher do i liv?", :location_query, "where do i live?"},
      {"waht does this meen?", :definition_query, "what does this mean?"},
      {"tranlsate hola into englis", :translation_request, "translate hola into english"},
      {"pls help me", :help_request, "please help me"},
      {"this issue doesnt work", :debug_request, "this issue does not work"},
      {"thx that helped", :thanks, "thanks that helped"}
    ]

    for {raw, alias_name, text} <- cases do
      fuzzy = Fuzzy.interpret(raw)

      assert fuzzy.text == text
      assert alias_name in fuzzy.aliases
      assert Enum.any?(fuzzy.evidence, &(&1.role == :fuzzy_alias and &1.alias == alias_name))
    end
  end

  test "does not rewrite already-known vocabulary through edit distance" do
    fuzzy = Fuzzy.interpret("what did i tell you about my sister?")

    assert fuzzy.text == "what did i tell you about my sister?"
    assert fuzzy.corrections == []
    assert :fact_query in fuzzy.aliases
  end

  test "protects think and only repairs ddo in short conversational input" do
    clean = Fuzzy.interpret("what do you think", known_word?: fn _ -> false end)

    assert clean.text == "what do you think"
    assert clean.corrections == []

    typo = Fuzzy.interpret("what ddo you think", known_word?: fn _ -> false end)

    assert typo.text == "what do you think"

    assert [
             %{original: "ddo", replacement: "do", reason: :known_typo}
           ] = typo.corrections
  end

  test "protects caller-known vocabulary before fuzzy repair" do
    fuzzy = Fuzzy.interpret("my new plaace", known_word?: &(&1 == "plaace"))

    assert fuzzy.text == "my new plaace"
    assert fuzzy.corrections == []
  end

  test "uses wildcard context frames to prefer phrase-compatible repairs" do
    fuzzy = Fuzzy.interpret("my new plaace", known_word?: fn _ -> false end)

    assert fuzzy.text == "my new place"

    assert [
             %{original: "plaace", replacement: "place", reason: :context_frame}
           ] = fuzzy.corrections
  end

  test "context frames can contribute candidates without creating trusted phrases" do
    fuzzy = Fuzzy.interpret("math exma", known_word?: fn _ -> false end)

    assert fuzzy.text == "math exam"

    assert [
             %{original: "exma", replacement: "exam", reason: :context_frame}
           ] = fuzzy.corrections

    refute Map.has_key?(fuzzy, :mwe)
  end

  test "semantic candidate scores can break spelling ties without becoming hard truth" do
    fuzzy =
      Fuzzy.interpret("plaase",
        known_word?: fn _ -> false end,
        candidate_lookup: fn _ ->
          [
            %{norm: "please", context_score: 0.94},
            %{norm: "place", context_score: 0.10}
          ]
        end
      )

    assert fuzzy.text == "please"

    assert [
             %{original: "plaase", replacement: "please", reason: :pgvector_context}
           ] = fuzzy.corrections
  end
end
