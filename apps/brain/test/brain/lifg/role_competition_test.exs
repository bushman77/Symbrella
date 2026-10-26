defmodule Brain.LIFG.RoleCompetitionTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.{RoleCompetition, GrammarEvidence}

  defp token_words(words) do
    words
    |> Enum.with_index()
    |> Enum.map(fn {phrase, index} ->
      %{phrase: phrase, index: index, n: 1}
    end)
  end

  defp cell(word, pos, tags \\ []) do
    %{id: "#{word}|#{pos}|1", norm: word, pos: pos, semantic_atoms: tags}
  end

  defp np(span, roles) do
    %{id: :noun_phrase, source: :core_syntax, span: span, roles: roles,
      evidence: [:nominal_constituent, :pos_pattern]}
  end

  defp light_input do
    %{
      sentence: "She carried a light suitcase.",
      tokens: token_words(["She", "carried", "a", "light", "suitcase."]),
      active_cells: [
        cell("a", "determiner"), cell("a", "adjective"),
        cell("light", "adjective"), cell("light", "noun"),
        cell("suitcase", "noun")
      ],
      sentence_signatures: [
        np({2, 4}, %{determiner: 2, modifier: 3, head: 4}),
        np({2, 4}, %{modifier_1: 2, modifier_2: 3, head: 4}),
        np({2, 3}, %{determiner: 2, head: 3}),
        np({3, 4}, %{modifier: 3, head: 4})
      ]
    }
  end

  defp bat_input do
    %{
      sentence: "A bat flew through the cave.",
      tokens: token_words(["A", "bat", "flew", "through", "the", "cave."]),
      active_cells: [
        cell("a", "determiner"), cell("bat", "noun"),
        cell("bat", "verb"),
        cell("flew", "verb", ["tag:form-of", "tag:past"]),
        cell("flew", "noun"), cell("through", "preposition")
      ],
      sentence_signatures: [
        %{id: :verb_particle_construction, source: :core_syntax,
          span: {1, 3}, roles: %{verb: 1, object: 2, particle: 3},
          evidence: [:separated_particle, :object_between]},
        np({0, 1}, %{determiner: 0, head: 1}),
        %{id: :finite_past_candidate, source: :core_syntax,
          span: {2, 2}, roles: %{verb: 2},
          evidence: [:simple_past_form_available]}
      ]
    }
  end

  test "full determiner-adjective-noun interpretation outranks shorter incompatible phrase hypotheses" do
    si = light_input()
    result = RoleCompetition.resolve(si)
    assert %{pos: "adjective", role: :modifier,
             basis: :complete_determiner_adjective_noun,
             status: :supported, span: {2, 4}} = result.roles[3]
    refute Map.has_key?(result.roles, 2)
    refute Map.has_key?(result.roles, 4)

    scores = %{
      "light|noun|7" => 0.51,
      "light|adjective|2" => 0.40,
      "light|adjective|fallback" => 0.60
    }
    ctx = GrammarEvidence.prepare(Map.put(si, :role_competition, result))
    revised = GrammarEvidence.apply_role_competition(scores, ctx, 3)

    assert_in_delta revised["light|adjective|2"], 0.70, 1.0e-10
    assert revised["light|noun|7"] == 0.51
    assert revised["light|adjective|fallback"] == 0.60
    assert GrammarEvidence.apply_role_competition(scores, ctx, 0) == scores

    # Even this strong POS evidence does not choose an adjective sense ID.
    assert result.roles[3].pos == "adjective"
    refute Map.has_key?(result.roles[3], :sense_id)
    assert si == light_input()
  end

  test "an attested finite-past predicate supports preceding determiner-noun subject" do
    si = bat_input()
    result = RoleCompetition.resolve(si)
    assert %{pos: "noun", role: :subject_head,
             basis: :determined_subject_before_attested_past_predicate,
             status: :supported} = result.roles[1]

    scores = %{
      "bat|noun|1" => 0.40, "bat|noun|20" => 0.40,
      "bat|verb|3" => 0.55, "bat|proper_noun|1" => 0.60
    }
    ctx = GrammarEvidence.prepare(Map.put(si, :role_competition, result))
    updated = GrammarEvidence.apply_role_competition(scores, ctx, 1)

    assert_in_delta updated["bat|noun|1"], 0.70, 1.0e-10
    assert_in_delta updated["bat|noun|20"], 0.70, 1.0e-10
    assert updated["bat|verb|3"] == 0.55
    assert updated["bat|proper_noun|1"] == 0.60
  end

  test "an unverified past tense, absent morphology, or incomplete clause is not POS evidence" do
    si = bat_input()
    without_past =
      %{si | active_cells:
        Enum.reject(si.active_cells, &(&1.norm == "flew" and &1.pos == "verb"))}
    assert RoleCompetition.resolve(without_past).roles == %{}

    without_signature =
      %{si | sentence_signatures:
        Enum.reject(si.sentence_signatures, &(&1.id == :finite_past_candidate))}
    assert RoleCompetition.resolve(without_signature).roles == %{}

    without_np = %{si | sentence_signatures:
      Enum.reject(si.sentence_signatures, &(&1.id == :noun_phrase))}
    assert RoleCompetition.resolve(without_np).roles == %{}

    light = light_input()
    no_adjective =
      %{light | active_cells:
        Enum.reject(light.active_cells, &(&1.norm == "light" and &1.pos == "adjective"))}
    assert RoleCompetition.resolve(no_adjective).roles == %{}

    no_determiner = %{light | tokens:
      List.update_at(light.tokens, 2, &Map.put(&1, :phrase, "maybe"))}
    assert RoleCompetition.resolve(no_determiner).roles == %{}

    assert GrammarEvidence.apply_role_competition(
      %{"bat|noun|1" => 0.3}, GrammarEvidence.prepare(si), 1
    ) == %{"bat|noun|1" => 0.3}
  end

  test "adjudication does not mutate lexical data, create candidate IDs, or assert sense truth" do
    si = bat_input()
    result = RoleCompetition.resolve(si)
    assert Map.keys(result.roles) == [1]
    assert result.source == :core_syntax_and_attested_dictionary_pos
    assert result.decisions |> Enum.all?(&(&1.status in [:supported, :unresolved]))
    assert si.active_cells == bat_input().active_cells
    refute Map.has_key?(result, :selected_senses)
    refute Map.has_key?(result, :verified_facts)
  end

  test "full definition command resolves both POS conflicts with exact dictionary IDs" do
    cells = [
      cell("define", "noun"), cell("define", "verb"),
      cell("artist", "noun"), cell("artist", "proper_noun")
    ]

    signature = %{
      id: :definition_imperative, source: :core_syntax,
      span: {0, 1}, roles: %{predicate: 0, object_head: 1},
      evidence: [:complete_imperative_surface, :explicit_definition_command,
                 :attested_verb_alternative, :attested_common_noun_object]
    }

    si = %{
      intent: :define, sentence: "define artist",
      tokens: token_words(["define", "artist"]), active_cells: cells,
      sentence_signatures: [signature]
    }
    resolved = RoleCompetition.resolve(si)

    assert %{pos: "verb", basis: :definition_imperative_predicate,
             status: :supported} = resolved.roles[0]
    assert %{pos: "noun", basis: :definition_imperative_object,
             status: :supported} = resolved.roles[1]

    ctx = GrammarEvidence.prepare(Map.put(si, :role_competition, resolved))

    predicate = GrammarEvidence.apply_role_competition(
      %{"define|noun|1" => 0.40, "define|verb|1" => 0.30}, ctx, 0)
    assert predicate["define|verb|1"] > predicate["define|noun|1"]

    object = GrammarEvidence.apply_role_competition(
      %{"artist|noun|1" => 0.30, "artist|proper_noun|1" => 0.32}, ctx, 1)
    assert object["artist|noun|1"] > object["artist|proper_noun|1"]

    refute Map.has_key?(resolved.roles[0], :sense_id)
    refute Map.has_key?(resolved.roles[1], :sense_id)

    assert RoleCompetition.resolve(%{si | active_cells:
      Enum.reject(cells, &(&1.pos == "noun" and &1.norm == "artist"))}).roles == %{}
    assert RoleCompetition.resolve(%{si | intent: :unknown}).roles == %{}
    assert RoleCompetition.resolve(%{si | sentence_signatures: []}).roles == %{}

    # Recheck the determiner witness even for a caller-supplied signature.
    with_determiner = %{
      si | tokens: token_words(["define", "the", "artist"]),
           sentence: "define the artist",
           sentence_signatures: [%{signature |
             span: {0, 2},
             roles: %{predicate: 0, object_determiner: 1, object_head: 2}}]
    }
    assert RoleCompetition.resolve(with_determiner).roles == %{}

    checked = %{with_determiner |
      active_cells: [cell("the", "determiner") | cells]}
    assert %{pos: "noun", status: :supported} =
      RoleCompetition.resolve(checked).roles[2]
  end

end
