defmodule Core.Pipeline.LexicalFocusTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.LexicalFocus
  alias Core.SemanticInput

  defp bank_rows do
    for n <- 1..45 do
      %{id: "bank|noun|#{n}", token_index: 0, norm: "bank",
        pos: "noun", definition: "Dictionary bank sense #{n}."}
    end
  end

  test "45 bank senses remain retrievable while the focus shows only 8" do
    rows = bank_rows()
    si = %SemanticInput{
      sentence: "The bank was near the river.",
      tokens: [%{index: 0, phrase: "bank", n: 1}],
      active_cells: rows
    }

    output = LexicalFocus.attach(si)

    assert output.active_cells == rows
    assert output.tokens == si.tokens
    assert output.sense_candidates == si.sense_candidates
    assert output.lexical_focus.mode == :observe_only
    assert output.lexical_focus.sense_verified? == false
    assert output.lexical_focus.positions[0].total_senses == 45
    assert length(output.lexical_focus.positions[0].focus) == 8
    assert output.lexical_focus.positions[0].deferred_senses == 37
    assert length(LexicalFocus.expand(output, 0)) == 45
    assert Enum.sort(Enum.map(LexicalFocus.expand(output, 0), & &1.id)) ==
             Enum.sort(Enum.map(rows, & &1.id))
    assert :ok = SemanticInput.validate_contract(output)
    assert {:ok, normalized} = SemanticInput.normalize_contract(output)
    assert normalized.lexical_focus.positions[0].total_senses == 45
  end

  test "supported grammar prioritizes an attested POS without removing noun alternatives" do
    rows = [
      %{id: "light|noun|1", token_index: 1, norm: "light", pos: "noun",
        definition: "Visible radiation."},
      %{id: "light|adjective|1", token_index: 1, norm: "light", pos: "adjective",
        definition: "Having low weight."},
      %{id: "light|noun|2", token_index: 1, norm: "light", pos: "noun",
        definition: "A lamp."},
      %{id: "light|verb|1", token_index: 1, norm: "light", pos: "verb",
        definition: "To ignite."}
    ]

    si = %SemanticInput{
      sentence: "a light suitcase",
      tokens: [%{index: 1, phrase: "light", n: 1}],
      active_cells: rows,
      role_competition: %{
        source: :core_syntax_and_attested_dictionary_pos,
        roles: %{1 => %{status: :supported, pos: "adjective"}}
      }
    }

    out = LexicalFocus.attach(si, lexical_focus_size: 2)
    assert hd(out.lexical_focus.positions[1].focus).id == "light|adjective|1"
    assert out.lexical_focus.positions[1].supported_pos == "adjective"
    assert out.lexical_focus.positions[1].deferred_senses == 2
    assert Enum.map(out.lexical_focus.positions[1].focus, & &1.pos) |> Enum.uniq() |> length() == 2
    assert length(LexicalFocus.expand(out, 1)) == 4
    assert out.active_cells == rows
    assert out.role_competition == si.role_competition
  end

  test "dictionary context differentiates senses without claiming a final semantic winner" do
    si = %SemanticInput{
      sentence: "The fisherman sat on the bank of the river.",
      tokens: [%{index: 5, phrase: "bank", n: 1}],
      active_cells: [
        %{id: "bank|noun|financial", token_index: 5, pos: "noun",
          definition: "A financial institution that holds money and makes loans."},
        %{id: "bank|noun|river", token_index: 5, pos: "noun",
          definition: "The sloping land alongside a river."}
      ]
    }

    out = LexicalFocus.attach(si, lexical_focus_size: 1)

    assert [%{id: "bank|noun|river", grammatical_support?: false}] =
             out.lexical_focus.positions[5].focus
    assert out.lexical_focus.positions[5].deferred_senses == 1
    assert out.lexical_focus.positions[5].verification == :candidate_hypotheses_only
    assert length(LexicalFocus.expand(out, 5)) == 2
  end

  test "repeated words keep independent focus and complete per-position recall" do
    rows = [
      %{id: "bank|noun|1", token_index: 0, pos: "noun", definition: "River edge."},
      %{id: "bank|noun|2", token_index: 0, pos: "noun", definition: "Money institution."},
      %{id: "bank|noun|1", token_index: 2, pos: "noun", definition: "River edge."},
      %{id: "bank|noun|2", token_index: 2, pos: "noun", definition: "Money institution."}
    ]
    si = %SemanticInput{sentence: "bank near bank",
      tokens: [%{index: 0, phrase: "bank"}, %{index: 2, phrase: "bank"}],
      active_cells: rows,
      sense_candidates: %{0 => [%{id: "bank|noun|2"}],
                          2 => [%{id: "bank|noun|1"}]},
      neuron_activation: %{"bank|noun|1" => 0.3}}

    out = LexicalFocus.attach(si, lexical_focus_size: 1)
    assert Map.keys(out.lexical_focus.positions) |> Enum.sort() == [0, 2]
    assert out.lexical_focus.positions[0].deferred_senses == 1
    assert out.lexical_focus.positions[2].deferred_senses == 1
    assert length(LexicalFocus.expand(out, 0)) == 2
    assert length(LexicalFocus.expand(out, 2)) == 2
    assert out.sense_candidates == si.sense_candidates
    assert out.neuron_activation == si.neuron_activation
    assert out.active_cells == rows
  end

  test "unsupported roles cannot fabricate grammatical support" do
    si = %SemanticInput{
      sentence: "light",
      tokens: [%{index: 0, phrase: "light", n: 1}],
      active_cells: [
        %{id: "light|noun|1", token_index: 0, pos: "noun",
          definition: "Visible radiation."}
      ],
      role_competition: %{
        source: :user_claim,
        roles: %{0 => %{status: :supported, pos: "noun"}}
      }
    }
    out = LexicalFocus.attach(si)
    assert out.lexical_focus.positions[0].supported_pos == nil
    assert out.lexical_focus.positions[0].status == :unresolved
    refute hd(out.lexical_focus.positions[0].focus).grammatical_support?
  end

  test "post-LIFG audit distinguishes focused, deferred, and out-of-LTM choices" do
    rows = bank_rows()
    si = %SemanticInput{
      sentence: "bank", tokens: [%{index: 0, phrase: "bank", n: 1}],
      active_cells: rows, sense_candidates: %{0 => [%{id: "bank|noun|45"}]}
    }
    focused = LexicalFocus.attach(si, lexical_focus_size: 8)

    in_focus_id = hd(focused.lexical_focus.positions[0].focus).id
    deferred_id =
      rows
      |> Enum.map(& &1.id)
      |> Enum.find(fn id ->
        not Enum.any?(focused.lexical_focus.positions[0].focus, &(&1.id == id))
      end)

    for {id, expected} <- [
          {in_focus_id, :in_focus},
          {deferred_id, :deferred},
          {"bank|noun|fallback", :not_in_ltm}
        ] do
      before = Map.put(focused, :lifg_choices, [%{id: id, token_index: 0}])
      after_audit = LexicalFocus.audit_lifg(before)

      assert after_audit.lexical_focus.lifg_alignment.compared == 1
      assert [%{token_index: 0, status: ^expected}] =
               after_audit.lexical_focus.lifg_alignment.outcomes
      assert after_audit.lifg_choices == before.lifg_choices
      assert after_audit.active_cells == rows
      assert after_audit.sense_candidates == before.sense_candidates
      assert after_audit.lexical_focus.lifg_alignment.semantic_accuracy == :not_measured
      refute after_audit.lexical_focus.lifg_alignment.changed_lifg_choice?
    end
  end

  test "large slates bypass ranking but preserve retrieval for reconsideration" do
    rows = for n <- 1..257 do
      %{id: "word|noun|#{n}", token_index: 0, pos: "noun"}
    end
    out = LexicalFocus.attach(%SemanticInput{sentence: "word", active_cells: rows})
    assert out.lexical_focus.status == :skipped_large_slate
    assert out.active_cells == rows
    assert length(LexicalFocus.expand(out, 0)) == 257
  end

  test "disabled focus leaves all cognitive state untouched" do
    si = %SemanticInput{
      sentence: "bank", active_cells: bank_rows(),
      tokens: [%{index: 0, phrase: "bank", n: 1}]
    }

    assert LexicalFocus.attach(si, lexical_focus?: false) == si
    assert LexicalFocus.expand(si, 1) == []
  end
end
