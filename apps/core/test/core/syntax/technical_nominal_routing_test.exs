defmodule Core.Syntax.TechnicalNominalRoutingTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.Syntax.{NominalEvidence, RoleCompetition, ArgumentEvidence}
  alias Brain.LIFG.{GrammarEvidence, SenseFocus, Stage1}

  defp cell(word, pos, tags \\ []) do
    %{id: "#{word}|#{pos}|1", norm: word, pos: pos,
      semantic_atoms: tags, activation: 0.5}
  end

  defp input(words, cells) do
    {tokens, _end} =
      words
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {word, idx}, start ->
        finish = start + byte_size(word)
        {%{index: idx, phrase: word, span: {start, finish},
           n: 1, mw: false, kind: :word}, finish + 1}
      end)

    %SemanticInput{
      sentence: Enum.join(words, " "),
      tokens: tokens, active_cells: cells
    }
  end

  defp cells do
    [cell("can", "auxiliary"), cell("you", "pronoun"),
     cell("write", "verb", ["tag:ditransitive"]),
     %{cell("write", "verb") | id: "write|verb|5"},
     %{cell("write", "verb") | id: "write|verb|2",
       semantic_atoms: ["tag:transitive"]},
     cell("me", "pronoun"), cell("a", "determiner"),
     cell("basic", "adjective"), cell("basic", "noun"),
     cell("basic", "proper_noun"), cell("in", "preposition"),
     cell("module", "noun"), cell("called", "verb")]
  end

  test "technical unknowns stay unresolved while modifying adjective gets structural support" do
    si = input(~w(Can you write me a basic GenServer in a module called MyExample?), cells())
    out = RoleCompetition.attach(si)
    proof = out.role_competition

    assert Enum.map(proof.unresolved_tokens,
      &{&1.token_index, &1.surface, &1.kind}) == [
        {6, "GenServer", :technical_identifier},
        {11, "MyExample", :declared_identifier}
      ]

    assert [%{modifier: 5, head: 6, head_status: :lexically_unresolved}] =
             proof.nominal_frames
    assert %{status: :supported, pos: "adjective", role: :modifier,
             basis: :determined_adjective_before_unresolved_head} =
             proof.roles[5]

    scores = %{"basic|proper_noun|1" => 0.53,
               "basic|noun|1" => 0.54,
               "basic|adjective|1" => 0.40}
    revised = GrammarEvidence.apply_role_competition(
      scores, GrammarEvidence.prepare(out), 5)
    assert revised["basic|adjective|1"] > revised["basic|noun|1"]
    assert revised["basic|adjective|1"] > revised["basic|proper_noun|1"]

    focus = SenseFocus.plan(out, 5,
      Enum.filter(cells(), &(&1.norm == "basic")))
    assert focus.mode == :focused
    assert focus.focus_pos == "adjective"
    assert focus.context_evaluations == 1
    assert focus.context_skipped == 2

    assert si.active_cells == out.active_cells
    assert si.tokens == out.tokens
    refute Enum.any?(out.active_cells,
      &(&1.id in ["genserver|noun|1", "myexample|noun|1"]))
    assert out.trace |> hd() |> Map.get(:meta) |> Map.get(:unresolved_code_tokens) == 2
    assert :ok == SemanticInput.validate_contract(out)
  end

  test "live Stage1 prefers witnessed modifier POS while retaining entire candidate slate" do
    si = input(~w(Can you write me a basic GenServer), cells())
    original = Enum.filter(cells(), &(&1.norm == "basic"))
    out = %{RoleCompetition.attach(si) | sense_candidates: %{5 => original}}

    assert {:ok, %{choices: choices}} =
             Stage1.run(out, scores: :all, preserve_sense_candidates: true,
               db_backfill?: false, semantic_control?: false,
               weights: [lex_fit: 0.0, context_fit: 0.0,
                         rel_prior: 0.0, activation: 1.0, intent_bias: 0.0])

    choice = Enum.find(choices, &(&1.token_index == 5))
    assert choice
    assert choice.sense_focus.mode == :focused
    assert String.contains?(choice.chosen_id, "|adjective|")
    assert Map.keys(choice.scores) |> Enum.sort() ==
             Enum.sort(Enum.map(original, & &1.id))
  end

  test "recipient and unresolved requested artifact remain role hypotheses" do
    si = input(~w(Can you write me a basic GenServer in a module called MyExample?), cells())
    report = RoleCompetition.attach(si).role_competition

    assert [%{status: :candidate, construction: :recipient_object,
              predicate: 2, recipient: 3, object_modifier: 5, object_head: 6,
              object_status: :lexically_unresolved_code_head,
              meaning_verified?: false, event_verified?: false}] =
             report.argument_frames

    assert [%{status: :shadow_only, applied_to_lifg?: false,
              winner_changed?: false, semantic_sense_verified?: false,
              candidate_count: 3} = comparison] = report.argument_compatibility

    rows = Map.new(comparison.candidates, &{&1.id, &1})
    assert rows["write|verb|1"].positive_support == 0.10
    assert rows["write|verb|5"].positive_support == 0.10
    assert rows["write|verb|2"].positive_support == 0.0
  end

  test "ordinary noun objects, recipient objects and absent objects" do
    lexical = cells() ++ [cell("book", "noun"), cell("letter", "noun")]
    book = input(~w(Can you write a book?), lexical)
    letter = input(~w(Can you write me a letter?), lexical)
    incomplete = input(~w(Can you write me?), lexical)

    assert [%{construction: :direct_object, recipient: nil,
              object_surface: "book", object_status: :dictionary_noun_witness}] =
             RoleCompetition.attach(book).role_competition.argument_frames

    assert [%{construction: :recipient_object, recipient: 3,
              object_surface: "letter"}] =
             RoleCompetition.attach(letter).role_competition.argument_frames

    assert RoleCompetition.attach(incomplete).role_competition.argument_frames == []
  end

  test "no invented code senses, no POS inference from a CamelCase word alone" do
    si = input(~w(Please examine GenServer), [cell("examine", "verb")])
    nominal = NominalEvidence.detect(si)
    assert [%{surface: "GenServer", kind: :technical_identifier}] =
             nominal.unresolved_tokens
    assert nominal.proposals == []
    assert RoleCompetition.attach(si).role_competition.roles == %{}

    plain = input(~w(a basic building), [
      cell("a", "determiner"), cell("basic", "adjective"),
      cell("building", "noun")
    ])
    assert NominalEvidence.detect(plain).proposals == []

    no_adjective = input(~w(a basic GenServer), [
      cell("a", "determiner"), cell("basic", "noun")
    ])
    assert NominalEvidence.detect(no_adjective).proposals == []

    fake = %{role_competition: %{roles: %{2 => %{status: :supported,
      role: :predicate, pos: "verb"}}}}
    assert ArgumentEvidence.detect(si, fake, nominal) == []
  end
end
