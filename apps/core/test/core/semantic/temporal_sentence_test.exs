defmodule Core.Semantic.TemporalSentenceTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.{SentenceMeaning, TemporalSentence}

  @before_words ~w(The bird is sitting on the fence before the cat is sleeping under the tree)
  @after_words ~w(The bird is sitting on the fence after the cat is sleeping under the tree)
  @pos ~w(determiner noun verb verb preposition determiner noun conjunction determiner noun verb verb preposition determiner noun)

  defp input(words \\ @before_words) do
    rows =
      Enum.zip(words, @pos)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos,
          semantic_atoms: if(norm in ["sitting", "sleeping"],
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
      end)

    %Core.SemanticInput{
      source: :test,
      intent: :statement,
      sentence: Enum.join(words, " ") <> ".",
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices: rows |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{token_index: index, id: row.id} end)
    }
  end

  defp live_style_input(words \\ @before_words) do
    si = input(words)
    winners = %{2 => "is|auxiliary|0", 3 => "sitting|adjective|1",
                10 => "is|auxiliary|0", 11 => "sleeping|adjective|1"}
    choices = Enum.map(si.lifg_choices, fn choice ->
      case Map.fetch(winners, choice.token_index) do
        {:ok, winner} ->
          choice
          |> Map.put(:id, winner)
          |> Map.put(:candidate_probs, %{choice.id => 0.7})
        :error -> choice
      end
    end)
    tokens = Enum.map(si.tokens, fn
      %{index: 14} = token -> %{token | phrase: token.phrase <> "."}
      token -> token
    end)
    %{si | tokens: tokens, lifg_choices: choices, source: :prod}
  end

  test "live token punctuation and attested verb alternatives compose both connectives" do
    for {words, connective, early, late} <- [
      {@before_words, :before, :sit, :sleep},
      {@after_words, :after, :sleep, :sit}
    ] do
      si = live_style_input(words) |> SentenceMeaning.attach()
      assert {:ok, meaning} = TemporalSentence.verified(si)
      assert meaning.kind == :explicit_temporal_report
      assert meaning.connective == connective
      assert meaning.temporal_claim.earlier_event_property.predicate == early
      assert meaning.temporal_claim.later_event_property.predicate == late
      alternatives =
        meaning.compositional_evidence.lexical_witnesses
        |> Enum.filter(&(&1.selection_basis == :construction_alternative))
        |> Enum.map(& &1.token_index)
      assert alternatives == [2, 3, 10, 11]
      assert meaning.factual_authority? == false
      assert meaning.temporal_claim.event_occurrence_verified? == false
    end
  end

  defp live_tied_after do
    si = live_style_input(@after_words)
    extra_rows = [
      %{id: "after|noun|1", norm: "after", pos: "noun", semantic_atoms: []},
      %{id: "after|conjunction|2", norm: "after", pos: "conjunction",
        semantic_atoms: []},
      %{id: "after|preposition|1", norm: "after", pos: "preposition",
        semantic_atoms: []}
    ]

    choices = Enum.map(si.lifg_choices, fn
      %{token_index: 7} = choice ->
        %{choice | id: "after|noun|1",
          candidate_probs: %{
            "after|conjunction|1" => 0.08,
            "after|conjunction|2" => 0.08,
            "after|preposition|1" => 0.08
          }}
      choice -> choice
    end)

    %{si | active_cells: extra_rows ++ si.active_cells,
      lifg_choices: choices}
  end

  test "live alternatives require current dictionary witnesses" do
    si = live_style_input()
    without_verb = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.id == "sitting|verb|1"))}
    assert TemporalSentence.build(without_verb) == nil
  end

  test "a witnessed before clause composes a speaker-attributed typed temporal meaning" do
    si = input() |> SentenceMeaning.attach()

    assert {:ok, meaning} = TemporalSentence.verified(si)
    assert meaning.kind == :explicit_temporal_report
    assert meaning.source == :revalidated_temporal_clause_composition
    assert meaning.connective == :before
    assert meaning.speech_act == :assertion
    assert meaning.temporal_claim.relation == :before
    assert meaning.temporal_claim.earlier_event_property.predicate == :sit
    assert meaning.temporal_claim.earlier_event_property.actor.kind == :bird
    assert meaning.temporal_claim.later_event_property.predicate == :sleep
    assert meaning.temporal_claim.later_event_property.actor.kind == :cat
    assert meaning.temporal_claim.speaker_attributed?
    refute meaning.temporal_claim.external_chronology_verified?
    refute meaning.temporal_claim.event_occurrence_verified?
    refute meaning.temporal_claim.referent_identity_verified?
    assert meaning.compositional_evidence.type ==
      {:arrow, :event, {:arrow, :event, :truth}}
    assert meaning.compositional_evidence.event_properties_verified?
    assert meaning.compositional_evidence.temporal_operator_verified?
    assert meaning.compositional_evidence.connective_witness_verified?
    assert length(meaning.compositional_evidence.lexical_witnesses) == 15
    assert Enum.all?(meaning.compositional_evidence.lexical_witnesses,
      &(&1.selection_basis == :selected))
    assert :actual_event_chronology in meaning.unresolved
    refute meaning.factual_authority?
    assert :ok = Core.SemanticInput.validate_contract(si)
  end

  test "after reverses claimed temporal direction without swapping the original clauses" do
    si = input(@after_words) |> SentenceMeaning.attach()
    assert {:ok, meaning} = TemporalSentence.verified(si)
    assert meaning.connective == :after
    assert Enum.map(meaning.compositional_evidence.source_event_semantics,
      & &1.predicate) == [:sit, :sleep]
    assert meaning.temporal_claim.earlier_event_property.predicate == :sleep
    assert meaning.temporal_claim.later_event_property.predicate == :sit
    assert meaning.compositional_evidence.result != nil
    assert meaning.factual_authority? == false
  end

  test "current lexical selections and participle morphology are required" do
    si = input()

    no_connective = %{si | lifg_choices:
      Enum.map(si.lifg_choices, fn
        %{token_index: 7} = choice -> %{choice | id: "before|noun|1"}
        choice -> choice
      end)}
    assert TemporalSentence.build(no_connective) == nil

    missing_predicate =
      %{si | active_cells: Enum.map(si.active_cells, fn
        %{norm: "sitting"} = cell -> %{cell | semantic_atoms: []}
        cell -> cell
      end)}
    assert TemporalSentence.build(missing_predicate) == nil

    no_dictionary = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.norm == "before"))}
    assert TemporalSentence.build(no_dictionary) == nil

    forged_lexical_choice = %{si | lifg_choices:
      Enum.map(si.lifg_choices, fn
        %{token_index: 3} = choice -> %{choice | id: "sitting|adjective|1"}
        choice -> choice
      end)}
    assert TemporalSentence.build(forged_lexical_choice) == nil
  end

  test "the complete original utterance and clause grammar are mandatory" do
    si = input()
    assert TemporalSentence.build(%{si | sentence:
      "The bird is sitting on the fence before the cat is sleeping under the tree?"}) == nil

    assert TemporalSentence.build(%{si | sentence:
      "The bird is sitting on the fence while the cat is sleeping under the tree."}) == nil

    assert TemporalSentence.build(%{si | sentence:
      "The bird is sitting on the fence before it is sleeping under the tree."}) == nil

    assert TemporalSentence.build(%{si | sentence:
      "The bird is sitting on the fence before the cat is sleeping under the tree. Extra."}) == nil

    assert TemporalSentence.build(%{si | source: :assistant}) == nil
  end

  test "stale, edited and unsupported temporal results never verify" do
    si = input() |> SentenceMeaning.attach()
    assert {:ok, _} = TemporalSentence.verified(si)

    incorrect = put_in(si, [Access.key!(:sentence_meaning),
      Access.key!(:temporal_claim), Access.key!(:external_chronology_verified?)], true)
    assert TemporalSentence.verified(incorrect) == :not_applicable

    wrong_connective = put_in(si,
      [Access.key!(:sentence_meaning), Access.key!(:connective)], :after)
    assert TemporalSentence.verified(wrong_connective) == :not_applicable

    mutated = %{si | sentence: Enum.join(@after_words, " ") <> "."}
    assert TemporalSentence.verified(mutated) == :not_applicable

    assert TemporalSentence.build(%{si | source: :tool}) == nil
  end
end
