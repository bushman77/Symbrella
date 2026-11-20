defmodule Core.Syntax.ChartClauseRulesTest do
  @moduledoc """
  P-039: harness-only `:clause_v1` rule set. Pure fixture nodes (no DB):
  each node carries dictionary-attested POS and morphology exactly as
  `Core.Syntax.Signatures.chart_nodes/1` would build them.
  """
  use ExUnit.Case, async: true

  alias Core.Syntax.Chart

  @harness [rule_set: :clause_v1, harness_override: true]

  # word => {POS list, node extras}
  @lexicon %{
    "the" => {["determiner"], %{}},
    "a" => {["determiner"], %{}},
    "dog" => {["noun", "verb"], %{base_present_verb?: true}},
    "dogs" => {["noun"], %{plural_noun?: true}},
    "cat" => {["noun"], %{}},
    "garden" => {["noun"], %{}},
    "it" => {["pronoun"], %{}},
    "he" => {["pronoun"], %{}},
    "small" => {["adjective"], %{}},
    "big" => {["adjective"], %{}},
    "chased" => {["verb", "adjective"], %{morphology: [:simple_past, :past_participle]}},
    "chases" => {["verb", "noun"], %{finite_present_verb?: true}},
    "chase" => {["verb", "noun"], %{base_present_verb?: true}},
    "chasing" => {["verb", "noun"], %{morphology: [:present_participle]}},
    "is" => {["auxiliary", "verb"], %{finite_present_verb?: true}},
    "are" => {["auxiliary", "verb"], %{}},
    "was" => {["auxiliary", "verb"], %{morphology: [:simple_past]}},
    "did" => {["verb", "auxiliary"], %{morphology: [:simple_past]}},
    "does" => {["verb", "auxiliary"], %{finite_present_verb?: true}},
    "do" => {["verb", "auxiliary"], %{base_present_verb?: true}},
    "not" => {["adverb", "particle"], %{}},
    "never" => {["adverb"], %{}},
    "nobody" => {["pronoun", "noun"], %{}},
    "could" => {["verb"], %{}},
    "by" => {["preposition"], %{}},
    "in" => {["preposition"], %{}},
    "tired" => {["adjective"], %{}}
  }

  defp node(index, word) do
    {pos, extra} = Map.fetch!(@lexicon, word)

    %{
      index: index,
      word: word,
      surface: word,
      pos: MapSet.new(pos),
      morphology: MapSet.new(Map.get(extra, :morphology, [])),
      finite_present_verb?: Map.get(extra, :finite_present_verb?, false),
      plural_noun?: Map.get(extra, :plural_noun?, false),
      base_present_verb?: Map.get(extra, :base_present_verb?, false)
    }
  end

  defp setup_input(sentence, source \\ :user) do
    words =
      sentence
      |> String.replace(~r/[.?]\z/, "")
      |> String.downcase()
      |> String.split()

    nodes = words |> Enum.with_index() |> Enum.map(fn {w, i} -> node(i, w) end)
    tokens = Enum.map(nodes, &%{index: &1.index, phrase: &1.surface})
    {%{sentence: sentence, source: source, tokens: tokens}, nodes}
  end

  defp clauses(sentence, opts \\ @harness) do
    {si, nodes} = setup_input(sentence)
    result = Chart.diagnose(si, nodes, [], opts)
    {result, Enum.filter(result.signatures, &(&1.id == :finite_clause))}
  end

  defp features(sig), do: Map.take(sig.clause, [:tense, :aspect, :polarity, :voice, :agent])

  test "active simple past: one full-span clause with object roles" do
    {_, [sig]} = clauses("The dog chased the cat.")

    assert sig.span == {0, 4}
    assert sig.roles == %{subject_determiner: 0, subject_head: 1, predicate: 2,
                          object_determiner: 3, object_head: 4}
    assert features(sig) == %{tense: :past, aspect: :simple, polarity: :positive,
                              voice: :active, agent: :not_applicable}
    assert sig.source == :core_syntax
    assert sig.factual_authority? == false
    assert :clause_rules_v1_harness_only in sig.evidence
  end

  test "present third-singular, progressive and do-support negation" do
    {_, [present]} = clauses("The dog chases the cat.")
    assert %{tense: :present, aspect: :simple, polarity: :positive} = features(present)

    {_, [progressive]} = clauses("The dog is chasing the cat.")
    assert %{tense: :present, aspect: :progressive, voice: :active} = features(progressive)
    assert progressive.roles.auxiliary == 2 and progressive.roles.predicate == 3

    {_, [negated]} = clauses("The dog did not chase the cat.")
    assert %{tense: :past, polarity: :negative, voice: :active} = features(negated)
    assert negated.roles.auxiliary == 2 and negated.roles.negation == 3
    assert negated.roles.predicate == 4 and negated.roles.object_head == 6

    {_, [does_not]} = clauses("The dog does not chase the cat.")
    assert %{tense: :present, polarity: :negative} = features(does_not)

    {_, [do_not]} = clauses("The dogs do not chase the cat.")
    assert %{tense: :present, polarity: :negative} = features(do_not)
  end

  test "passive with a by-agent exposes the agentless+adjunct alternative too" do
    {_, sigs} = clauses("The cat was chased by the dog.")
    by_agent = Enum.filter(sigs, &(&1.clause.agent == :expressed))
    adjunct = Enum.filter(sigs, &(&1.clause.agent == :unexpressed))

    assert [%{roles: %{agent_preposition: 4, agent_head: 6, subject_head: 1, predicate: 3}}] =
             by_agent

    assert [%{roles: %{predicate_adjunct_1_preposition: 4}}] = adjunct
    assert Enum.all?(sigs, &(&1.clause.voice == :passive))
  end

  test "agentless passive" do
    {_, [sig]} = clauses("The cat was chased.")
    assert %{voice: :passive, agent: :unexpressed, tense: :past} = features(sig)
  end

  test "NP-internal modifiers on subject and object reuse the nominal rules" do
    {_, [sig]} = clauses("The big dog chased a small cat.")

    assert sig.roles == %{subject_determiner: 0, subject_modifier_1: 1, subject_head: 2,
                          predicate: 3, object_determiner: 4, object_modifier_1: 5,
                          object_head: 6}
  end

  test "PP after the object yields BOTH attachments; none is chosen" do
    {_, sigs} = clauses("The dog chased the cat in the garden.")
    sites = sigs |> Enum.flat_map(& &1.clause.attachments) |> Enum.map(& &1.attached_to)

    assert length(sigs) == 2
    assert Enum.sort(sites) == [:object, :predicate]
  end

  test "PP inside the subject attaches to the subject only" do
    {_, [sig]} = clauses("The dog in the garden chased the cat.")
    assert [%{attached_to: :subject, preposition: 2, object: 4}] = sig.clause.attachments
    assert sig.roles.subject_head == 1 and sig.roles.object_head == 7
  end

  test "pronoun subject and pronoun object" do
    {_, [sig]} = clauses("He chased it.")
    assert sig.roles == %{subject_head: 0, predicate: 1, object_head: 2}
  end

  test "agreement conflict abstains (no finite clause)" do
    {_, []} = clauses("The dogs chases the cat.")
    {_, []} = clauses("The dog do not chase the cat.")
    {_, []} = clauses("The dogs is chasing the cat.")
  end

  test "BE never takes a direct object" do
    {_, sigs} = clauses("The dog is the cat.")
    assert sigs == []
  end

  test "questions, modals and non-'do not' negation abstain" do
    for sentence <- [
          "Did the dog chase the cat?",
          "Is the dog chasing the cat?",
          "The dog could chase the cat.",
          "Nobody chased the cat.",
          "The dog never chased the cat."
        ] do
      {result, sigs} = clauses(sentence)
      assert sigs == [], sentence
      assert result.status == :abstained, sentence
    end
  end

  test "budget overflow abstains instead of truncating" do
    {si, nodes} = setup_input("The dog chased the cat.")
    result = Chart.diagnose(si, nodes, [], @harness ++ [per_span: 1])
    assert result.status == :abstained
    assert result.reason in [:chart_budget_exceeded, :lexical_ambiguity_budget]
    assert result.signatures == []
  end

  test "without the explicit override the default chart is used unchanged" do
    for sentence <- ["The dog chased the cat.", "The dog is tired.", "The cat was chased."],
        source <- [:user, :colony_external] do
      {si, nodes} = setup_input(sentence, source)
      default = Chart.diagnose(si, nodes)

      assert Chart.diagnose(si, nodes, [], rule_set: :clause_v1) == default
      assert Chart.diagnose(si, nodes, [], harness_override: true) == default
      assert Chart.diagnose(si, nodes, [], rule_set: :other, harness_override: true) == default
      refute Enum.any?(default.signatures, &(&1.id == :finite_clause))
    end
  end

  test "the default path still rejects non-Colony sources and negation" do
    {si, nodes} = setup_input("The dog chased the cat.")
    assert %{status: :abstained, reason: :unsupported_source} = Chart.diagnose(si, nodes)

    {si, nodes} = setup_input("The dog did not chase the cat.", :colony_external)
    assert %{status: :abstained, reason: :negation} = Chart.diagnose(si, nodes)
  end
end
