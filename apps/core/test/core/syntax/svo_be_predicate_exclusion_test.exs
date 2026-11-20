defmodule Core.Syntax.SvoBePredicateExclusionTest do
  @moduledoc """
  P-024: the closed BE paradigm never fills a transitive SVO predicate slot.

  Each BE negative has a counterfactual twin in which the BE token is replaced
  by a nonce verb carrying identical dictionary rows. The twin must still be
  detected, which proves the fixtures are sufficient and that the BE exclusion,
  not some other missing witness, removes the false reading.
  """
  use ExUnit.Case, async: true

  alias Brain.LIFG.{GrammarEvidence, StructuralCompetition}
  alias Core.SemanticInput
  alias Core.Syntax.{PredicateEvidence, RoleCompetition, Signatures}

  @svo_ids [
    :finite_present_svo_candidate,
    :finite_past_svo_candidate,
    :counted_finite_present_svo_candidate,
    :bounded_regular_past_transitive
  ]

  @present3 ["tag:form-of", "tag:indicative", "tag:present", "tag:singular", "tag:third-person"]
  @past ["tag:form-of", "tag:indicative", "tag:past"]
  @plural ["tag:form-of", "tag:plural"]

  defp input(words, cells) do
    {tokens, _end} =
      words
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {word, index}, start ->
        stop = start + byte_size(word)

        {%{index: index, phrase: word, kind: :word, n: 1, mw: false, span: {start, stop}},
         stop + 1}
      end)

    %SemanticInput{sentence: Enum.join(words, " "), tokens: tokens, active_cells: cells}
  end

  defp cell(word, pos, atoms \\ [], n \\ 1) do
    %{id: "#{word}|#{pos}|#{n}", norm: word, pos: pos, semantic_atoms: atoms, activation: 0.5}
  end

  defp base_cells do
    [
      cell("the", "determiner"),
      cell("a", "determiner"),
      cell("some", "determiner"),
      cell("it", "pronoun"),
      cell("he", "pronoun"),
      cell("report", "noun"),
      cell("reports", "noun", @plural),
      cell("draft", "noun"),
      cell("problem", "noun"),
      cell("ready", "adjective"),
      cell("ready", "noun"),
      cell("small", "adjective"),
      cell("small", "noun"),
      cell("fly", "noun"),
      cell("fly", "verb"),
      cell("artist", "noun"),
      cell("artists", "noun", @plural),
      cell("picture", "noun"),
      cell("pictures", "noun"),
      cell("robot", "noun"),
      cell("work", "noun"),
      cell("bread", "noun")
    ]
  end

  # BE rows mirror the live LTM shapes observed in the D-1 receipt (§10.9):
  # an auxiliary row plus verb rows with full finite morphology, and a plain
  # (non-form-of) verb row for "are"/"were".
  defp be_cells do
    [
      cell("is", "auxiliary"),
      cell("is", "verb", @present3),
      cell("was", "auxiliary"),
      cell("was", "verb", @past),
      cell("were", "auxiliary"),
      cell("were", "verb", @past),
      cell("were", "verb", [], 2),
      cell("are", "auxiliary"),
      cell("are", "verb")
    ]
  end

  defp verb_cells do
    [
      cell("draws", "noun", @plural),
      cell("draws", "verb", @present3),
      cell("has", "auxiliary"),
      cell("has", "verb", @present3),
      cell("does", "auxiliary"),
      cell("does", "verb", @present3),
      cell("bought", "verb", @past),
      cell("drew", "verb", @past),
      cell("draw", "verb"),
      cell("draw", "noun")
    ]
  end

  defp cells, do: base_cells() ++ be_cells() ++ verb_cells()

  # Replace the BE token with a nonce verb that copies every BE verb row.
  defp counterfactual(words, index, nonce) do
    be = Enum.at(words, index)

    twin_rows =
      be_cells()
      |> Enum.filter(&(&1.norm == be and &1.pos == "verb"))
      |> Enum.with_index(1)
      |> Enum.map(fn {row, n} -> cell(nonce, "verb", row.semantic_atoms, n) end)

    {List.replace_at(words, index, nonce), base_cells() ++ twin_rows}
  end

  defp svo_ids(words, cells) do
    words
    |> input(cells)
    |> Signatures.detect()
    |> Enum.map(& &1.id)
    |> Enum.filter(&(&1 in @svo_ids))
  end

  defp attach(words, cells) do
    words |> input(cells) |> Signatures.attach() |> RoleCompetition.attach()
  end

  # {words, BE index, nonce verb for the counterfactual twin}
  @be_cases [
    {~w(The report is ready), 2, "blorps"},
    {~w(The fly is small), 2, "blorps"},
    {~w(The report is a draft), 2, "blorps"},
    {~w(The report was the problem), 2, "blorped"},
    {~w(It was the problem), 1, "blorped"},
    {~w(The reports were the problem), 2, "blorped"}
  ]

  test "BE never fills an SVO predicate slot, while an identical non-BE verb does" do
    for {words, index, nonce} <- @be_cases do
      assert svo_ids(words, cells()) == [], "unexpected SVO for #{inspect(words)}"

      {twin, twin_cells} = counterfactual(words, index, nonce)
      assert svo_ids(twin, twin_cells) != [], "fixture insufficient for #{inspect(twin)}"
    end
  end

  # Shapes for which PredicateEvidence derives a predicate from a non-BE verb.
  # (The 5-token D-N-V-D-N past frame yields none, consistent with D-1.)
  @proposal_cases [
    {~w(The report is ready), 2, "blorps"},
    {~w(The fly is small), 2, "blorps"},
    {~w(The report is a draft), 2, "blorps"},
    {~w(It was the problem), 1, "blorped"},
    {~w(The reports were the problem), 2, "blorped"},
    {~w(The reports are the problem), 2, "blorp"}
  ]

  test "no pre-LIFG verb-predicate support is proposed for a BE token" do
    for {words, index, _nonce} <- @be_cases ++ @proposal_cases do
      out = attach(words, cells())

      refute Enum.any?(out.role_competition.predicate_proposals, &(&1.token_index == index)),
             "predicate proposal at BE index for #{inspect(words)}"

      refute match?(%{pos: "verb"}, out.role_competition.roles[index]),
             "verb role at BE index for #{inspect(words)}"
    end

    for {words, index, nonce} <- @proposal_cases do
      {twin, twin_cells} = counterfactual(words, index, nonce)
      twin_out = attach(twin, twin_cells)

      assert Enum.any?(twin_out.role_competition.predicate_proposals, &(&1.token_index == index)),
             "fixture insufficient for #{inspect(twin)}"
    end
  end

  test "PredicateEvidence never proposes a plural-present BE predicate" do
    for be <- ["are", "were"] do
      si = input(["The", "reports", be, "the", "problem"], cells())
      refute Enum.any?(PredicateEvidence.detect(si), &(&1.token_index == 2))
    end

    control = input(~w(The artists draw pictures), cells())

    assert [%{token_index: 2, basis: :plural_present_predicate}] =
             PredicateEvidence.detect(control)
  end

  test "Stage1 grammar boosts leave BE senses unchanged but still support a real verb" do
    out = attach(~w(The report is a draft), cells())
    context = GrammarEvidence.prepare(out)
    scores = %{"is|verb|1" => 0.40, "is|auxiliary|1" => 0.60}

    boosted =
      scores
      |> GrammarEvidence.apply_finite_present(context, 2)
      |> GrammarEvidence.apply_role_competition(context, 2)

    assert boosted == scores

    control = attach(~w(The artist draws a picture), cells())
    control_context = GrammarEvidence.prepare(control)
    control_scores = %{"draws|verb|1" => 0.40, "draws|noun|1" => 0.57}

    control_boosted =
      control_scores
      |> GrammarEvidence.apply_finite_present(control_context, 2)
      |> GrammarEvidence.apply_role_competition(control_context, 2)

    assert control_boosted["draws|verb|1"] > control_scores["draws|verb|1"]
    assert control_boosted["draws|noun|1"] == control_scores["draws|noun|1"]
  end

  test "a BE sentence never reaches a preferred SVO structure even with correct LIFG choices" do
    choices = [
      %{token_index: 0, id: "the|determiner|1"},
      %{token_index: 1, id: "report|noun|1"},
      %{token_index: 2, id: "was|auxiliary|1"},
      %{token_index: 3, id: "the|determiner|1"},
      %{token_index: 4, id: "problem|noun|1"}
    ]

    result =
      ~w(The report was the problem)
      |> input(cells())
      |> Signatures.attach()
      |> Map.put(:lifg_choices, choices)
      |> StructuralCompetition.resolve()

    refute result.status == :preferred and result.preferred.construction in @svo_ids
  end

  test "genuine transitive verbs, including ones with auxiliary rows, keep their SVO readings" do
    assert :finite_present_svo_candidate in svo_ids(~w(The artist draws a picture), cells())
    assert :finite_present_svo_candidate in svo_ids(~w(The artist has a picture), cells())
    assert :finite_present_svo_candidate in svo_ids(~w(The robot does work), cells())
    assert :finite_past_svo_candidate in svo_ids(~w(He bought some bread), cells())
    assert :finite_past_svo_candidate in svo_ids(~w(The artist drew the picture), cells())
  end

  test "the real transitive control still reaches a preferred SVO structure" do
    choices = [
      %{token_index: 0, id: "the|determiner|1"},
      %{token_index: 1, id: "artist|noun|1"},
      %{token_index: 2, id: "draws|verb|1"},
      %{token_index: 3, id: "a|determiner|1"},
      %{token_index: 4, id: "picture|noun|1"}
    ]

    result =
      ~w(The artist draws a picture)
      |> input(cells())
      |> Signatures.attach()
      |> Map.put(:lifg_choices, choices)
      |> StructuralCompetition.resolve()

    assert %{status: :preferred, preferred: %{construction: :finite_present_svo_candidate}} = result
  end
end
