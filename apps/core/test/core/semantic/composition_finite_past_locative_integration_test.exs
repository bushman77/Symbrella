defmodule Core.Semantic.CompositionFinitePastLocativeIntegrationTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.FinitePastLocativeIntegration, as: Integration
  alias Core.Semantic.Composition.{FinitePastLocative, LemmaGroundedPredicate, RoleWitness}
  alias Core.SemanticInput
  alias Core.Syntax.Signatures
  alias Core.TokenFilters

  defp si do
    words = ~w(the bird sat on the fence)

    cells = Enum.flat_map(Enum.with_index(words), fn {word, i} ->
      pos = case i do
        0 -> "determiner"
        1 -> "noun"
        2 -> "noun"
        3 -> "preposition"
        4 -> "determiner"
        5 -> "noun"
      end

      [%{id: "#{word}|#{pos}|1", norm: word, pos: pos,
         token_index: i, semantic_atoms: []}]
    end) ++ [
      %{id: "sat|verb|1", norm: "sat", pos: "verb", token_index: 2,
        definition: "simple past and past participle of sit",
        semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]}
    ]

    choices = Enum.with_index(words) |> Enum.map(fn {word, i} ->
      pos = case i do
        0 -> "determiner"
        1 -> "noun"
        2 -> "noun"
        3 -> "preposition"
        4 -> "determiner"
        5 -> "noun"
      end

      id = "#{word}|#{pos}|1"
      %{id: id, token_index: i,
        candidate_probs: if(i == 2, do: %{id => 0.8, "sat|verb|1" => 0.2},
                            else: %{id => 1.0})}
    end)

    %SemanticInput{sentence: "The bird sat on the fence.", active_cells: cells}
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Map.put(:lifg_choices, choices)
    |> Signatures.attach()
  end

  defp lookup("sit") do
    [
      %{id: "sit|verb|1", norm: "sit", pos: "verb",
        definition: "To be seated with the upper body is upright.",
        semantic_atoms: []}
    ]
  end

  test "uses current input signatures, selected senses and dictionary evidence" do
    original = si()
    assert Enum.any?(original.sentence_signatures,
      &(&1.id == :finite_past_locative_candidate))
    for {role, index, pos} <- [
          {:subject_determiner, 0, "determiner"},
          {:subject_head, 1, "noun"},
          {:preposition, 3, "preposition"},
          {:location_determiner, 4, "determiner"},
          {:location_head, 5, "noun"}
        ] do
      assert {:ok, _} = RoleWitness.resolve(original, role, index, [pos]),
        "missing current-turn role witness: #{inspect(role)}"
    end

    assert %{status: :forms_attested, requested_lemmas: ["sit"]} = discovery =
             LemmaGroundedPredicate.discover(original, 2)

    assert %{status: :predicate_feature_candidates} = grounded =
             LemmaGroundedPredicate.ground(discovery, lookup("sit"))

    signature = Enum.find(original.sentence_signatures,
      &(&1.id == :finite_past_locative_candidate))

    assert {:ok, _} = FinitePastLocative.compose(%{
      signature: signature,
      grounding: grounded,
      predicate_surface: "sat",
      actor: "bird",
      preposition: "on",
      landmark: "fence"
    })

    base_tokens =
      original.tokens
      |> Enum.filter(&(Map.get(&1, :n, 1) == 1 and
                       Map.get(&1, :kind) not in [:punct, "punct"]))
      |> Enum.sort_by(& &1.index)

    assert Enum.map(base_tokens, & &1.index) == Enum.to_list(0..5)
    assert Enum.map(base_tokens, fn token ->
      token.phrase |> String.downcase() |> String.trim() |>
        String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    end) ==
             ~w(the bird sat on the fence)
    assert String.trim_trailing(String.downcase(String.trim(original.sentence)), ".") ==
             Enum.map_join(base_tokens, " ", fn token ->
               token.phrase |> String.downcase() |> String.trim() |>
                 String.replace(~r/^\p{P}+|\p{P}+$/u, "")
             end)
    assert signature in Signatures.detect(original)

    assert {:ok, proof} = Integration.derive(original, lookup: &lookup/1)
    assert proof.event_semantics.predicate == :sit
    assert proof.event_semantics.tense == :past
    assert proof.premises.selected_lifg_id == "sat|noun|1"
    assert proof.event_semantics.event_existence == :not_asserted
    refute proof.factual_authority?
    assert original.lifg_choices |> Enum.at(2) |> Map.fetch!(:id) == "sat|noun|1"
  end

  test "tied attested noun senses license a noun-kind role without choosing a sense" do
    original = si()
    cells = original.active_cells ++ [
      %{id: "bird|noun|2", norm: "bird", pos: "noun",
        token_index: 1, semantic_atoms: []}
    ]
    choices = Enum.map(original.lifg_choices, fn choice ->
      if choice.token_index == 1 do
        %{choice | id: "bird|noun|999",
          candidate_probs: %{"bird|noun|1" => 0.5, "bird|noun|2" => 0.5}}
      else
        choice
      end
    end)
    changed = %{original | active_cells: cells, lifg_choices: choices}
    assert :unsupported = RoleWitness.resolve(changed, :subject_head, 1, ["noun"])
    assert {:ok, witness} = RoleWitness.resolve_noun_kind(changed, :subject_head, 1)
    assert witness.sense_id == nil
    assert witness.selection_basis == :construction_noun_pos_only
    assert length(witness.attested_sense_ids) == 2
    assert {:ok, proof} = Integration.derive(changed, lookup: &lookup/1)
    assert proof.premises.selected_lifg_id == "sat|noun|1"

    unscored = %{changed |
      lifg_choices: Enum.map(changed.lifg_choices, fn choice ->
        if choice.token_index == 1, do: %{choice | candidate_probs: %{}}, else: choice
      end)
    }
    assert :unsupported = RoleWitness.resolve_noun_kind(unscored, :subject_head, 1)
    assert :not_applicable = Integration.derive(unscored, lookup: &lookup/1)
  end

  test "default-on attachment preserves existing derivations and explicit false opts out" do
    original = si()
    existing = %{kind: :progressive_event_property, marker: :preserve}
    input = %{original | compositional_derivation: existing}

    assert Integration.attach(input, finite_past_locative?: false) == input

    enabled = Integration.attach(input, lookup: &lookup/1)
    assert enabled == Integration.attach(input,
      finite_past_locative?: true, lookup: &lookup/1)

    assert enabled.compositional_derivation == existing
    assert enabled.lifg_choices == input.lifg_choices
    assert %{kind: :finite_past_locative_property} =
             enabled.finite_past_locative_derivation
    assert :ok =
             Core.Semantic.Composition.FinitePastLocative.verify(
               enabled.finite_past_locative_derivation
             )
  end

  test "enabled attachment abstains without a proof and leaves primary semantics untouched" do
    original = si()
    existing = %{kind: :nominal_property, marker: :preserve}
    input = %{original | compositional_derivation: existing,
                         sentence_signatures: []}

    result = Integration.attach(input,
      finite_past_locative?: true, lookup: &lookup/1)

    assert result.compositional_derivation == existing
    assert result.finite_past_locative_derivation == nil
    assert result.lifg_choices == input.lifg_choices
  end

  test "rejects missing signatures and scored morphological evidence" do
    original = si()
    assert :not_applicable = Integration.derive(
      Map.put(original, :sentence_signatures, []), lookup: &lookup/1)
    choices = Enum.map(original.lifg_choices, fn c ->
      if c.token_index == 2, do: %{c | candidate_probs: %{"sat|noun|1" => 1.0}},
        else: c
    end)
    assert :not_applicable = Integration.derive(
      Map.put(original, :lifg_choices, choices), lookup: &lookup/1)
  end

  test "rejects absent lemma definitions and does not consult a failing lookup again" do
    original = si()
    assert :not_applicable = Integration.derive(original, lookup: fn "sit" -> [] end)
    assert :not_applicable = Integration.derive(original,
      lookup: fn "sit" -> raise "unavailable" end)
  end

  test "rejects ungrounded nominal role choices and extra tokens" do
    original = si()
    assert :not_applicable = Integration.derive(
      Map.put(original, :lifg_choices,
        Enum.reject(original.lifg_choices, &(&1.token_index == 1))),
      lookup: &lookup/1)
    assert :not_applicable = Integration.derive(
      Map.put(original, :tokens, original.tokens ++
        [%{phrase: "yesterday", index: 6, n: 1}]), lookup: &lookup/1)
  end
end
