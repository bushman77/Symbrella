defmodule Core.Semantic.CompositionCrossConstructionRegressionTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition
  alias Core.Semantic.Composition.{FinitePastLocative, FinitePastLocativeIntegration, Proof}
  alias Core.Syntax.{Signatures, StructuralCompetition}
  alias Core.TokenFilters

  defp standard(words, positions, atoms \\ %{}) do
    cells =
      words
      |> Enum.zip(positions)
      |> Enum.with_index()
      |> Enum.map(fn {{word, pos}, i} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos, token_index: i,
          semantic_atoms: Map.get(atoms, i, [])}
      end)

    %Core.SemanticInput{
      sentence: Enum.join(words, " ") <> ".",
      tokens: Enum.with_index(words, fn phrase, index ->
        %{phrase: phrase, index: index, n: 1}
      end),
      active_cells: cells,
      lifg_choices: Enum.with_index(cells, fn cell, index ->
        %{id: cell.id, token_index: index}
      end)
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  defp attach(input, opts \\ []) do
    FinitePastLocativeIntegration.attach(input,
      Keyword.merge([finite_past_locative?: true, lookup: fn _ -> raise "unexpected lookup" end], opts))
  end

  test "nominal composition remains verified; past hook abstains without DB lookup" do
    before = standard(~w(The black bird), ~w(determiner adjective noun))
    assert %{kind: :nominal_property} = before.compositional_derivation
    assert {:ok, _} = Composition.verified(before)

    after_attachment = attach(before)
    assert after_attachment.compositional_derivation == before.compositional_derivation
    assert after_attachment.finite_past_locative_derivation == nil
    assert after_attachment.lifg_choices == before.lifg_choices
    assert {:ok, _} = Composition.verified(after_attachment)
    assert :ok = Core.SemanticInput.validate_contract(after_attachment)
  end

  test "progressive event proof remains verified; past hook abstains" do
    before = standard(
      ~w(The cat is sleeping under the table),
      ~w(determiner noun verb verb preposition determiner noun),
      %{3 => ["tag:form-of", "tag:present", "tag:participle"]}
    )
    assert %{kind: :progressive_event_property} = before.compositional_derivation
    assert {:ok, _} = Composition.verified(before)

    after_attachment = attach(before)
    assert after_attachment.compositional_derivation == before.compositional_derivation
    assert after_attachment.finite_past_locative_derivation == nil
    assert after_attachment.lifg_choices == before.lifg_choices
    assert {:ok, _} = Composition.verified(after_attachment)
    assert :ok = Core.SemanticInput.validate_contract(after_attachment)
  end

  test "past proof attaches separately while regular composition stays unchanged" do
    input = standard(
      ~w(The bird sat on the fence),
      ~w(determiner noun noun preposition determiner noun)
    )
    form = %{
      id: "sat|verb|1", norm: "sat", pos: "verb", token_index: 2,
      definition: "simple past and past participle of sit",
      semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]
    }
    choices = Enum.map(input.lifg_choices, fn
      %{token_index: 2} = choice ->
        Map.put(choice, :candidate_probs,
          %{"sat|noun|1" => 0.8, "sat|verb|1" => 0.2})
      choice -> Map.put(choice, :candidate_probs, %{choice.id => 1.0})
    end)

    past =
      %{input | active_cells: input.active_cells ++ [form], lifg_choices: choices}
      |> Signatures.attach()
      |> StructuralCompetition.attach()
      |> Composition.attach()

    baseline = past.compositional_derivation
    assert FinitePastLocativeIntegration.attach(past, finite_past_locative?: false) == past

    lookup = fn "sit" ->
      [%{id: "sit|verb|1", norm: "sit", pos: "verb",
        definition: "To be seated with the upper body is upright.",
        semantic_atoms: []}]
    end

    attached = FinitePastLocativeIntegration.attach(past, lookup: lookup)
    assert attached == FinitePastLocativeIntegration.attach(past,
      finite_past_locative?: true, lookup: lookup)

    assert %{kind: :finite_past_locative_property} = proof =
      attached.finite_past_locative_derivation
    assert :ok = FinitePastLocative.verify(proof)
    assert proof.event_semantics.tense == :past
    assert proof.event_semantics.event_existence == :not_asserted
    assert attached.compositional_derivation == baseline
    assert attached.lifg_choices == past.lifg_choices
    assert :ok = Core.SemanticInput.validate_contract(attached)
  end
end
