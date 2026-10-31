defmodule Core.Semantic.CompositionFinitePastLocativeHardeningTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{FinitePastLocative, FinitePastLocativeIntegration}
  alias Core.SemanticInput
  alias Core.Syntax.Signatures
  alias Core.TokenFilters

  # Fixtures intentionally simulate current-turn scored dictionary evidence;
  # none of these tests relies on mutable PostgreSQL or global LIFG state.
  defp input(surface, lemma) do
    words = ["the", "bird", surface, "on", "the", "fence"]
    selected_pos = if surface == "sat", do: "noun", else: "verb"
    selected_id = "#{surface}|#{selected_pos}|1"
    form_id = "#{surface}|verb|1"

    cells =
      words
      |> Enum.with_index()
      |> Enum.flat_map(fn {word, i} ->
        case i do
          2 ->
            form = %{
              id: form_id, norm: word, pos: "verb", token_index: i,
              definition: "(form-of, participle, past) simple past and past participle of #{lemma}",
              semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]
            }

            if surface == "sat" do
              [%{id: selected_id, norm: word, pos: "noun",
                 token_index: i, semantic_atoms: []}, form]
            else
              [form]
            end

          _ ->
            pos =
              case i do
                n when n in [0, 4] -> "determiner"
                n when n in [1, 5] -> "noun"
                3 -> "preposition"
              end

            [%{id: "#{word}|#{pos}|1", norm: word, pos: pos,
               token_index: i, semantic_atoms: []}]
        end
      end)

    choices =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, i} ->
        if i == 2 do
          scores = if surface == "sat",
            do: %{selected_id => 0.8, form_id => 0.2},
            else: %{form_id => 1.0}

          %{id: selected_id, token_index: i, candidate_probs: scores}
        else
          pos =
            case i do
              n when n in [0, 4] -> "determiner"
              n when n in [1, 5] -> "noun"
              3 -> "preposition"
            end

          id = "#{word}|#{pos}|1"
          %{id: id, token_index: i, candidate_probs: %{id => 1.0}}
        end
      end)

    %SemanticInput{
      sentence: Enum.join(words, " ") <> ".",
      active_cells: cells
    }
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Map.put(:lifg_choices, choices)
    |> Signatures.attach()
  end

  defp rows("sit") do
    [
      %{id: "sit|verb|10", norm: "sit", pos: "verb",
        definition: "To be seated with the upper body is upright."},
      %{id: "sit|verb|2", norm: "sit", pos: "verb",
        definition: "To serve on a committee."}
    ]
  end

  defp rows("sleep") do
    [
      %{id: "sleep|verb|5", norm: "sleep", pos: "verb",
        definition: "To be dead."},
      %{id: "sleep|verb|1", norm: "sleep", pos: "verb",
        definition: "To rest in a state of reduced consciousness."},
      %{id: "sleep|verb|2", norm: "sleep", pos: "verb",
        definition: "To have sexual intercourse."}
    ]
  end

  defp lookup(lemma), do: rows(lemma)

  test "supported lemma policy is single-source and closed" do
    assert FinitePastLocative.supported_lemma?("sit")
    assert FinitePastLocative.supported_lemma?("sleep")
    refute FinitePastLocative.supported_lemma?("fly")
    refute FinitePastLocative.supported_lemma?("walk")
    refute FinitePastLocative.supported_lemma?(nil)
  end

  test "both predicates compose, replay, and retain only conditional authority" do
    for {surface, lemma, expected_selected_id} <- [
          {"sat", "sit", "sat|noun|1"},
          {"slept", "sleep", "slept|verb|1"}
        ] do
      original = input(surface, lemma)
      assert Enum.any?(original.sentence_signatures,
        &(&1.id == :finite_past_locative_candidate))

      assert {:ok, proof} = FinitePastLocativeIntegration.derive(original, lookup: &lookup/1)
      assert :ok = FinitePastLocative.verify(proof)
      assert proof.event_semantics.predicate == String.to_existing_atom(lemma)
      assert proof.event_semantics.tense == :past
      assert proof.event_semantics.event_existence == :not_asserted
      assert proof.event_semantics.actor.referent == :unresolved
      refute proof.factual_authority?
      refute proof.semantic_compatibility_proven?
      assert proof.premises.selected_lifg_id == expected_selected_id

      attached = FinitePastLocativeIntegration.attach(original, lookup: &lookup/1)
      assert attached.finite_past_locative_derivation == proof
      assert attached.lifg_choices == original.lifg_choices
      assert attached.active_cells == original.active_cells
      assert attached.compositional_derivation == original.compositional_derivation
      assert :ok = SemanticInput.validate_contract(attached)
    end
  end

  test "unapproved verb never invokes a canonical lemma dictionary lookup" do
    si = input("flew", "fly")
    assert Enum.any?(si.sentence_signatures,
      &(&1.id == :finite_past_locative_candidate))
    refute FinitePastLocative.supported_lemma?("fly")
    result = FinitePastLocativeIntegration.derive(si,
      lookup: fn lemma -> send(self(), {:unexpected_lookup, lemma}); rows("sleep") end)
    assert result == :not_applicable
    refute_received {:unexpected_lookup, _}
  end

  test "conflicting scored form-of lemmas force abstention before lookup" do
    original = input("sat", "sit")
    second_form = %{
      id: "sat|verb|2", norm: "sat", pos: "verb", token_index: 2,
      definition: "simple past and past participle of sleep",
      semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]
    }

    conflicted = %{original |
      active_cells: original.active_cells ++ [second_form],
      lifg_choices: Enum.map(original.lifg_choices, fn
        %{token_index: 2} = choice ->
          %{choice | candidate_probs: Map.put(choice.candidate_probs, "sat|verb|2", 0.1)}
        choice -> choice
      end)
    } |> Signatures.attach()

    assert :not_applicable = FinitePastLocativeIntegration.derive(conflicted,
      lookup: fn lemma -> send(self(), {:unexpected_lookup, lemma}); [] end)
    refute_received {:unexpected_lookup, _}
  end

  test "form-of tag, scored candidate, or exact morphology cannot be fabricated" do
    for {surface, lemma} <- [{"sat", "sit"}, {"slept", "sleep"}] do
      original = input(surface, lemma)
      form_id = "#{surface}|verb|1"
      changed_tag = %{original | active_cells:
        Enum.map(original.active_cells, fn
          %{id: id} = cell when id == form_id ->
            %{cell | semantic_atoms: ["tag:past", "tag:participle"]}
          cell -> cell
        end)} |> Signatures.attach()
      assert :not_applicable = FinitePastLocativeIntegration.derive(changed_tag,
        lookup: &lookup/1)

      changed_score = %{original | lifg_choices:
        Enum.map(original.lifg_choices, fn
          %{token_index: 2} = choice ->
            %{choice | candidate_probs: Map.delete(choice.candidate_probs,
              "#{surface}|verb|1")}
          choice -> choice
        end)}
      assert :not_applicable = FinitePastLocativeIntegration.derive(changed_score,
        lookup: &lookup/1)

      participle_only = %{original | active_cells:
        Enum.map(original.active_cells, fn
          %{id: id} = cell when id == form_id ->
            %{cell | definition: "past participle of #{lemma}"}
          cell -> cell
        end)} |> Signatures.attach()
      assert :not_applicable = FinitePastLocativeIntegration.derive(participle_only,
        lookup: &lookup/1)
    end
  end

  test "wrong lemma rows, irrelevant senses and unavailable database fail closed" do
    for {surface, lemma} <- [{"sat", "sit"}, {"slept", "sleep"}] do
      original = input(surface, lemma)
      assert :not_applicable = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> [] end)
      assert :not_applicable = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> nil end)
      assert :not_applicable = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> raise "database unavailable" end)
      assert :not_applicable = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> throw(:not_available) end)
      assert :not_applicable = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> List.duplicate(hd(rows(lemma)), 129) end)

      irrelevant = [
        %{id: "#{lemma}|noun|1", norm: lemma, pos: "noun",
          definition: "To rest in a state of reduced consciousness."},
        %{id: "unrelated|verb|1", norm: "unrelated", pos: "verb",
          definition: "To rest in a state of reduced consciousness."},
        %{id: "#{lemma}|verb|999", norm: lemma, pos: "verb",
          definition: "To be unrelated to the intended interpretation."}
      ]
      assert :not_applicable = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> irrelevant end)
    end
  end

  test "missing syntax, conflicting surface, and extra tokens do not receive proof" do
    original = input("slept", "sleep")

    missing_signature = %{original | sentence_signatures: []}
    assert :not_applicable = FinitePastLocativeIntegration.derive(missing_signature,
      lookup: &lookup/1)

    mismatched_sentence = %{original | sentence: "The bird sat on the fence."}
    assert :not_applicable = FinitePastLocativeIntegration.derive(mismatched_sentence,
      lookup: &lookup/1)

    extra_token = %{original | tokens: original.tokens ++ [
      %{phrase: "yesterday", index: 6, n: 1}
    ]}
    assert :not_applicable = FinitePastLocativeIntegration.derive(extra_token,
      lookup: &lookup/1)

    no_role_witness = %{original | lifg_choices:
      Enum.reject(original.lifg_choices, &(&1.token_index == 1))}
    assert :not_applicable = FinitePastLocativeIntegration.derive(no_role_witness,
      lookup: &lookup/1)
  end

  test "opt-out remains byte-for-byte unchanged and cannot trigger a lookup" do
    for {surface, lemma} <- [{"sat", "sit"}, {"slept", "sleep"}] do
      original = input(surface, lemma)
      disabled = FinitePastLocativeIntegration.attach(original,
        finite_past_locative?: false,
        lookup: fn _ -> flunk("lookup performed while disabled") end)
      assert disabled == original
    end
  end

  test "proof replay rejects semantic tampering, corrupted premises and malformed shapes" do
    for {surface, lemma} <- [{"sat", "sit"}, {"slept", "sleep"}] do
      assert {:ok, proof} = FinitePastLocativeIntegration.derive(
        input(surface, lemma), lookup: &lookup/1)

      corruptions = [
        Map.put(proof, :status, :asserted),
        Map.put(proof, :factual_authority?, true),
        Map.put(proof, :semantic_compatibility_proven?, true),
        Map.put(proof, :epistemic_status, :verified_truth),
        put_in(proof, [:event_semantics, :event_existence], :asserted),
        put_in(proof, [:event_semantics, :tense], :present),
        put_in(proof, [:event_semantics, :spatial, :arrival], :established),
        put_in(proof, [:event_semantics, :predicate], :unknown),
        Map.put(proof, :steps, []),
        Map.put(proof, :premises, nil),
        Map.put(proof, :premises, 42),
        put_in(proof, [:premises, :form_witness], 42),
        Map.put(proof, :event_semantics, nil),
        Map.put(proof, :event_semantics, 42),
        put_in(proof, [:event_semantics, :spatial], 42),
        put_in(proof, [:event_semantics, :actor], 42)
      ]

      for tampered <- corruptions do
        assert {:error, :invalid_derivation} = FinitePastLocative.verify(tampered)
      end
    end
  end

  test "lemma row reordering does not change which dictionary sense is used" do
    for {surface, lemma} <- [{"sat", "sit"}, {"slept", "sleep"}] do
      original = input(surface, lemma)
      assert {:ok, first} = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> rows(lemma) end)
      assert {:ok, reversed} = FinitePastLocativeIntegration.derive(original,
        lookup: fn ^lemma -> Enum.reverse(rows(lemma)) end)
      assert first == reversed
    end
  end
end
