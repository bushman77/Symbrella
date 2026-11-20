defmodule Core.Semantic.ClausePropositionTest do
  @moduledoc "P-039: pure mapping from `:finite_clause` signatures to structural hypotheses."
  use ExUnit.Case, async: true

  alias Core.Semantic.ClauseProposition

  defp tokens(sentence) do
    sentence
    |> String.replace(~r/[.?]\z/, "")
    |> String.split()
    |> Enum.with_index()
    |> Enum.map(fn {w, i} -> %{index: i, phrase: w, n: 1} end)
  end

  defp choice(i, id), do: %{token_index: i, id: id, chosen_id: id}

  defp past_cell(word, base),
    do: %{id: "#{word}|verb|1", norm: word, pos: "verb",
          semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"],
          definition: "simple past and past participle of #{base}"}

  defp base_cell(word), do: %{id: "#{word}|verb|1", norm: word, pos: "verb", semantic_atoms: []}

  defp active_si(sentence, extra \\ %{}) do
    Map.merge(
      %{sentence: sentence, tokens: tokens(sentence),
        lifg_choices: [choice(2, "chased|verb|1")],
        active_cells: [past_cell("chased", "chase")]},
      extra
    )
  end

  defp active_sig do
    %{id: :finite_clause, span: {0, 4},
      roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
               object_determiner: 3, object_head: 4},
      clause: %{tense: :past, aspect: :simple, polarity: :positive, voice: :active,
                agent: :not_applicable, attachments: []}}
  end

  test "active clause: subject is agent, object is patient, lemma from attested form-of" do
    {:ok, p} = ClauseProposition.build(active_si("The dog chased the cat."), active_sig())

    assert p.semantic_roles.agent.surface == "the dog"
    assert p.semantic_roles.patient.surface == "the cat"
    assert p.syntactic_roles.subject.head_surface == "dog"
    assert p.predicate == %{surface: "chased", token_index: 2, lemma: "chase",
                            lemma_basis: :attested_past_form_of}
    assert %{tense: :past, aspect: :simple, polarity: :positive, voice: :active} = p
    assert p.epistemic_status == :structural_hypothesis_not_verified
    assert p.factual_authority? == false
    assert p.semantic_role_basis == :voice_mapping_hypothesis
  end

  test "reversed twin swaps agent and patient with identical role indices" do
    {:ok, a} = ClauseProposition.build(active_si("The dog chased the cat."), active_sig())
    {:ok, b} = ClauseProposition.build(active_si("The cat chased the dog."), active_sig())

    assert {a.semantic_roles.agent.surface, a.semantic_roles.patient.surface} ==
             {"the dog", "the cat"}

    assert {b.semantic_roles.agent.surface, b.semantic_roles.patient.surface} ==
             {"the cat", "the dog"}
  end

  test "passive: subject is patient; by-object is agent; agentless is :unexpressed" do
    sentence = "The cat was chased by the dog."
    si = active_si(sentence, %{lifg_choices: [choice(3, "chased|verb|1")]})

    sig = %{id: :finite_clause, span: {0, 6},
            roles: %{subject_determiner: 0, subject_head: 1, auxiliary: 2, predicate: 3,
                     agent_preposition: 4, agent_head: 6},
            clause: %{tense: :past, aspect: :simple, polarity: :positive, voice: :passive,
                      agent: :expressed, attachments: []}}

    {:ok, p} = ClauseProposition.build(si, sig)
    assert p.semantic_roles.patient.surface == "the cat"
    assert p.semantic_roles.agent.surface == "the dog"
    assert p.syntactic_roles.by_object.span == {5, 6}
    assert p.syntactic_roles.object == nil
    assert p.predicate.lemma == "chase"

    agentless = %{sig | span: {0, 3},
                        roles: Map.drop(sig.roles, [:agent_preposition, :agent_head]),
                        clause: %{sig.clause | agent: :unexpressed}}

    {:ok, q} = ClauseProposition.build(%{si | sentence: "The cat was chased.",
                                             tokens: tokens("The cat was chased.")}, agentless)
    assert q.semantic_roles.agent == :unexpressed
    assert q.semantic_roles.patient.surface == "the cat"
  end

  test "negated base verb takes its lemma from a non-inflected attested row" do
    sentence = "The dog did not chase the cat."

    si = %{sentence: sentence, tokens: tokens(sentence),
           lifg_choices: [choice(4, "chase|verb|10")],
           active_cells: [%{base_cell("chase") | id: "chase|verb|10"}]}

    sig = %{id: :finite_clause, span: {0, 6},
            roles: %{subject_determiner: 0, subject_head: 1, auxiliary: 2, negation: 3,
                     predicate: 4, object_determiner: 5, object_head: 6},
            clause: %{tense: :past, aspect: :simple, polarity: :negative, voice: :active,
                      agent: :not_applicable, attachments: []}}

    {:ok, p} = ClauseProposition.build(si, sig)
    assert p.polarity == :negative
    assert p.predicate.lemma == "chase" and p.predicate.lemma_basis == :base_form
    refute p.assertion_admissible_svo_v1
  end

  test "lemma is unresolved without a selected verb witness or for progressives" do
    si = active_si("The dog chased the cat.", %{lifg_choices: [choice(2, "chased|adjective|1")]})
    {:ok, p} = ClauseProposition.build(si, active_sig())
    assert p.predicate.lemma == nil and p.predicate.lemma_basis == :no_selected_verb_witness

    si = active_si("The dog chased the cat.", %{active_cells: []})
    {:ok, p} = ClauseProposition.build(si, active_sig())
    assert p.predicate.lemma_basis == :no_selected_verb_witness

    progressive = put_in(active_sig(), [:clause, :aspect], :progressive)
    {:ok, p} = ClauseProposition.build(active_si("The dog chased the cat."), progressive)
    assert p.predicate.lemma == nil and p.predicate.lemma_basis == :unresolved
  end

  test "malformed input is :error, never a raise" do
    assert :error = ClauseProposition.build(nil, active_sig())
    assert :error = ClauseProposition.build(%{}, %{id: :copular_predication})
    assert :error = ClauseProposition.build(%{tokens: []}, active_sig())
    assert :error = ClauseProposition.build(active_si("x"), %{active_sig() | roles: %{predicate: 2}})
  end
end
