defmodule Core.Response.SymbolicAnaphoricSpatialTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Discourse.References
  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{Composition, DiscourseComposition, SentenceMeaning}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  @features %{risk_bucket: :low, intent: :unknown}
  @decision %{action: :answer}

  setup do
    old = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if old == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, old)
      end
    end)

    :ok
  end

  defp session, do: "anaphoric-response-#{System.unique_integer([:positive])}"

  defp input(words, positions, session) do
    rows =
      Enum.zip(words, positions)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        atoms =
          if norm in ~w(sitting sleeping flying) and pos == "verb",
            do: ["tag:form-of", "tag:present", "tag:participle"],
            else: []

        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos, semantic_atoms: atoms}
      end)

    %Core.SemanticInput{
      session_id: session,
      sentence: Enum.join(words, " ") <> ".",
      source: :test,
      intent: :unknown,
      tokens:
        words |> Enum.with_index()
        |> Enum.map(fn {word, index} -> %{phrase: word, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices:
        rows |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{token_index: index, id: row.id} end)
    }
  end

  defp run(first_words, first_pos, second_words, second_pos) do
    sid = session()

    first =
      input(first_words, first_pos, sid)
      |> Signatures.attach()
      |> StructuralCompetition.attach()
      |> Composition.attach()

    assert {:ok, _} = Composition.verified(first)

    context = WMComprehension.context_from(first, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })

    second =
      input(second_words, second_pos, sid)
      |> Map.put(:wm_context, context)
      |> References.attach()
      |> Signatures.attach()
      |> StructuralCompetition.attach()
      |> SentenceMeaning.attach()

    assert {:ok, _} = DiscourseComposition.verified(second)
    second
  end

  defp bird do
    run(
      ~w(The small black bird),
      ~w(determiner adjective adjective noun),
      ~w(It is sitting on the fence),
      ~w(pronoun verb verb preposition determiner noun)
    )
  end

  test "revalidated cross-turn meaning becomes a cautious native conversation reply" do
    si = bird()
    assert %{operation: :clarify_anaphoric_spatial_report,
             status: :eligible, realizer: :symbolic_generation,
             source: :revalidated_cross_turn_composition} =
      plan = SemanticStrategy.select(si)

    assert plan.missing == [:referent_identity, :event_occurrence,
      :exact_dictionary_senses, :typed_cross_turn_join]

    assert {:ok, response, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, si.sentence, @features, @decision, si)

    assert response ==
      "You're describing the small black bird sitting on the fence. " <>
      "Is that the bird you mean?"

    assert meta.strategy_operation == :clarify_anaphoric_spatial_report
    assert meta.nominal_proof_verified?
    assert meta.event_proof_verified?
    assert meta.typed_cross_turn_join_verified? == false
    assert meta.referent_verified? == false
    assert meta.event_occurrence_verified? == false
    assert meta.exact_dictionary_sense_verified? == false
    assert meta.factual_claims? == false
    assert meta.llm_called? == false
  end

  test "the production response planner preserves the cross-turn proof and speaks natively" do
    si = bird()
    projected = Core.Response.Attach.maybe_build_response_plan(si, [])

    assert projected.discourse_reference == si.discourse_reference
    assert projected.sentence_meaning == si.sentence_meaning
    assert projected.response_text ==
      "You're describing the small black bird sitting on the fence. " <>
      "Is that the bird you mean?"
    assert projected.response_meta.response_source == :symbolic_generation
    assert projected.response_meta.response_reflection.llm_called? == false

    reflection = projected.response_meta.response_reflection
    assert reflection.nominal_proof_verified? == true
    assert reflection.event_proof_verified? == true
    assert reflection.typed_cross_turn_join_verified? == false
    assert reflection.referent_verified? == false
    assert reflection.event_occurrence_verified? == false
    assert reflection.exact_dictionary_sense_verified? == false
    assert reflection.factual_claims? == false
    assert projected.response_meta.semantic_response_strategy.operation ==
      :clarify_anaphoric_spatial_report
  end

  test "response composes nouns, modifiers, actions, and landmarks instead of parroting" do
    si = run(
      ~w(The white small cat),
      ~w(determiner adjective adjective noun),
      ~w(It is sleeping under the table),
      ~w(pronoun verb verb preposition determiner noun)
    )

    assert {:ok, response, meta} =
      SymbolicGeneration.generate(si.sentence, @features, @decision, si)
    assert response ==
      "You're describing the white small cat sleeping under the table. " <>
      "Is that the cat you mean?"
    assert meta.factual_claims? == false
  end

  test "stale discourse reference, wrong session or tampered semantics never speak" do
    si = bird()
    plan = SemanticStrategy.select(si)

    tampered = %{si | discourse_reference:
      %{si.discourse_reference | antecedent_head: "cat"}}
    assert SemanticStrategy.select(tampered) == :not_applicable
    assert SymbolicGeneration.generate(tampered.sentence, @features,
      @decision, tampered) == :not_applicable

    other_session = %{si | session_id: session()}
    assert SemanticStrategy.select(other_session) == :not_applicable

    changed_event = put_in(si, [
      Access.key!(:sentence_meaning), Access.key!(:event_semantics),
      Access.key!(:event_existence)
    ], :asserted)
    assert SemanticStrategy.select(changed_event) == :not_applicable
    assert SemanticStrategy.realize(plan, changed_event.sentence, @features,
      @decision, changed_event) == :not_applicable
  end

  test "question, high-risk and disallowed actions cannot produce nominal follow-up" do
    si = bird()
    question = %{si | sentence: "Is it sitting on the fence?"}
    assert SemanticStrategy.select(question) == :not_applicable
    assert SymbolicGeneration.generate(question.sentence, @features,
      @decision, question) == :not_applicable

    assert SymbolicGeneration.generate(si.sentence,
      %{risk_bucket: :high, intent: :unknown}, @decision, si) == :not_applicable
    assert SymbolicGeneration.generate(si.sentence, @features,
      %{action: :suppress}, si) == :not_applicable

    assert SymbolicGeneration.generate(si.sentence,
      %{risk_bucket: :low, intent: :question}, @decision, si) == :not_applicable
  end
end
