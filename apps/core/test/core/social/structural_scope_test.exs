defmodule Core.Social.StructuralScopeTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.StructuralCompetition
  alias Core.Social.StructuralScope

  # Counterfactual tests call the REAL Brain module, so a drift in its 48-token
  # bound or its token-validity rule fails here instead of silently
  # mislabelling a sentence in /social.

  defp tokens(count, overrides \\ %{}) do
    for index <- 0..(count - 1) do
      base = %{index: index, phrase: "w#{index}", n: 1, kind: :word}
      Map.merge(base, Map.get(overrides, index, %{}))
    end
  end

  defp clause(last, roles \\ %{subject_head: 0, predicate: 1}) do
    %{
      id: :finite_present_svo_candidate,
      source: :core_syntax,
      span: {0, last},
      roles: roles,
      required_features: %{},
      evidence: [:contiguous_surface]
    }
  end

  defp si(tokens, signatures),
    do: %{tokens: tokens, sentence_signatures: signatures, lifg_choices: []}

  defp stub, do: %{status: :no_complete_candidate, candidates: []}

  defp observe(si) do
    structural = StructuralCompetition.resolve(si)
    {structural, StructuralScope.observe(si, structural)}
  end

  describe "48-token structural bound, checked against the real Brain module" do
    test "48 base tokens (last index 47) still produce a Brain candidate" do
      input = si(tokens(48), [clause(47)])
      {structural, scope} = observe(input)

      assert length(structural.candidates) == 1
      refute structural.status == :no_complete_candidate
      assert scope.reason == :not_applicable
    end

    test "49 base tokens (last index 48) exceed the bound in Brain and in the diagnostic" do
      input = si(tokens(49), [clause(48)])
      {structural, scope} = observe(input)

      assert structural.status == :no_complete_candidate
      assert structural.candidates == []
      assert scope.reason == :structural_token_budget_exceeded
    end
  end

  describe "reason classification" do
    test "a preferred or unresolved outcome is not a structural failure" do
      input = si(tokens(5), [clause(4)])
      {structural, scope} = observe(input)

      assert structural.status in [:preferred, :unresolved]
      assert scope.reason == :not_applicable
    end

    test "no base tokens" do
      {structural, scope} = observe(si([], []))

      assert structural.status == :no_complete_candidate
      assert scope.reason == :no_base_tokens
      assert scope.base_tokens == 0
    end

    test "only punctuation is no base tokens" do
      punct = [%{index: 0, phrase: ".", n: 1, kind: :punct}]
      {structural, scope} = observe(si(punct, []))

      assert structural.status == :no_complete_candidate
      assert scope.reason == :no_base_tokens
    end

    test "noncontiguous indices, with a contiguous counterfactual that Brain accepts" do
      gapped = for i <- [0, 1, 3], do: %{index: i, phrase: "w#{i}", n: 1, kind: :word}
      {structural, scope} = observe(si(gapped, [clause(3)]))

      assert structural.status == :no_complete_candidate
      assert scope.reason == :noncontiguous_token_indices

      {fixed, fixed_scope} = observe(si(tokens(4), [clause(3)]))
      assert length(fixed.candidates) == 1
      assert fixed_scope.reason == :not_applicable
    end

    test "contiguous tokens but no signatures" do
      {structural, scope} = observe(si(tokens(5), []))

      assert structural.status == :no_complete_candidate
      assert scope.reason == :no_sentence_signatures
      assert scope.sentence_signatures == 0
    end

    test "signatures exist but none spans the whole sentence" do
      {structural, scope} = observe(si(tokens(5), [clause(2)]))

      assert structural.status == :no_complete_candidate
      assert scope.reason == :no_full_span_signature
      assert scope.full_span_signatures == 0
    end

    test "a full-span signature Brain still rejects is reported as unknown eligibility" do
      # No :predicate or :verb role, so Brain's complete_clause?/2 refuses it.
      ineligible = clause(4, %{subject_head: 0, object_head: 4})
      {structural, scope} = observe(si(tokens(5), [ineligible]))

      assert structural.status == :no_complete_candidate
      assert scope.reason == :complete_signature_eligibility_unknown
      assert scope.full_span_signatures == 1
      assert scope.eligible_candidates == 0
    end
  end

  describe "source-token validity must agree with Brain" do
    test "invalid tokens are ignored identically by Brain and the diagnostic" do
      variants = [
        missing_phrase: %{phrase: nil},
        negative_index: %{index: -1},
        multiword_token: %{n: 2},
        punctuation_kind: %{kind: :punct}
      ]

      for {label, override} <- variants do
        broken = tokens(5, %{2 => override})
        {structural, scope} = observe(si(broken, [clause(4)]))

        assert structural.status == :no_complete_candidate, "Brain accepted #{label}"
        assert scope.reason == :noncontiguous_token_indices, "diagnostic disagreed on #{label}"
      end
    end
  end

  describe "coverage fields (observational)" do
    test "a clause core plus a constituent leaves the remaining token uncovered" do
      noun_phrase = %{
        id: :noun_phrase,
        source: :core_syntax,
        span: {0, 1},
        roles: %{determiner: 0, head: 1},
        required_features: %{},
        evidence: [:nominal_constituent]
      }

      clause_core = %{clause(4, %{subject_head: 2, predicate: 3}) | span: {2, 4}}
      scope = StructuralScope.observe(si(tokens(6), [noun_phrase, clause_core]), stub())

      assert scope.clause_core_signatures == 1
      assert scope.longest_span_tokens == 3
      assert scope.uncovered_token_count == 1
      refute scope.constituent_only?
      assert scope.reason == :no_full_span_signature
    end

    test "only constituents means no clause core" do
      noun_phrase = %{
        id: :noun_phrase,
        source: :core_syntax,
        span: {0, 1},
        roles: %{determiner: 0, head: 1},
        required_features: %{},
        evidence: [:nominal_constituent]
      }

      particle = %{
        id: :verb_particle_construction,
        source: :core_syntax,
        span: {2, 3},
        roles: %{verb: 2, particle: 3},
        required_features: %{},
        evidence: [:contiguous_surface]
      }

      scope = StructuralScope.observe(si(tokens(5), [noun_phrase, particle]), stub())

      assert scope.clause_core_signatures == 0
      assert scope.constituent_only?
      assert scope.uncovered_token_count == 1
    end

    test "malformed signatures and spans are ignored rather than raising" do
      junk = [%{}, %{span: :bad}, %{span: {3, 1}}, %{span: {-1, 2}}, nil, 7]
      scope = StructuralScope.observe(si(tokens(3), junk), stub())

      assert scope.longest_span_tokens == 0
      assert scope.uncovered_token_count == 3
      assert scope.clause_core_signatures == 0
    end
  end

  describe "observation never changes a grammatical decision" do
    test "observe is pure and Brain's outcome is identical before and after it" do
      input = si(tokens(5), [clause(2)])

      before_result = StructuralCompetition.resolve(input)
      first = StructuralScope.observe(input, before_result)
      second = StructuralScope.observe(input, before_result)

      assert first == second
      assert StructuralCompetition.resolve(input) == before_result
      assert first.factual_authority? == false
      assert first.scope == :observed_signatures_not_authoritative_eligibility
    end
  end
end
