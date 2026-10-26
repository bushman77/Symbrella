defmodule Core.Semantic.ConstructionGeneralizationTest do
  use ExUnit.Case, async: false

  alias Core.Semantic.{ConstructionSchema, FrameUnifier, SentenceMeaning}
  alias Core.Syntax.{Signatures, RoleCompetition}
  alias Core.Response
  alias Core.Response.LexicalDefinition

  defp row(word, pos, sense \\ 1, gloss \\ "A supported dictionary definition.") do
    %{
      id: "#{word}|#{pos}|#{sense}",
      norm: word,
      pos: pos,
      definition: gloss,
      source: :ltm,
      semantic_atoms: []
    }
  end

  @anchors [
    {"define", "verb"}, {"define", "noun"},
    {"please", "adverb"}, {"please", "verb"},
    {"the", "determiner"},
    {"tell", "verb"}, {"me", "pronoun"},
    {"meaning", "noun"}, {"definition", "noun"},
    {"of", "preposition"},
    {"mean", "verb"},
    {"artist", "noun"}, {"artist", "proper_noun"},
    {"bank", "noun"}, {"bank", "verb"}
  ]

  defp lexicon do
    base = Enum.map(@anchors, fn {word, pos} -> row(word, pos) end)

    base ++ [
      row("artist", "noun", 2, "An occupation in art."),
      row("bank", "noun", 2, "Land beside a river.")
    ]
  end

  defp tokens(sentence) do
    sentence
    |> String.downcase()
    |> String.replace(~r/[?!.]+\z/u, "")
    |> String.split(~r/\s+/u, trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {word, index} ->
      %{index: index, phrase: word, n: 1, mw: false}
    end)
  end

  defp input(sentence, intent, cells \\ nil) do
    %{
      sentence: sentence, text: sentence, source: :test, intent: intent,
      confidence: 0.90, session_id: "construction-#{System.unique_integer([:positive])}",
      trace: [],
      tokens: tokens(sentence),
      active_cells: cells || lexicon()
    }
  end

  defp run(si) do
    si
    |> Signatures.attach()
    |> RoleCompetition.attach()
    |> ConstructionSchema.attach()
    |> FrameUnifier.attach()
    |> SentenceMeaning.attach()
  end

  test "variable lexical targets and distinct surface structures unify to one operation" do
    for {sentence, intent, construction, target} <- [
      {"define artist", :define, :definition_imperative, "artist"},
      {"please define artist", :define, :definition_imperative, "artist"},
      {"define the artist", :define, :definition_imperative, "artist"},
      {"what does artist mean?", :define, :meaning_wh_question, "artist"},
      {"what is the meaning of artist?", :define, :meaning_wh_question, "artist"},
      {"tell me the meaning of artist", :command, :meaning_request_imperative, "artist"},
      {"please tell me the meaning of bank", :command,
       :meaning_request_imperative, "bank"}
    ] do
      out = run(input(sentence, intent))
      assert [%{operation: :define, schema: ^construction}] = out.construction_candidates
      assert %{status: :structurally_interpreted, operation: :define,
               construction: ^construction, speech_act: :request} =
               out.semantic_request_frame
      assert out.semantic_request_frame.target.lemma == target
      assert out.semantic_request_frame.grammatical.role == :lexical_target
      assert out.semantic_request_frame.semantic.selected_sense == nil
      assert out.semantic_request_frame.semantic.status == :multiple_candidates_unresolved
      assert {:ok, _} = FrameUnifier.verified(out, sentence)
      assert %{kind: :lexical_definition_request, status: :structurally_interpreted,
               speech_act: :request_information} = out.sentence_meaning
      assert out.sentence_meaning.roles.lexical_target.lemma == target

      assert {:ok, answer, meta} =
        LexicalDefinition.generate(sentence,
          %{risk_bucket: :low, intent: intent}, %{action: :answer}, out)
      assert answer =~ ~s("#{target}" has multiple recorded meanings:)
      assert meta.construction == construction
      assert meta.selected_sense_verified? == false
      assert meta.llm_called? == false
    end
  end

  test "definition imperative retains independently attested grammatical roles" do
    out = run(input("please define artist", :define))
    assert [%{source: :core_syntax,
              roles: %{predicate: 1, object_head: 2}}] =
      Enum.filter(out.sentence_signatures, &(&1.id == :definition_imperative))

    assert %{status: :supported, pos: "verb", role: :predicate} =
      out.role_competition.roles[1]
    assert %{status: :supported, pos: "noun", role: :object_head} =
      out.role_competition.roles[2]
    assert out.semantic_request_frame.grammatical.pos == "noun"
    refute out.semantic_request_frame.semantic.selected_sense
  end

  test "new paraphrase reaches production response planner without the LLM" do
    prior = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if prior == :unset,
        do: Application.delete_env(:core, :llm_inference_enabled?),
        else: Application.put_env(:core, :llm_inference_enabled?, prior)
    end)

    sentence = "tell me the meaning of artist"
    turn = run(input(sentence, :command))
    assert LexicalDefinition.requested_term(sentence) == nil

    {_tone, reply, meta} = Response.plan(turn, %{})
    assert reply =~ ~s("artist" has multiple recorded meanings:)
    assert meta.response_source == :symbolic_lexical_definition
    assert meta.response_reflection.construction == :meaning_request_imperative
    assert meta.response_reflection.llm_called? == false

    attached = Core.Response.Attach.maybe_build_response_plan(turn, [])
    assert attached.response_text == reply
    assert attached.response_meta.response_source == :symbolic_lexical_definition
  end

  test "reject incomplete commands, unrelated clauses, and unsupported target vocabulary" do
    for sentence <- [
      "define artist now",
      "artist define",
      "what does artist mean and why?",
      "tell me the history of artist",
      "tell me the meaning of artist and bank",
      "the artist defines a bank",
      "what does bank define?"
    ] do
      out = run(input(sentence, :define))
      assert out.construction_candidates == [], sentence
      assert out.semantic_request_frame == nil, sentence
      refute out.sentence_meaning.kind == :lexical_definition_request
    end

    missing_word = run(input("tell me the meaning of imaginarything", :command))
    assert missing_word.semantic_request_frame == nil

    missing_anchor =
      input("tell me the meaning of artist", :command,
        Enum.reject(lexicon(), &(&1.norm == "meaning")))
      |> run()
    assert missing_anchor.semantic_request_frame == nil
  end

  test "reject fabricated role adjudication, edited frame, mismatched input and unsafe action" do
    base = input("define artist", :define)
    no_roles = base |> Signatures.attach() |> ConstructionSchema.attach()
    assert no_roles.construction_candidates == []

    original = run(base)
    tampered = put_in(original, [:semantic_request_frame, :target, :lemma], "bank")
    assert FrameUnifier.verified(tampered, original.sentence) == :not_applicable
    assert :not_applicable ==
      LexicalDefinition.generate(original.sentence,
        %{risk_bucket: :low}, %{action: :answer}, tampered)

    assert FrameUnifier.verified(original, "define bank") == :not_applicable
    assert :not_applicable ==
      LexicalDefinition.generate(original.sentence,
        %{risk_bucket: :high}, %{action: :answer}, original)
  end

  test "missing syntax and tied dictionary senses do not become invented facts" do
    si = run(input("what does bank mean?", :define))
    assert si.semantic_request_frame.grammatical.pos == :undetermined
    assert "noun" in si.semantic_request_frame.grammatical.pos_alternatives
    assert "verb" in si.semantic_request_frame.grammatical.pos_alternatives
    assert si.semantic_request_frame.semantic.candidate_count >= 2
    assert si.semantic_request_frame.semantic.selected_sense == nil
    assert si.sentence_meaning.grounding.external_facts == :not_checked
  end
end
