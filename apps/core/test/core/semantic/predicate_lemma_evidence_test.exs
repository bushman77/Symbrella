defmodule Core.Semantic.PredicateLemmaEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.PredicateLemmaEvidence

  @predicate %{
    token_index: 2, word: "granted", attested_verb_ids: ["granted|verb|1"]
  }

  @granted %{
    id: "granted|verb|1",
    norm: "granted",
    pos: "verb",
    token_index: 2,
    definition: "(form-of, participle, past) simple past and past participle of grant",
    semantic_atoms: ["tag:form-of", "tag:participle", "tag:past", "pos_raw:verb"]
  }

  @grant [
    %{id: "grant|verb|1", norm: "grant", pos: "verb",
      definition: "(ditransitive) To give (permission or wish).",
      semantic_atoms: ["tag:ditransitive", "pos_raw:verb"]},
    %{id: "grant|verb|2", norm: "grant", pos: "verb",
      definition: "(ditransitive) To give (bestow upon or confer, particularly in answer to prayer or request).",
      semantic_atoms: ["tag:ditransitive", "pos_raw:verb"]},
    %{id: "grant|verb|3", norm: "grant", pos: "verb",
      definition: "(transitive) To agree with (someone) on (something).",
      semantic_atoms: ["tag:transitive", "pos_raw:verb"]},
    %{id: "grant|verb|4", norm: "grant", pos: "verb",
      definition: "(intransitive) To assent; to consent.",
      semantic_atoms: ["tag:intransitive", "pos_raw:verb"]}
  ]

  test "actual form-of definition grounds all grant verb senses without a winner" do
    parent = self()
    lookup = fn
      "grant" ->
        send(parent, {:lookup, "grant"})
        {:ok, %{rows: @grant}}
      other ->
        flunk("unexpected dictionary query #{other}")
    end

    result = PredicateLemmaEvidence.audit(@predicate, [@granted],
      lookup: lookup)

    assert result.status == :lemma_senses_attested
    assert result.lemma == "grant"
    assert result.lemma_sense_count == 4
    assert [%{inflected_sense_id: "granted|verb|1",
              supporting_text: "simple past and past participle of grant",
              lemma: "grant"}] = result.forms

    assert Enum.map(result.lemma_senses, & &1.sense_id) ==
             ~w(grant|verb|1 grant|verb|2 grant|verb|3 grant|verb|4)

    assert Enum.map(result.lemma_senses, & &1.valency_tags) ==
             [["tag:ditransitive"], ["tag:ditransitive"],
              ["tag:transitive"], ["tag:intransitive"]]

    assert Enum.all?(result.lemma_senses, fn sense ->
      sense.valency_relation_to_explicit_object == :not_determined and
        not sense.semantic_relation_verified?
    end)

    assert_receive {:lookup, "grant"}
    refute_receive {:lookup, _}
    assert result.selected_sense_id == nil
    refute result.semantic_relation_verified?
    refute result.modifies_lifg?
  end

  test "form-of tag without an attested lemma definition does not query" do
    missing_gloss = %{@granted |
      definition: "(form-of, participle, past) inflection unavailable"}

    result = PredicateLemmaEvidence.audit(@predicate, [missing_gloss],
      lookup: fn _ -> flunk("no lemma lookup without a valid form-of gloss") end)

    assert result.status == :no_attested_form_of
    assert result.forms == []
  end

  test "token index, POS and source sense ID must all match" do
    for invalid <- [
      %{@granted | token_index: 3},
      %{@granted | pos: "noun"},
      %{@granted | id: "granted|verb|99"},
      %{@granted | norm: "other"}
    ] do
      result = PredicateLemmaEvidence.audit(@predicate, [invalid],
        lookup: fn _ -> flunk("unattested dictionary row must not be used") end)
      assert result.status == :no_attested_form_of
    end
  end

  test "lookup failure is retained without a fabricated lemma sense" do
    result = PredicateLemmaEvidence.audit(@predicate, [@granted],
      lookup: fn "grant" -> {:error, :ltm_query_failed} end)

    assert result.status == :lookup_error
    assert result.reason == :ltm_query_failed
    assert result.lemma == "grant"
    assert result.lemma_senses == []
    assert result.selected_sense_id == nil
  end

  test "empty lemma lookup abstains; unrelated noun senses are never promoted" do
    result = PredicateLemmaEvidence.audit(@predicate, [@granted],
      lookup: fn "grant" ->
        {:ok, [%{id: "grant|noun|1", norm: "grant", pos: "noun",
                 definition: "A gift."}]}
      end)

    assert result.status == :lemma_senses_unavailable
    assert result.lemma_sense_count == 0
    refute result.semantic_relation_verified?
  end
end
