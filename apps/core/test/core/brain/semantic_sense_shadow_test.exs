defmodule Core.Brain.SemanticSenseShadowTest do
  use ExUnit.Case, async: false

  alias Core.Brain.SemanticSenseShadow
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @proof [:contiguous_surface, :attested_nominal_subject,
          :attested_nominal_object, :attested_finite_present_verb]
  @inflected ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp input(patient \\ "picture") do
    choices = [
      %{token_index: 1, id: "artist|noun|1", definition: "A person who makes art.",
        candidate_probs: %{"artist|noun|1" => 1.0}},
      %{token_index: 2, id: "draws|verb|1", definition: "An inflected form of draw."},
      %{token_index: 4, id: "#{patient}|noun|1",
        definition: "An image created with lines and colours.",
        candidate_probs: %{"#{patient}|noun|1" => 1.0}}
    ]

    %{
      source: :user,
      sentence: "The artist draws a #{patient}.",
      session_id: "live-sense-shadow-fixture",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "artist", n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: patient <> ".", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @proof,
          roles: %{subject_determiner: 0, subject_head: 1,
                   predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: "artist|noun|1", norm: "artist", pos: "noun",
          definition: "A person who makes art."},
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          definition: "An inflected form of draw.", semantic_atoms: @inflected},
        %{id: "#{patient}|noun|1", norm: patient, pos: "noun",
          definition: "An image created with lines and colours."}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      response_text: "Preexisting response unaffected",
      selected_action: :answer_user,
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp dictionary do
    [
      %{id: "draw|verb|1", norm: "draw", pos: "verb",
        definition: "(transitive) To pull an object in a particular direction."},
      %{id: "draw|verb|2", norm: "draw", pos: "verb",
        definition: "(transitive) To produce a picture by tracing lines."},
      %{id: "draw|verb|3", norm: "draw", pos: "verb",
        definition: "(transitive) To draw water from a well."}
    ]
  end

  test "eligible semantic chat adds one compact trace with role evidence, changing no prior state" do
    si = input()
    previous_trace = si.trace
    out = SemanticSenseShadow.attach(si,
      fetch_base_senses: fn base ->
        assert base == "draw"
        dictionary()
      end)

    assert [%{stage: :semantic_sense_shadow, meta: meta} | ^previous_trace] = out.trace
    assert meta.mode == :shadow_only
    assert meta.role_expansion_enabled?
    assert meta.status == :provisional_hypothesis
    assert meta.proposed_id == "draw|verb|2"
    assert meta.original_surface_id == "draws|verb|1"
    assert meta.sense_count == 3
    assert length(meta.top_candidates) <= 3
    assert length(meta.exclusions) <= 8
    assert meta.role_sources != []
    assert meta.sense_verified? == false
    assert meta.applied_to_lifg? == false
    assert meta.synaptic_weight_transferred? == false
    assert meta.response_changed? == false
    assert Jason.encode!(meta) =~ "draw|verb|2"
    assert Map.delete(out, :trace) == Map.delete(si, :trace)
    assert out.response_text == si.response_text
    assert out.lifg_choices == si.lifg_choices
    assert out.atl_slate == si.atl_slate
    assert out.semantic_choices == si.semantic_choices
  end

  test "the shadow stage can be disabled with no trace or cognitive changes" do
    si = input()
    assert SemanticSenseShadow.attach(si, semantic_sense_shadow?: false,
      fetch_base_senses: fn _ -> raise "must not query DB" end) == si
  end

  test "ineligible utterances still have bounded skipped evidence without a dictionary read" do
    si = %{sentence: "Good morning.", source: :assistant, trace: [],
      lifg_choices: [], response_text: "Good morning."}
    out = SemanticSenseShadow.attach(si,
      fetch_base_senses: fn _ -> raise "no validated human SVO" end)

    assert [%{stage: :semantic_sense_shadow, decision: :skipped, meta: meta}] = out.trace
    assert meta.mode == :shadow_only
    assert meta.status == :skipped
    assert meta.response_changed? == false
    assert Jason.encode!(meta) =~ "skip_reason"
    assert Map.delete(out, :trace) == Map.delete(si, :trace)
  end

  test "unexpected dictionary failure is isolated and does not interrupt chat" do
    si = input()
    out = SemanticSenseShadow.attach(si,
      fetch_base_senses: fn _ -> raise "temporary DB error" end)

    assert [%{stage: :semantic_sense_shadow, decision: :skipped} | _] = out.trace
    assert out.response_text == si.response_text
    assert out.lifg_choices == si.lifg_choices
  end

  test "restriction-exclusion reasons are JSON-serializable without losing their type" do
    metadata =
      SemanticSenseShadow.compact(%{
        status: :abstained, reason: :insufficient_lexical_evidence,
        role_expansion_sources: [], eligible_top: [],
        exclusions: [
          %{id: "throw|verb|5", grammar: :compatible_direct_object,
            exclusions: [{:missing_restriction_witness, :cricket_bowling}]}
        ]
      })

    assert [%{exclusions: [%{kind: :missing_restriction_witness,
                             detail: :cricket_bowling}]}] = metadata.exclusions
    assert is_binary(Jason.encode!(metadata))
  end
end
