defmodule Core.Response.AttentionFallbackTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Response
  alias Core.Response.AttentionFallback

  setup do
    previous = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if previous == :unset,
        do: Application.delete_env(:core, :llm_inference_enabled?),
        else: Application.put_env(:core, :llm_inference_enabled?, previous)
    end)

    :ok
  end

  defp focused_si(session, sentence) do
    WMComprehension.store(%{
      source: :user, session_id: session, sentence: sentence, intent: :ask_info,
      comprehension: %{
        recognized_topics: ["ocean"],
        understood: [], uncertain: ["ocean"], degraded?: true
      },
      trace: []
    }, comprehension_wm?: true)
  end

  test "no-LLM continuation uses the admitted same-session focus, not a fabricated answer" do
    sid = "attention-response-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    si = focused_si(sid, "Tell me about the Pacific Ocean?")
    assert si.wm_context.topic == "the Pacific Ocean"
    assert si.wm_context.attention.scope == :session
    assert si.wm_context.attention.salience == 1.0

    assert {:ok, reply, metadata} =
      AttentionFallback.generate(si.sentence,
        %{risk_bucket: :low, intent: :ask_info}, %{action: :answer}, si)

    assert reply =~ "the Pacific Ocean"
    assert reply =~ "don't have a grounded answer"
    refute reply =~ ~s|what you mean by "ocean"|
    assert metadata.status == :attention_continuation
    assert metadata.provenance == :admitted_same_session_comprehension
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false

    {_tone, planned, plan_meta} =
      Response.plan(Map.merge(si, %{text: si.sentence, confidence: 0.7}), %{})
    assert planned == reply
    assert plan_meta.response_source == :wm_attention_fallback
    assert plan_meta.curiosity_probe == nil

    attached = Core.Response.Attach.maybe_build_response_plan(
      Map.merge(si, %{confidence: 0.7}), [])
    assert attached.response_text == reply
    assert attached.response_meta.response_source == :wm_attention_fallback
  end


  test "a different unresolved term still triggers repair within a recognized topic request" do
    sid = "attention-topic-repair-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)

    sentence = "Tell me about the Pacific Ocean and flarped?"
    si =
      WMComprehension.store(%{
        source: :user, session_id: sid, sentence: sentence, intent: :ask_info,
        comprehension: %{
          recognized_topics: ["ocean"],
          understood: [],
          uncertain: ["ocean", "flarped"],
          unresolved: ["flarped"],
          degraded?: true
        },
        trace: []
      }, comprehension_wm?: true)

    assert si.wm_context.attention.salience > 0.0

    assert {:ok, reply, metadata} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :ask_info},
        %{action: :answer}, si)

    assert metadata.status == :attention_comprehension_repair
    assert metadata.unresolved_term == "flarped"
    assert String.contains?(reply, "flarped")
    refute String.contains?(reply, ~s|using "ocean"|)
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false
  end

  test "unadmitted, stale, other-session, and safety-blocked focus cannot be voiced" do
    sid = "attention-private-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    si = focused_si(sid, "Tell me about the Pacific Ocean?")
    features = %{risk_bucket: :low, intent: :ask_info}
    action = %{action: :answer}

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features, action,
        %{si | session_id: "another-session"})

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features, action,
        Map.put(si, :wm_context, %{topic: "private context"}))

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence,
        %{features | risk_bucket: :high}, action, si)

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features,
        %{action: :ask_clarifying_question}, si)

    Brain.defocus(WMComprehension.frame_id(sid))
    assert :not_applicable ==
      AttentionFallback.generate(si.sentence, features, action, si)
  end

  defp repair_si(session, sentence, opts \\ []) do
    verified? = Keyword.get(opts, :verified?, true)
    recognized? = Keyword.get(opts, :recognized?, false)
    focus_subject = Keyword.get(opts, :focus_subject, "bird")
    uncertain = Keyword.get(opts, :uncertain, ["flarped", "blorp"])

    lexical =
      if verified? do
        [%{dictionary_verified?: true, id: "bird|noun|1",
          phrase: "bird", definition: "an animal", context_score: 0.9}]
      else
        []
      end

    WMComprehension.store(%{
      source: :user,
      session_id: session,
      sentence: sentence,
      intent: :statement,
      lexical_constructions: lexical,
      symbolic_frame: if(recognized?, do: %{subject: focus_subject}, else: %{}),
      comprehension: %{
        recognized_topics: [],
        understood: if(verified?, do: ["bird"], else: []),
        uncertain: uncertain,
        unresolved: uncertain,
        degraded?: true
      },
      trace: []
    }, comprehension_wm?: true)
  end

  test "unknown term and verified lexical anchor yield a targeted partial-comprehension repair" do
    sid = "attention-repair-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped beside the blorp."
    si = repair_si(sid, sentence)

    assert si.wm_context.attention.goal_terms == ["bird", "flarped", "blorp"]
    assert si.wm_context.attention.salience > 0.0

    assert {:ok, reply, metadata} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert reply ==
      "I understand \"bird\", but I'm not sure how you're using \"flarped\" here. Could you clarify that part?"
    assert metadata.status == :attention_comprehension_repair
    assert metadata.recognized_level == :dictionary_verified
    assert metadata.unresolved_term == "flarped"
    assert metadata.attention_selected? == true
    assert metadata.provenance == :admitted_same_session_comprehension
    assert metadata.evidence_level == :conversation_context_only
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false

    {_tone, planned, meta} =
      Response.plan(Map.merge(si, %{text: sentence, confidence: 0.7}), %{})
    assert planned == reply
    assert meta.response_source == :wm_attention_fallback
    assert meta.curiosity_probe == nil

    attached = Core.Response.Attach.maybe_build_response_plan(
      Map.merge(si, %{confidence: 0.7}), [])
    assert attached.response_text == reply
    assert attached.response_meta.response_source == :wm_attention_fallback
  end

  test "recognized focus is distinguished from verified lexical understanding" do
    sid = "attention-recognized-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped."
    si = repair_si(sid, sentence, verified?: false, recognized?: true,
      uncertain: ["flarped"])

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert reply ==
      "I'm following \"bird\", but I'm not sure how you're using \"flarped\" here. Could you clarify that part?"
    assert meta.recognized_level == :recognized_focus
    assert meta.factual_claims? == false
  end

  test "an unresolved word can be selected without inventing a known referent" do
    sid = "attention-unresolved-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "Flarped."
    si = repair_si(sid, sentence, verified?: false,
      uncertain: ["Flarped"])

    assert si.wm_context.entities == []
    assert si.wm_context.attention.goal_terms == ["Flarped"]

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :unknown}, %{action: :ask_first}, si)

    assert reply ==
      "I'm not sure how you're using \"Flarped\" here. Could you clarify what you mean?"
    assert meta.recognized_level == :none
    assert meta.recognized_term == nil
    assert meta.unresolved_term == "Flarped"
  end

  test "repair refuses a forged unknown, missing witness, and already-interpreted meaning" do
    sid = "attention-boundary-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped."
    si = repair_si(sid, sentence, uncertain: ["flarped"])

    # An unknown must occur on the current surface, not only in cached WM.
    ungrounded = %{si | sentence: "The bird arrived."}
    assert :not_applicable ==
      AttentionFallback.generate(ungrounded.sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, ungrounded)

    # The same uncertain word in a later utterance cannot borrow the
    # preceding turn's admitted focus or lexical comprehension.
    rephrased = %{si | sentence: "The bird flarped again."}
    assert :not_applicable ==
      AttentionFallback.generate(rephrased.sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, rephrased)

    # A dictionary-verified lexical witness is needed before using
    # the stronger "I understand" wording.
    unverified = Map.put(si, :lexical_constructions, [])
    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, unverified)
    refute String.contains?(reply, "I understand")
    assert meta.recognized_level == :none

    completed = Map.put(si, :sentence_meaning, %{kind: :explicit_temporal_report})
    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, completed)

    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :high, intent: :statement},
        %{action: :ask_first}, si)
    assert :not_applicable ==
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :safe_redirect}, si)
  end


  test "attention selects a later unresolved term over mere lexical list order" do
    sid = "attention-priority-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird flarped beside the blorp."
    si = repair_si(sid, sentence, recognized?: true, focus_subject: "blorp")

    assert si.wm_context.attention.goal_terms == ["blorp"]

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert String.contains?(reply, "I understand \"bird\"")
    assert String.contains?(reply, "using \"blorp\"")
    assert meta.unresolved_term == "blorp"
    assert meta.attention_selected? == true
    assert meta.recognized_level == :dictionary_verified
    assert meta.llm_called? == false
  end


  defp temporal_report_si(session) do
    words = ~w(The bird is sitting on the fence before the cat is sleeping under the tree)
    pos = ~w(determiner noun verb verb preposition determiner noun conjunction determiner noun verb verb preposition determiner noun)

    cells =
      Enum.zip(words, pos)
      |> Enum.map(fn {word, part} ->
        word = String.downcase(word)
        %{
          id: "#{word}|#{part}|1", norm: word, pos: part,
          semantic_atoms: if(word in ["sitting", "sleeping"],
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])
        }
      end)

    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, i} ->
        %{index: i, n: 1,
          phrase: if(i == 14, do: word <> ".", else: word)}
      end)

    si = %{
      source: :user,
      session_id: session,
      sentence: Enum.join(words, " ") <> ".",
      intent: :statement,
      tokens: tokens,
      active_cells: cells,
      lifg_choices: cells |> Enum.with_index()
        |> Enum.map(fn {cell, i} ->
          %{token_index: i, id: cell.id}
        end),
      comprehension: %{
        recognized_topics: ["sleeping"], understood: [],
        uncertain: ["bird", "sitting", "before", "cat"],
        degraded?: true
      },
      trace: []
    }

    si
    |> Core.Semantic.SentenceMeaning.attach()
    |> WMComprehension.store(comprehension_wm?: true)
  end

  test "verified reported order voices concrete known and unresolved parts, not an intent keyword" do
    sid = "attention-meaning-report-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    si = temporal_report_si(sid)
    assert {:ok, %{kind: :explicit_temporal_report}} =
      Core.Semantic.TemporalSentence.verified(si)

    assert si.wm_context.attention.salience > 0.0

    assert {:ok, reply, meta} =
      AttentionFallback.generate(si.sentence,
        %{risk_bucket: :low, intent: :statement}, %{action: :answer}, si)

    assert reply ==
      "I can interpret the order you described: the bird sitting on the fence before the cat sleeping under the tree. What I haven't established is the exact dictionary sense of every word, the identities of the individual animals, or whether these events actually occurred."
    assert meta.status == :attention_comprehension_report
    assert meta.provenance == :revalidated_temporal_clause_composition
    assert meta.known_parts == [:first_event_property, :second_event_property,
      :reported_temporal_order]
    assert :exact_dictionary_senses in meta.unresolved_parts
    assert meta.semantic_verified? == true
    assert meta.factual_claims? == false
    assert meta.llm_called? == false

    {_tone, planned, plan_meta} =
      Response.plan(Map.merge(si, %{text: si.sentence, confidence: 0.7}), %{})
    assert planned == reply
    assert plan_meta.response_source == :wm_attention_fallback
  end

  test "an edited temporal meaning cannot authorize a comprehension report" do
    sid = "attention-meaning-spoof-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    si = temporal_report_si(sid)

    spoofed = put_in(si, [:sentence_meaning, :connective], :after)
    assert :not_applicable ==
      Core.Semantic.TemporalSentence.verified(spoofed)

    assert {:ok, _reply, meta} =
      AttentionFallback.generate(spoofed.sentence,
        %{risk_bucket: :low, intent: :statement}, %{action: :answer}, spoofed)
    refute meta.status == :attention_comprehension_report

    assert :not_applicable ==
      AttentionFallback.generate(si.sentence,
        %{risk_bucket: :high, intent: :statement}, %{action: :answer}, si)
  end


  test "weak LIFG senses without truly unknown words are reported, not mistaken for definitions" do
    sid = "attention-weak-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence =
      "The bird is sitting on the fence before the cat is sleeping under the tree."

    si =
      WMComprehension.store(%{
        source: :user, session_id: sid, sentence: sentence, intent: :statement,
        comprehension: %{
          keyword: "sleeping",
          recognized_topics: ["sleeping"],
          understood: [],
          uncertain: ["bird", "sitting", "before", "cat"],
          unresolved: [],
          degraded?: true
        },
        trace: []
      }, comprehension_wm?: true)

    assert si.wm_context.attention.goal_terms ==
      ["bird", "sitting", "before", "cat"]

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :answer}, si)

    assert reply ==
      ~s|I can identify "bird", "sitting", and "before" as words in your sentence, but I haven't resolved their exact senses or the full relationship you're describing.|
    assert meta.status == :attention_lexical_gap_report
    assert meta.known_level == :surface_terms_only
    assert meta.uncertain_lexical_parts == ["bird", "sitting", "before"]
    assert meta.factual_claims? == false
    assert meta.llm_called? == false
    refute String.contains?(reply, "what you mean by")
    refute String.contains?(reply, ~s|"sleeping"|)
  end


  test "context-guided fuzzy repair offers a provisional guess without asserting it" do
    sid = "attention-fuzzy-hypothesis-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The bird is flarping on the fence."

    si = repair_si(sid, sentence, verified?: false,
      uncertain: ["flarping"])

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert reply ==
      ~s|I'm not sure how you're using "flarping" here. Did you mean "flapping"?|
    assert meta.status == :attention_comprehension_repair
    assert meta.unresolved_term == "flarping"
    assert meta.fuzzy_suggestion.replacement == "flapping"
    assert meta.fuzzy_suggestion.status == :hypothesis_only
    assert meta.semantic_verified? == false
    assert meta.factual_claims? == false
    assert meta.llm_called? == false

    assert si.sentence == sentence
    assert si.wm_context.turn_sha256 != nil
  end

  test "non-contextual unknown keeps ordinary targeted clarification" do
    sid = "attention-fuzzy-abstains-#{System.unique_integer([:positive])}"
    on_exit(fn -> Brain.defocus(WMComprehension.frame_id(sid)) end)
    sentence = "The cat is flarping on the fence."
    si = repair_si(sid, sentence, verified?: false, uncertain: ["flarping"])

    assert {:ok, reply, meta} =
      AttentionFallback.generate(sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :ask_first}, si)

    assert reply == ~s|I'm not sure how you're using "flarping" here. Could you clarify what you mean?|
    assert meta.fuzzy_suggestion == nil
    assert meta.llm_called? == false
  end

end
