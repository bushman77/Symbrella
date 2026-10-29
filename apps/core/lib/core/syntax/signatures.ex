defmodule Core.Syntax.Signatures do
  @moduledoc """
  Read-only grammatical evidence for Core.SemanticInput.

  Uses base tokens and *all* POS/morphological alternatives from active_cells.
  It does not choose BrainCell IDs, assign confidence, or update memory.

  Each signature's `span` is an inclusive range of original token indices.
  A detected construction is a hypothesis, not a semantic conclusion.
  """

  alias Core.Pipeline.Trace
  alias Core.MWE.Boundaries

  # POS sequences are grammatical patterns, not MWE entries. Longer nominal
  # patterns precede shorter constituents; valid overlapping hypotheses remain.
  @noun_patterns [
    {["determiner", "adjective", "adjective", "noun"],
     [:determiner, :modifier_1, :modifier_2, :head]},
    {["determiner", "adjective", "noun"], [:determiner, :modifier, :head]},
    {["adjective", "adjective", "noun"], [:modifier_1, :modifier_2, :head]},
    {["determiner", "noun"], [:determiner, :head]},
    {["adjective", "noun"], [:modifier, :head]},
    {["noun", "noun"], [:modifier, :head]}
  ]

  # These are lexical adverbs/prepositions that can also function as verbal
  # particles. Unlike a generic verb + adverb rule this bounded inventory
  # does not label ordinary manner adverbs ("walk quickly") as particles.
  # A candidate still requires dictionary POS evidence on the specific word.
  @particle_surfaces MapSet.new(~w(
    up down off on out in over away back around through apart along aside
  ))

  @pos_features %{
    "determiner" => :pos_determiner,
    "adjective" => :pos_adjective,
    "noun" => :pos_noun,
    "preposition" => :pos_preposition
  }

  # Do not mistake contracted pronouns/auxiliaries for possessive owners.
  # Other apostrophe-s tokens remain hypotheses until a nominal head follows.
  @contraction_bases MapSet.new(~w(
    he she it that there who what where when why how here let
  ))
  @max_possessive_tokens 4

  # A finite present predicate requires attested verb morphology and a
  # contiguous subject–verb–object construction. These patterns describe
  # structural hypotheses, not approved word meanings.
  @finite_present_patterns [
    {["determiner", "noun", "verb", "determiner", "noun"],
     [:subject_determiner, :subject_head, :predicate, :object_determiner, :object_head]},
    {["determiner", "noun", "verb", "noun"],
     [:subject_determiner, :subject_head, :predicate, :object_head]},
    {["pronoun", "verb", "determiner", "noun"],
     [:subject_head, :predicate, :object_determiner, :object_head]},
    {["pronoun", "verb", "noun"],
     [:subject_head, :predicate, :object_head]}
  ]
  @third_person_singular_pronouns MapSet.new(~w(he she it))

  @have_words ~w(have has had)
  defp regex_have_contract, do: ~r/^(?:i|you|we|they)'ve$/u
  defp regex_have_or_be_contract, do: ~r/^(?:he|she|it|that|there|who)'s$/u

  # These regular-past surfaces have been admitted by the bounded Colony
  # declarative composer. A source word alone never supplies a POS witness.
  @bounded_regular_past MapSet.new(~w(verified supported refuted contradicted produced))

  @type signature :: Core.SemanticInput.sentence_signature()

  @doc "Detect grammatical constructions without modifying the input."
  @spec detect(map()) :: [signature()]
  def detect(%{} = si) do
    cells = Map.get(si, :active_cells, [])
    lexicon = build_lexicon(cells)
    nodes = base_nodes(Map.get(si, :tokens, []), lexicon)

    noun_phrases = noun_phrase_constructions(nodes)
    possessive_phrases = possessive_noun_phrase_constructions(nodes, noun_phrases)

    (definition_imperatives(si, nodes) ++
       definition_request_structures(si, nodes) ++
       wh_aux_questions(si, nodes) ++ wh_aux_object_questions(si, nodes) ++
       progressive_locatives(nodes) ++ progressive_lexical_gaps(nodes) ++
       anaphoric_progressive_locatives(nodes) ++
       finite_present_svo(nodes) ++
       finite_past_svo(nodes) ++
       counted_present_svo(nodes) ++
       copular_relative_classifications(nodes) ++
       bounded_past_transitives(nodes) ++
       finite_past(nodes) ++ perfect_predicates(nodes) ++
       nominalized_adjective_subject_candidates(nodes) ++
       have_verb_infinitives(nodes) ++ verb_particle_constructions(nodes) ++ noun_phrases ++
       possessive_phrases ++
       prepositional_phrase_constructions(nodes, noun_phrases ++ possessive_phrases))
    |> Enum.uniq_by(fn sig -> {sig.id, sig.span, sig.roles} end)
  end

  def detect(_), do: []

  @doc """
  Attach observation-only signatures and a standard Core trace event.

  Does not alter tokens, sense_candidates, LIFG choices, or active_cells.
  """
  @spec attach(map()) :: map()
  def attach(%{} = si) do
    signatures = detect(si)

    si
    |> Map.put(:sentence_signatures, signatures)
    |> Trace.append(:sentence_signatures,
      decision: if(signatures == [], do: :no_match, else: :observed),
      reason: :grammatical_candidate_patterns,
      scores: %{},
      meta: %{
        count: length(signatures),
        constructions: Enum.frequencies_by(signatures, & &1.id)
      }
    )
  end

  def attach(other), do: other

  # Candidate aggregation preserves alternatives, even contradictory ones.
  defp build_lexicon(cells) when is_list(cells) do
    Enum.reduce(cells, %{}, fn cell, acc ->
      word = norm(get(cell, :norm) || get(cell, :word))
      pos = norm(get(cell, :pos))

      if word == "" or pos == "" do
        acc
      else
        candidate = %{
          pos: MapSet.new([pos]),
          morphology: morphology(get(cell, :semantic_atoms)),
          finite_present_verb?: pos == "verb" and finite_present_verb?(get(cell, :semantic_atoms)),
          plural_noun?: pos == "noun" and plural_noun?(get(cell, :semantic_atoms)),
          base_present_verb?: pos == "verb" and base_present_verb?(get(cell, :semantic_atoms))
        }

        Map.update(acc, word, candidate, fn existing ->
          %{
            pos: MapSet.union(existing.pos, candidate.pos),
            morphology: MapSet.union(existing.morphology, candidate.morphology),
            finite_present_verb?: existing.finite_present_verb? or candidate.finite_present_verb?,
            plural_noun?: existing.plural_noun? or candidate.plural_noun?,
            base_present_verb?: existing.base_present_verb? or candidate.base_present_verb?
          }
        end)
      end
    end)
  end

  defp build_lexicon(_), do: %{}

  defp morphology(atoms) when is_list(atoms) do
    tags = MapSet.new(atoms)
    form_of? = MapSet.member?(tags, "tag:form-of")
    past? = MapSet.member?(tags, "tag:past")
    participle? = MapSet.member?(tags, "tag:participle")

    cond do
      form_of? and past? and participle? -> MapSet.new([:past_participle])
      form_of? and participle? and MapSet.member?(tags, "tag:present") ->
        MapSet.new([:present_participle])
      form_of? and past? -> MapSet.new([:simple_past])
      true -> MapSet.new()
    end
  end

  defp morphology(_), do: MapSet.new()

  defp finite_present_verb?(atoms) when is_list(atoms) do
    tags = MapSet.new(atoms)

    Enum.all?(
      ["tag:form-of", "tag:indicative", "tag:present", "tag:singular", "tag:third-person"],
      &MapSet.member?(tags, &1)
    )
  end

  defp finite_present_verb?(_), do: false

  defp plural_noun?(tags) when is_list(tags),
    do: "tag:form-of" in tags and "tag:plural" in tags
  defp plural_noun?(_), do: false

  # An uninflected base-form verb is compatible with a plural-present
  # subject only when the original dictionary row attests verb POS.
  # Inflected/past/participle forms cannot be promoted to base verbs.
  defp base_present_verb?(tags) when is_list(tags),
    do: not Enum.any?(tags, &(&1 in ["tag:form-of", "tag:past", "tag:participle"]))
  defp base_present_verb?(_), do: false

  # Base tokens only; never treat MWE phrase windows as individual words.
  # Keep each original token index even when filtering punctuation.
  defp base_nodes(tokens, lexicon) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.flat_map(fn {token, position} ->
      word = norm(get(token, :phrase))
      n = get(token, :n)
      kind = get(token, :kind)

      if word == "" or (n != nil and n != 1) or kind in [:punct, "punct"] do
        []
      else
        index =
          case get(token, :index) do
            i when is_integer(i) and i >= 0 -> i
            _ -> position
          end

        entry =
          Map.get(lexicon, word, %{
            pos: MapSet.new(),
            morphology: MapSet.new(),
            finite_present_verb?: false,
            plural_noun?: false,
            base_present_verb?: false
          })
        [
          %{
            index: index,
            word: word,
            surface: get(token, :phrase),
            pos: entry.pos,
            morphology: entry.morphology,
            finite_present_verb?: entry.finite_present_verb?,
            plural_noun?: entry.plural_noun?,
            base_present_verb?: entry.base_present_verb?
          }
        ]
      end
    end)
  end

  defp base_nodes(_, _), do: []


  # An entire definition command is an imperative, not a noun phrase.
  # The intent is established before Core syntax runs, but the intent alone
  # never creates a signature: the *original* word order and exact POS
  # alternatives must agree. This is grammatical evidence, not an answer.
  #
  # The noun object is specifically a common noun: "define artist" is about
  # the lexical noun, even if an unrelated proper-noun dictionary row exists.
  defp definition_imperatives(si, nodes) do
    if get(si, :intent) in [:define, :command] and length(nodes) in 2..4 do
      parsed =
        case nodes do
          [predicate, object] ->
            {predicate, nil, object}

          [first, second, third] ->
            cond do
              first.word == "please" ->
                {second, nil, third}

              second.word in ~w(a an the) ->
                {first, second, third}

              true ->
                nil
            end

          [polite, predicate, determiner, object] ->
            if polite.word == "please" and determiner.word in ~w(a an the),
              do: {predicate, determiner, object}, else: nil

          _ ->
            nil
        end

      case parsed do
        {predicate, determiner, object} ->
          indexes = Enum.map(nodes, & &1.index)

          if predicate.word == "define" and
               indexes == Enum.to_list(0..(length(nodes) - 1)) and
               pos?(predicate, "verb") and pos?(object, "noun") and
               (is_nil(determiner) or pos?(determiner, "determiner")) and
               valid_surface?(nodes) do
            roles = %{predicate: predicate.index, object_head: object.index}
            roles =
              if determiner,
                do: Map.put(roles, :object_determiner, determiner.index),
                else: roles

            [signature(
              :definition_imperative,
              {0, object.index},
              roles,
              %{predicate: [:pos_verb], object_head: [:pos_noun]},
              [:complete_imperative_surface, :explicit_definition_command,
               :attested_verb_alternative, :attested_common_noun_object]
            )]
          else
            []
          end

        nil ->
          []
      end
    else
      []
    end
  end


  # Grammar for a family of lexical requests. These signatures are produced
  # before LIFG/ATL or FrameUnifier, from complete original token sequences
  # and independent dictionary POS alternatives.
  # No exact dictionary *sense* is inferred by a signature.
  defp definition_request_structures(si, nodes) do
    if get(si, :intent) in [:define, :command, :ask, :ask_info] and
         length(nodes) in 4..7 and valid_surface?(nodes) and
         Enum.map(nodes, & &1.index) == Enum.to_list(0..(length(nodes) - 1)) do
      words = Enum.map(nodes, & &1.word)
      leading = if hd(words) == "please", do: 1, else: 0
      core = Enum.drop(nodes, leading)

      cond do
        match?([_, _, _, _, _, _], core) and
          Enum.take(Enum.map(core, & &1.word), 5) ==
            ["tell", "me", "the", "meaning", "of"] and
          pos?(Enum.at(core, 0), "verb") and
          pos?(Enum.at(core, 1), "pronoun") and
          pos?(Enum.at(core, 2), "determiner") and
          pos?(Enum.at(core, 3), "noun") and
          pos?(Enum.at(core, 4), "preposition") and
          Enum.at(core, 5).pos != MapSet.new() ->
          roles = %{
            predicate: leading, recipient: leading + 1,
            determiner: leading + 2, nominal_head: leading + 3,
            preposition: leading + 4, lexical_target: leading + 5
          }
          roles = if leading == 1, do: Map.put(roles, :politeness, 0), else: roles

          [signature(
            :meaning_request_imperative, {0, length(nodes) - 1}, roles,
            %{predicate: [:pos_verb], recipient: [:pos_pronoun],
              nominal_head: [:pos_noun], preposition: [:pos_preposition]},
            [:complete_lexical_request, :attested_request_cues,
             :attested_target_alternatives, :original_contiguous_surface]
          )]

        leading == 0 and match?([_, _, _, _], core) and
          Enum.at(core, 0).word == "what" and
          Enum.at(core, 1).word == "does" and
          Enum.at(core, 3).word == "mean" and
          pos?(Enum.at(core, 3), "verb") and
          Enum.at(core, 2).pos != MapSet.new() ->
          [signature(
            :meaning_wh_question, {0, 3},
            %{wh: 0, auxiliary: 1, lexical_target: 2, predicate: 3},
            %{predicate: [:pos_verb]},
            [:complete_lexical_request, :attested_request_cues,
             :attested_target_alternatives, :original_contiguous_surface]
          )]

        leading == 0 and match?([_, _, _, _, _, _], core) and
          Enum.take(Enum.map(core, & &1.word), 3) == ["what", "is", "the"] and
          Enum.at(core, 3).word in ["meaning", "definition"] and
          Enum.at(core, 4).word == "of" and
          pos?(Enum.at(core, 2), "determiner") and
          pos?(Enum.at(core, 3), "noun") and
          pos?(Enum.at(core, 4), "preposition") and
          Enum.at(core, 5).pos != MapSet.new() ->
          [signature(
            :meaning_wh_question, {0, 5},
            %{wh: 0, auxiliary: 1, determiner: 2,
              nominal_head: 3, preposition: 4, lexical_target: 5},
            %{nominal_head: [:pos_noun], preposition: [:pos_preposition]},
            [:complete_lexical_request, :attested_request_cues,
             :attested_target_alternatives, :original_contiguous_surface]
          )]

        true ->
          []
      end
    else
      []
    end
  end

  # A bounded, compositional WH + auxiliary question frame. Adjunct adverbs
  # may appear after WH or around an optional negation before the predicate.
  # A negative contracted auxiliary is one base token, not two invented tokens.
  #
  # Roles retain the original base-token indices. This is evidence of a
  # complete clause, NEVER a choice of dictionary sense or an MWE deletion.
  @wh_words MapSet.new(~w(what who whom where when why how which))
  @question_auxiliaries MapSet.new(~w(do does did can could will would should may might))
  @negative_question_auxiliaries MapSet.new(~w(
    don't doesn't didn't can't couldn't won't wouldn't shouldn't mightn't
  ))
  @question_subjects MapSet.new(~w(i you he she it we they))
  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @topic_prepositions MapSet.new(~w(about of for on in at from with to))
  @negation_words MapSet.new(~w(not never))
  @max_question_tokens 64

  defp wh_aux_questions(si, nodes) do
    sentence = Map.get(si, :sentence) || ""

    if is_binary(sentence) and String.ends_with?(String.trim(sentence), "?") and
         length(nodes) <= @max_question_tokens do
      with [wh | after_wh] <- nodes,
           true <- wh.index == 0 and MapSet.member?(@wh_words, wh.word),
           {wh_modifier, [aux | after_aux]} <- take_wh_modifier(after_wh),
           true <- question_auxiliary?(aux),
           [subject | after_subject] <- after_aux,
           true <- (pos?(subject, "pronoun") or
                      MapSet.member?(@question_subjects, subject.word)),
           {predicate_modifier, negation, [verb, prep | topic]} <-
             take_predicate_prefix(after_subject),
           true <- pos?(verb, "verb"),
           true <- MapSet.member?(@topic_prepositions, prep.word),
           true <- topic != [] and
                     Enum.any?(Enum.take(topic, 7), fn node ->
                       pos?(node, "noun") or pos?(node, "proper_noun") or
                         pos?(node, "pronoun")
                     end),
           true <- valid_surface?(Enum.take(nodes, length(nodes) - length(topic))),
           true <- valid_surface?([prep, hd(topic)]) do
        roles =
          %{wh: wh.index, auxiliary: aux.index, subject: subject.index,
            predicate: verb.index, preposition: prep.index}
          |> maybe_question_role(:wh_modifier, wh_modifier)
          |> maybe_question_role(:predicate_modifier, predicate_modifier)
          |> maybe_question_role(:negation, negation)

        required =
          %{auxiliary: [:pos_auxiliary, :pos_modal, :pos_verb],
            subject: [:pos_pronoun], predicate: [:pos_verb],
            preposition: [:pos_preposition]}
          |> maybe_question_features(:wh_modifier, wh_modifier, [:pos_adverb])
          |> maybe_question_features(:predicate_modifier, predicate_modifier, [:pos_adverb])
          |> maybe_question_features(:negation, negation, [:pos_adverb, :pos_particle])

        evidence =
          [:wh_fronted, :auxiliary_inversion, :lexical_verb_available,
           :explicit_prepositional_topic, :question_punctuation] ++
            if(wh_modifier, do: [:wh_adverb_modifier], else: []) ++
            if(predicate_modifier, do: [:predicate_adverb_modifier], else: []) ++
            if(negation, do: [:explicit_negation], else: []) ++
            if(MapSet.member?(@negative_question_auxiliaries, aux.word),
              do: [:negative_auxiliary_contraction], else: [])

        [signature(:wh_aux_topic_question,
          {wh.index, List.last(nodes).index}, roles, required, evidence)]
      else
        _ -> []
      end
    else
      []
    end
  end

  # A complete WH object question with a determined nominal subject:
  # "What did the artist draw?" The interrogative WH asks for the unfilled
  # object, so this is a grammatical hypothesis, not an event or chosen sense.
  # Exactly five contiguous base tokens prevent a prefix of a longer clause
  # from acquiring a stronger structural status than the full utterance.
  defp wh_aux_object_questions(si, nodes) do
    sentence = Map.get(si, :sentence) || ""

    case nodes do
      [wh, aux, det, subject, predicate] = segment ->
        if is_binary(sentence) and String.ends_with?(String.trim(sentence), "?") and
             wh.index == 0 and wh.word in ~w(what who) and
             question_auxiliary?(aux) and
             MapSet.member?(@determiners, det.word) and pos?(det, "determiner") and
             pos?(subject, "noun") and pos?(predicate, "verb") and
             valid_surface?(segment) do
          [signature(
            :wh_aux_object_question,
            {wh.index, predicate.index},
            %{wh: wh.index, auxiliary: aux.index, subject_determiner: det.index,
              subject_head: subject.index, predicate: predicate.index},
            %{wh: [:pos_pronoun], auxiliary: [:pos_auxiliary, :pos_modal, :pos_verb],
              subject_determiner: [:pos_determiner],
              subject_head: [:pos_noun], predicate: [:pos_verb]},
            [:wh_fronted, :auxiliary_inversion, :bounded_determiner_surface,
             :attested_nominal_subject, :lexical_verb_available,
             :question_punctuation, :contiguous_surface]
          )]
        else
          []
        end

      _ ->
        []
    end
  end

  # "do" can be both adverb and verb in the production lexicon. Claim
  # the explicit question auxiliary before considering an optional WH adverb:
  # otherwise "What do you know ..." incorrectly consumes "do".
  defp take_wh_modifier([first | _] = nodes) do
    if question_auxiliary?(first),
      do: {nil, nodes},
      else: take_optional_adverb(nodes)
  end

  defp take_wh_modifier(nodes), do: {nil, nodes}

  # One lexical adverb per optional slot. We do not skip arbitrary tokens:
  # an unattested filler must not manufacture a strong sentence frame.
  defp take_optional_adverb([node | rest] = nodes) do
    if pos?(node, "adverb") and not MapSet.member?(@negation_words, node.word),
      do: {node, rest},
      else: {nil, nodes}
  end

  defp take_optional_adverb(nodes), do: {nil, nodes}

  defp take_optional_negation([node | rest] = nodes) do
    if MapSet.member?(@negation_words, node.word),
      do: {node, rest},
      else: {nil, nodes}
  end

  defp take_optional_negation(nodes), do: {nil, nodes}

  # A lexical verb can also carry an adverb alternative. Prefer the
  # complete verb + topic-preposition structure before consuming it as an
  # optional modifier. Do not mutate the original POS candidates.
  defp take_predicate_prefix([verb, prep | _] = nodes) do
    if pos?(verb, "verb") and MapSet.member?(@topic_prepositions, prep.word) do
      {nil, nil, nodes}
    else
      take_predicate_prefix_with_modifiers(nodes)
    end
  end

  defp take_predicate_prefix(nodes), do: take_predicate_prefix_with_modifiers(nodes)

  defp take_predicate_prefix_with_modifiers(nodes) do
    {leading_adverb, after_adverb} = take_optional_adverb(nodes)
    {negation, after_negation} = take_optional_negation(after_adverb)

    {trailing_adverb, remaining} =
      if leading_adverb == nil,
        do: take_optional_adverb(after_negation),
        else: {nil, after_negation}

    {leading_adverb || trailing_adverb, negation, remaining}
  end

  defp question_auxiliary?(%{word: word}) do
    MapSet.member?(@question_auxiliaries, word) or
      MapSet.member?(@negative_question_auxiliaries, word)
  end

  defp maybe_question_role(roles, _key, nil), do: roles
  defp maybe_question_role(roles, key, node), do: Map.put(roles, key, node.index)

  defp maybe_question_features(features, _key, nil, _required), do: features

  defp maybe_question_features(features, key, _node, required),
    do: Map.put(features, key, required)

  # Deliberately weak evidence: a simple-past form is *available* here.
  # Without a finite-clause parser, this is not a confirmed past predicate.
  # A complete, locally attested classification such as
  # "links were lines that refute the claim" can disambiguate the complement
  # noun. It does not assert truth, determine the precise noun sense, or treat
  # a proper name as a common noun. This provides syntactic evidence for
  # subsequent LIFG scoring; lexical decisions remain LIFG's responsibility.
  defp copular_relative_classifications(nodes) do
    nodes
    |> Enum.chunk_every(7, 1, :discard)
    |> Enum.flat_map(fn
      [subject, copula, complement, marker, relverb, det, object] = segment ->
        ordered? =
          Enum.map(segment, & &1.index) ==
            Enum.to_list(subject.index..(subject.index + 6))

        witnessed? =
          pos?(subject, "noun") and
            copula.word in ~w(is are was were) and
            (pos?(copula, "verb") or pos?(copula, "auxiliary")) and
            pos?(complement, "noun") and
            marker.word == "that" and
            pos?(relverb, "verb") and
            det.word in ~w(the a an) and
            pos?(det, "determiner") and
            pos?(object, "noun")

        # Do not apply the lower-case common-noun context to an explicitly
        # capitalized lexical name, even if another noun reading exists.
        common_surface? =
          is_binary(complement.surface) and
            complement.surface == String.downcase(complement.surface)

        if ordered? and witnessed? and common_surface? and valid_surface?(segment) do
          [
            signature(
              :copular_relative_classification,
              {subject.index, object.index},
              %{
                subject_head: subject.index,
                copula: copula.index,
                complement_head: complement.index,
                relative_marker: marker.index,
                relative_verb: relverb.index,
                object_determiner: det.index,
                object_head: object.index
              },
              %{
                subject_head: [:pos_noun],
                complement_head: [:pos_noun],
                relative_verb: [:pos_verb],
                object_determiner: [:pos_determiner],
                object_head: [:pos_noun]
              },
              [
                :contiguous_surface,
                :attested_subject_noun,
                :attested_copular_be,
                :attested_complement_noun,
                :explicit_relative_clause,
                :attested_relative_verb,
                :attested_object_noun
              ]
            )
          ]
        else
          []
        end

      _ ->
        []
    end)
  end

  # An attested D-N-V-D-N frame is stronger POS context than a local
  # "verified" noun reading, even when the imported verb sense lacks
  # Wiktextract's tag:form-of + tag:past. This is syntactic evidence only:
  # no generated dictionary IDs, meanings, or verified events.
  defp bounded_past_transitives(nodes) do
    nodes
    |> Enum.chunk_every(5, 1, :discard)
    |> Enum.flat_map(fn [subject_det, subject, predicate, object_det, object] = segment ->
      contiguous? =
        Enum.map(segment, & &1.index) ==
          Enum.to_list(subject_det.index..(subject_det.index + 4))

      attested? =
        MapSet.member?(@determiners, subject_det.word) and
          pos?(subject_det, "determiner") and pos?(subject, "noun") and
          MapSet.member?(@bounded_regular_past, predicate.word) and
          pos?(predicate, "verb") and
          MapSet.member?(@determiners, object_det.word) and
          pos?(object_det, "determiner") and pos?(object, "noun")

      if contiguous? and attested? and valid_surface?(segment) do
        [signature(
          :bounded_regular_past_transitive,
          {subject_det.index, object.index},
          %{subject_determiner: subject_det.index, subject_head: subject.index,
            predicate: predicate.index, object_determiner: object_det.index,
            object_head: object.index},
          %{subject_head: [:pos_noun], predicate: [:pos_verb],
            object_head: [:pos_noun]},
          [:contiguous_surface, :attested_nominal_subject,
           :attested_dictionary_verb, :bounded_past_surface,
           :attested_nominal_object]
        )]
      else
        []
      end
    end)
  end

  defp finite_past(nodes) do
    nodes
    |> Enum.with_index()
    |> Enum.flat_map(fn {verb, i} ->
      preceding = if i > 0, do: Enum.at(nodes, i - 1), else: nil

      if form?(verb, :simple_past) and not auxiliary_have?(preceding) do
        [
          signature(
            :finite_past_candidate,
            {verb.index, verb.index},
            %{verb: verb.index},
            %{verb: [:simple_past]},
            [:simple_past_form_available]
          )
        ]
      else
        []
      end
    end)
  end

  defp perfect_predicates(nodes) do
    nodes
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.flat_map(fn [aux, verb] ->
      if auxiliary_have?(aux) and form?(verb, :past_participle) do
        observations =
          [:have_auxiliary, :past_participle_form_available] ++
            if(ambiguous_contraction?(aux), do: [:have_or_be_contraction], else: [])

        [
          signature(
            :perfect_predicate,
            {aux.index, verb.index},
            %{auxiliary: aux.index, verb: verb.index},
            %{verb: [:past_participle]},
            observations
          )
        ]
      else
        []
      end
    end)
  end

  # An adjective following a determiner may function as a nominal group
  # before a perfect predicate ("the injured have recovered"). This does NOT
  # assign a noun sense, identify its referent, or bind it as an event subject.
  # The perfect auxiliary and participle are required grammatical witnesses;
  # the candidate nominal's span contains only the two original tokens.
  defp nominalized_adjective_subject_candidates(nodes) do
    nodes
    |> Enum.chunk_every(4, 1, :discard)
    |> Enum.flat_map(fn [det, adjective, aux, verb] = segment ->
      if pos?(det, "determiner") and pos?(adjective, "adjective") and
           auxiliary_have?(aux) and form?(verb, :past_participle) and
           valid_surface?(segment) do
        [signature(
          :nominalized_adjective_subject_candidate,
          {det.index, adjective.index},
          %{determiner: det.index, adjective: adjective.index},
          %{determiner: [:pos_determiner], adjective: [:pos_adjective]},
          [:determiner_adjective_surface, :following_perfect_predicate,
           :nominalization_only_hypothesized]
        )]
      else
        []
      end
    end)
  end

  # This is a syntactic frame, NOT a claim that the verb means obligation.
  # For example, "have tried to leave" fits the same broad structure.
  defp have_verb_infinitives(nodes) do
    nodes
    |> Enum.chunk_every(4, 1, :discard)
    |> Enum.flat_map(fn [aux, verb, marker, complement] ->
      if auxiliary_have?(aux) and pos?(verb, "verb") and
           marker.word == "to" and pos?(complement, "verb") do
        observations =
          [:have_auxiliary, :to_marker, :infinitive_verb_candidate] ++
            if(ambiguous_contraction?(aux), do: [:have_or_be_contraction], else: [])

        [
          signature(
            :have_verb_to_infinitive,
            {aux.index, complement.index},
            %{
              auxiliary: aux.index,
              verb: verb.index,
              infinitive_marker: marker.index,
              complement: complement.index
            },
            %{verb: [:pos_verb], complement: [:pos_verb]},
            observations
          )
        ]
      else
        []
      end
    end)
  end

  # Observe participants in a complete progressive locative clause:
  # determiner + noun + be + present-participle + preposition + determiner + noun.
  # This is a grammatical *candidate*, not a dictionary sense winner. A form-of
  # present participle must be present in the lexical alternatives; the chosen
  # sense remains untouched, even when LIFG prefers an adjective reading.
  # Additional semantic validation is performed by ParticipantSpatial.
  defp progressive_locatives(nodes) do
    nodes
    |> Enum.chunk_every(7, 1, :discard)
    |> Enum.flat_map(fn [subject_det, subject, aux, predicate, prep, loc_det, location] = segment ->
      if pos?(subject_det, "determiner") and pos?(subject, "noun") and
           aux.word == "is" and
           form?(predicate, :present_participle) and pos?(predicate, "verb") and
           pos?(prep, "preposition") and
           prep.word in ~w(on in under beside above below into) and
           pos?(loc_det, "determiner") and pos?(location, "noun") and
           valid_surface?(segment) do
        [signature(
          if(prep.word == "into",
            do: :progressive_directional_candidate,
            else: :progressive_locative_candidate),
          {subject_det.index, location.index},
          %{subject_determiner: subject_det.index, subject_head: subject.index,
            auxiliary: aux.index, predicate: predicate.index,
            preposition: prep.index, location_determiner: loc_det.index,
            location_head: location.index},
          %{subject_head: [:pos_noun], predicate: [:present_participle],
            preposition: [:pos_preposition], location_head: [:pos_noun]},
          [:contiguous_surface, :attested_nominal_subject,
           :attested_present_participle, :attested_preposition,
           :attested_nominal_location] ++
            if(prep.word == "into", do: [:directional_not_arrival], else: [])
        )]
      else
        []
      end
    end)
  end

  # A missing lexical predicate can occupy a progressive slot even when it
  # has no dictionary POS/participle row. This is *only* a grammatical gap
  # hypothesis: it must not become an LIFG winner or an asserted event.
  # The surrounding determiners, nominal heads and preposition require
  # independent lexical POS evidence; the predicate has none.
  defp progressive_lexical_gaps(nodes) do
    nodes
    |> Enum.chunk_every(7, 1, :discard)
    |> Enum.flat_map(fn [subject_det, subject, aux, predicate, prep, loc_det, location] = segment ->
      if pos?(subject_det, "determiner") and pos?(subject, "noun") and
           aux.word == "is" and
           MapSet.size(predicate.pos) == 0 and
           Regex.match?(~r/\A\p{L}{2,21}ing\z/u, predicate.word) and
           pos?(prep, "preposition") and
           prep.word in ~w(on in under beside above below) and
           pos?(loc_det, "determiner") and pos?(location, "noun") and
           valid_surface?(segment) do
        [signature(
          :progressive_lexical_gap_candidate,
          {subject_det.index, location.index},
          %{subject_determiner: subject_det.index, subject_head: subject.index,
            auxiliary: aux.index, predicate: predicate.index,
            preposition: prep.index, location_determiner: loc_det.index,
            location_head: location.index},
          %{subject_head: [:pos_noun], predicate: [:unresolved_ing_surface],
            preposition: [:pos_preposition], location_head: [:pos_noun]},
          [:contiguous_surface, :attested_nominal_subject,
           :unresolved_progressive_predicate, :attested_preposition,
           :attested_nominal_location]
        )
        |> Map.put(:source_span_sha256,
          :crypto.hash(:sha256, Enum.map_join(segment, " ", & &1.word))
          |> Base.encode16(case: :lower))]
      else
        []
      end
    end)
  end

  # Pronoun + copular progressive + spatial complement is a grammatical
  # hypothesis. DiscourseComposition separately requires a same-session,
  # verified nominal antecedent before giving the pronoun a candidate referent.
  # This signature alone makes NO claim about the speaker's referent or event.
  defp anaphoric_progressive_locatives(nodes) do
    nodes
    |> Enum.chunk_every(6, 1, :discard)
    |> Enum.flat_map(fn [subject, aux, predicate, prep, loc_det, location] = segment ->
      if subject.word == "it" and pos?(subject, "pronoun") and
           aux.word == "is" and
           (pos?(aux, "verb") or pos?(aux, "auxiliary")) and
           form?(predicate, :present_participle) and pos?(predicate, "verb") and
           pos?(prep, "preposition") and
           prep.word in ~w(on in under beside above below into) and
           pos?(loc_det, "determiner") and pos?(location, "noun") and
           valid_surface?(segment) do
        [signature(
          :progressive_anaphoric_locative_candidate,
          {subject.index, location.index},
          %{subject: subject.index, auxiliary: aux.index,
            predicate: predicate.index, preposition: prep.index,
            location_determiner: loc_det.index, location_head: location.index},
          %{subject: [:pos_pronoun], auxiliary: [:pos_auxiliary],
            predicate: [:present_participle], preposition: [:pos_preposition],
            location_determiner: [:pos_determiner], location_head: [:pos_noun]},
          [:contiguous_surface, :attested_subject_pronoun,
           :attested_present_participle, :attested_preposition,
           :attested_nominal_location, :unresolved_anaphoric_subject]
        )]
      else
        []
      end
    end)
  end

  # Attested third-person finite verb + bounded nominal subject/object.
  # Every term must be contiguous in the original sentence, with lexical POS
  # alternatives for each role. A noun plural tagged form-of cannot impersonate
  # the separately attested finite verb form.
  defp finite_present_svo(nodes) do
    Enum.flat_map(@finite_present_patterns, fn {pattern, role_names} ->
      nodes
      |> Enum.chunk_every(length(pattern), 1, :discard)
      |> Enum.flat_map(fn segment ->
        roles = Enum.zip(role_names, segment) |> Map.new(fn {role, node} -> {role, node.index} end)
        predicate = Enum.find(segment, &(&1.index == roles.predicate))
        subject = Enum.find(segment, &(&1.index == roles.subject_head))

        compatible? =
          Enum.zip(segment, pattern)
          |> Enum.all?(fn {node, pos} -> pos?(node, pos) end)

        third_person_subject? =
          if MapSet.member?(subject.pos, "pronoun"),
            do: MapSet.member?(@third_person_singular_pronouns, subject.word),
            else: not subject.plural_noun?

        if compatible? and third_person_subject? and predicate.finite_present_verb? and
             valid_surface?(segment) do
          [
            signature(
              :finite_present_svo_candidate,
              {hd(segment).index, List.last(segment).index},
              roles,
              %{subject_head: [:pos_noun_or_pronoun], predicate: [:pos_verb],
                object_head: [:pos_noun]},
              [:contiguous_surface, :attested_nominal_subject, :attested_nominal_object,
               :attested_finite_present_verb]
            )
          ]
        else
          []
        end
      end)
    end)
  end

  # A complete D-N-V-D-N clause is a finite-past hypothesis only with
  # *attested* simple-past verb morphology. This does not choose a verb
  # sense, canonicalize its lemma, or authorize episodic persistence.
  defp finite_past_svo(nodes) do
    nodes
    |> Enum.chunk_every(5, 1, :discard)
    |> Enum.flat_map(fn [sd, subject, predicate, od, object] = segment ->
      contiguous? = Enum.map(segment, & &1.index) ==
        Enum.to_list(sd.index..(sd.index + 4))

      if contiguous? and
           pos?(sd, "determiner") and pos?(subject, "noun") and
           pos?(predicate, "verb") and form?(predicate, :simple_past) and
           pos?(od, "determiner") and pos?(object, "noun") and
           valid_surface?(segment) do
        [signature(
          :finite_past_svo_candidate,
          {sd.index, object.index},
          %{subject_determiner: sd.index, subject_head: subject.index,
            predicate: predicate.index, object_determiner: od.index,
            object_head: object.index},
          %{subject_head: [:pos_noun], predicate: [:simple_past],
            object_head: [:pos_noun]},
          [:contiguous_surface, :attested_nominal_subject,
           :attested_nominal_object, :attested_simple_past_verb]
        )]
      else
        []
      end
    end)
  end

  # Counted plural SVO: "Four reports support the claims".
  # The number alone is not number agreement. Require original dictionary
  # morphology for the plural noun AND an attested uninflected verb.
  defp counted_present_svo(nodes) do
    nodes
    |> Enum.chunk_every(5, 1, :discard)
    |> Enum.flat_map(fn [count, subject, predicate, det, object] = segment ->
      counted_plural? =
        case Integer.parse(count.word) do
          {n, ""} when n in 2..999 -> true
          _ -> count.word in ~w(two three four five six seven eight nine ten eleven twelve thirteen)
        end

      if counted_plural? and pos?(subject, "noun") and subject.plural_noun? and
           pos?(predicate, "verb") and predicate.base_present_verb? and
           MapSet.member?(@determiners, det.word) and
           pos?(det, "determiner") and pos?(object, "noun") and
           valid_surface?(segment) do
        [signature(
           :counted_finite_present_svo_candidate,
           {count.index, object.index},
           %{count: count.index, subject_head: subject.index,
             predicate: predicate.index, object_determiner: det.index,
             object_head: object.index},
           %{subject_head: [:pos_noun], predicate: [:pos_verb],
             object_head: [:pos_noun]},
           [:contiguous_surface, :attested_counted_plural_subject,
            :attested_base_present_verb, :attested_nominal_object]
         )]
      else
        []
      end
    end)
  end

  # Nominal constituents do not imply that their word sequence is an MWE.
  # Keep POS ambiguity: a token may offer noun and verb analyses.
  defp noun_phrase_constructions(nodes) do
    Enum.flat_map(@noun_patterns, fn {pattern, role_names} ->
      nodes
      |> Enum.chunk_every(length(pattern), 1, :discard)
      |> Enum.flat_map(fn segment ->
        compatible? =
          Enum.zip(segment, pattern)
          |> Enum.all?(fn {node, pos} -> pos?(node, pos) end)

        if compatible? and valid_surface?(segment) do
          roles =
            Enum.zip(role_names, segment)
            |> Map.new(fn {role, node} -> {role, node.index} end)

          required_features =
            Enum.zip(role_names, pattern)
            |> Map.new(fn {role, pos} -> {role, [Map.fetch!(@pos_features, pos)]} end)

          [
            signature(
              :noun_phrase,
              {hd(segment).index, List.last(segment).index},
              roles,
              required_features,
              [:nominal_constituent, :pos_pattern]
            )
          ]
        else
          []
        end
      end)
    end)
  end

  # A genitive suffix is morphological evidence, not a new dictionary sense.
  # "Japan's railgun development" can therefore keep the existing noun
  # constituent and an ungrounded possessor role without inventing "Japan's"
  # as a BrainCell. Do not confuse "it's" or "he's" with possession.
  defp possessive_noun_phrase_constructions(nodes, noun_phrases) do
    nodes
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.flat_map(fn [owner, following] ->
      with base when is_binary(base) <- possessive_base(owner.surface),
           true <- following.index == owner.index + 1,
           true <- pos?(following, "noun") or pos?(following, "adjective"),
           matching <-
             Enum.filter(noun_phrases, fn phrase ->
               {first, last} = phrase.span
               first == following.index and
                 last <= owner.index + @max_possessive_tokens - 1
             end),
           noun_phrase <-
             matching
             |> Enum.sort_by(fn sig -> elem(sig.span, 1) end)
             |> List.last(),
           {roles, required, last} <-
             possessive_roles(following, noun_phrase),
           segment <- Enum.filter(nodes, &(&1.index >= owner.index and &1.index <= last)),
           true <- length(segment) == last - owner.index + 1 and valid_surface?(segment) do
        [
          signature(
            :possessive_noun_phrase,
            {owner.index, last},
            Map.put(roles, :possessor, owner.index),
            Map.put(required, :possessor, [:pos_noun_or_pronoun]),
            [:apostrophe_s, :nominal_head_available, :surface_possessive,
             :possessor_not_lexically_verified]
          )
          |> Map.put(:surface_possessor, base)
        ]
      else
        _ -> []
      end
    end)
  end

  # Prefer the longest locally supported nominal constituent. A single noun
  # head is enough when no multi-token NP exists ("Mary's car").
  defp possessive_roles(following, nil) do
    if pos?(following, "noun") do
      {%{head: following.index}, %{head: [:pos_noun]}, following.index}
    else
      {%{}, %{}, -1}
    end
  end

  defp possessive_roles(_following, noun_phrase) do
    {noun_phrase.roles, noun_phrase.required_features, elem(noun_phrase.span, 1)}
  end

  defp possessive_base(surface) when is_binary(surface) do
    case Regex.run(~r/\A([\p{L}][\p{L}\p{M}-]*)['’]s\z/u, String.trim(surface)) do
      [_, base] ->
        if MapSet.member?(@contraction_bases, String.downcase(base)),
          do: nil,
          else: base

      _ ->
        nil
    end
  end

  defp possessive_base(_), do: nil

  # PP constituents are assembled from a preposition and a following NP.
  # A bare noun or pronoun may also serve as the object of a preposition.
  # Retain the nested NP signature and its head/other roles independently.
  defp prepositional_phrase_constructions(nodes, noun_phrases) do
    by_index = Map.new(nodes, &{&1.index, &1})

    bare =
      nodes
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.flat_map(fn [prep, object] = segment ->
        if pos?(prep, "preposition") and
             (pos?(object, "noun") or pos?(object, "pronoun")) and
             valid_surface?(segment) do
          [
            signature(
              :prepositional_phrase,
              {prep.index, object.index},
              %{preposition: prep.index, object: object.index},
              %{
                preposition: [:pos_preposition],
                object: [:pos_noun_or_pronoun]
              },
              [:preposition, :bare_nominal_complement]
            )
          ]
        else
          []
        end
      end)

    composed =
      Enum.flat_map(noun_phrases, fn np ->
        {first, last} = np.span
        prep = Map.get(by_index, first - 1)

        if prep && pos?(prep, "preposition") do
          segment =
            Enum.filter(nodes, fn node ->
              node.index >= prep.index and node.index <= last
            end)

          if length(segment) == last - prep.index + 1 and valid_surface?(segment) do
            roles =
              np.roles
              |> Map.put(:preposition, prep.index)
              |> Map.put(:object, np.roles.head)

            required_features =
              np.required_features
              |> Map.put(:preposition, [:pos_preposition])
              |> Map.put(:object, [:pos_noun])

            [
              signature(
                :prepositional_phrase,
                {prep.index, last},
                roles,
                required_features,
                [:preposition, :noun_phrase_complement]
              )
            ]
          else
            []
          end
        else
          []
        end
      end)

    bare ++ composed
  end

  # A grammatical verb-particle candidate can be adjacent ("turn off")
  # or discontinuous ("turn the lights off"). This is not a lexical decision:
  # the same surface may also be an ordinary verb + preposition construction.
  # POS alternatives are preserved and no sense ID is assigned here.
  defp verb_particle_constructions(nodes) do
    adjacent =
      nodes
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.flat_map(fn [verb, particle] = segment ->
        if verb_particle?(verb, particle) and valid_surface?(segment) do
          [
            signature(
              :verb_particle_construction,
              {verb.index, particle.index},
              %{verb: verb.index, particle: particle.index},
              %{verb: [:pos_verb], particle: particle_features(particle)},
              [:adjacent_particle]
            )
          ]
        else
          []
        end
      end)

    bare_object =
      nodes
      |> Enum.chunk_every(3, 1, :discard)
      |> Enum.flat_map(fn [verb, object, particle] = segment ->
        if verb_particle?(verb, particle) and
             (pos?(object, "noun") or pos?(object, "pronoun")) and
             valid_surface?(segment) do
          [
            signature(
              :verb_particle_construction,
              {verb.index, particle.index},
              %{verb: verb.index, object: object.index, particle: particle.index},
              %{verb: [:pos_verb], object: [:pos_noun_or_pronoun], particle: particle_features(particle)},
              [:separated_particle, :object_between]
            )
          ]
        else
          []
        end
      end)

    determined_object =
      nodes
      |> Enum.chunk_every(4, 1, :discard)
      |> Enum.flat_map(fn [verb, determiner, noun, particle] = segment ->
        if verb_particle?(verb, particle) and pos?(determiner, "determiner") and
             pos?(noun, "noun") and valid_surface?(segment) do
          [
            signature(
              :verb_particle_construction,
              {verb.index, particle.index},
              %{
                verb: verb.index,
                determiner: determiner.index,
                object: noun.index,
                particle: particle.index
              },
              %{
                verb: [:pos_verb],
                determiner: [:pos_determiner],
                object: [:pos_noun],
                particle: particle_features(particle)
              },
              [:separated_particle, :determiner_noun_object]
            )
          ]
        else
          []
        end
      end)

    adjacent ++ bare_object ++ determined_object
  end

  defp verb_particle?(verb, particle) do
    pos?(verb, "verb") and particle_features(particle) != []
  end

  # Dictionary POS alternatives can mark otherwise ordinary words as
  # "particle". The lexical tag is evidence, but not proof of a verb particle:
  # require a plausible particle surface even for direct particle POS rows.
  defp particle_features(particle) do
    if MapSet.member?(@particle_surfaces, particle.word) do
      (if pos?(particle, "particle"), do: [:pos_particle], else: []) ++
        (if pos?(particle, "adverb"), do: [:pos_adverb], else: []) ++
        (if pos?(particle, "preposition"), do: [:pos_preposition], else: [])
    else
      []
    end
  end

  defp valid_surface?(nodes) do
    nodes
    |> Enum.map(fn node -> %{phrase: node.surface, index: node.index} end)
    |> Boundaries.valid_window?()
  end

  defp signature(id, span, roles, required_features, evidence) do
    %{
      id: id,
      span: span,
      roles: roles,
      required_features: required_features,
      evidence: evidence,
      source: :core_syntax
    }
  end

  defp pos?(%{pos: options}, pos), do: MapSet.member?(options, pos)
  defp form?(%{morphology: options}, kind), do: MapSet.member?(options, kind)

  defp auxiliary_have?(nil), do: false

  defp auxiliary_have?(%{word: word}) do
    word in @have_words or Regex.match?(regex_have_contract(), word) or
      Regex.match?(regex_have_or_be_contract(), word)
  end

  defp ambiguous_contraction?(%{word: word}),
    do: Regex.match?(regex_have_or_be_contract(), word)

  defp get(%{} = map, key), do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
  defp get(_, _), do: nil

  defp norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.replace("’", "'")
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp norm(_), do: ""
end
