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
  @have_contract ~r/^(?:i|you|we|they)'ve$/u
  @have_or_be_contract ~r/^(?:he|she|it|that|there|who)'s$/u

  @type signature :: Core.SemanticInput.sentence_signature()

  @doc "Detect grammatical constructions without modifying the input."
  @spec detect(map()) :: [signature()]
  def detect(%{} = si) do
    cells = Map.get(si, :active_cells, [])
    lexicon = build_lexicon(cells)
    nodes = base_nodes(Map.get(si, :tokens, []), lexicon)

    noun_phrases = noun_phrase_constructions(nodes)
    possessive_phrases = possessive_noun_phrase_constructions(nodes, noun_phrases)

    (wh_aux_questions(si, nodes) ++ finite_present_svo(nodes) ++
       finite_past(nodes) ++ perfect_predicates(nodes) ++
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
          finite_present_verb?: pos == "verb" and finite_present_verb?(get(cell, :semantic_atoms))
        }

        Map.update(acc, word, candidate, fn existing ->
          %{
            pos: MapSet.union(existing.pos, candidate.pos),
            morphology: MapSet.union(existing.morphology, candidate.morphology),
            finite_present_verb?: existing.finite_present_verb? or candidate.finite_present_verb?
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
            finite_present_verb?: false
          })
        [
          %{
            index: index,
            word: word,
            surface: get(token, :phrase),
            pos: entry.pos,
            morphology: entry.morphology,
            finite_present_verb?: entry.finite_present_verb?
          }
        ]
      end
    end)
  end

  defp base_nodes(_, _), do: []

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
            else: true

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
    word in @have_words or Regex.match?(@have_contract, word) or
      Regex.match?(@have_or_be_contract, word)
  end

  defp ambiguous_contraction?(%{word: word}),
    do: Regex.match?(@have_or_be_contract, word)

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
