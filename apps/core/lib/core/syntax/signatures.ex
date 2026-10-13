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

    (finite_past(nodes) ++ perfect_predicates(nodes) ++ have_verb_infinitives(nodes) ++
       verb_particle_constructions(nodes) ++ noun_phrases ++
       prepositional_phrase_constructions(nodes, noun_phrases))
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
          morphology: morphology(get(cell, :semantic_atoms))
        }

        Map.update(acc, word, candidate, fn existing ->
          %{
            pos: MapSet.union(existing.pos, candidate.pos),
            morphology: MapSet.union(existing.morphology, candidate.morphology)
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

        entry = Map.get(lexicon, word, %{pos: MapSet.new(), morphology: MapSet.new()})
        [
          %{
            index: index,
            word: word,
            surface: get(token, :phrase),
            pos: entry.pos,
            morphology: entry.morphology
          }
        ]
      end
    end)
  end

  defp base_nodes(_, _), do: []

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

  defp particle_features(particle) do
    direct = if pos?(particle, "particle"), do: [:pos_particle], else: []

    if MapSet.member?(@particle_surfaces, particle.word) do
      direct ++
        (if pos?(particle, "adverb"), do: [:pos_adverb], else: []) ++
        (if pos?(particle, "preposition"), do: [:pos_preposition], else: [])
    else
      direct
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
