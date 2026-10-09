defmodule Brain.LIFG.ClosedClass do
  @moduledoc """
  Closed-class lexical defaults and candidate construction for LIFG Stage1.

  Owns deterministic function-word, pronoun, identity-name, and entity fallback
  candidates plus the matching and POS-family rules used to maintain those candidates.
  """

  alias Brain.Utils.Safe

  @default_weights %{
    lex_fit: 0.40,
    context_fit: 0.00,
    rel_prior: 0.30,
    activation: 0.20,
    intent_bias: 0.10
  }

  @closed_class_pronouns MapSet.new(~w(
                           i me you he him she her it we us they them
                           who whom whose myself yourself himself herself
                           itself ourselves yourselves themselves
                         ))

  @closed_class_defaults %{
    "my" => %{
      pos: "determiner",
      rel_prior: 1.0,
      activation: 0.97,
      definition: "Possessive determiner meaning belonging to or associated with the speaker.",
      example: "I forgot my medication."
    },
    "your" => %{
      pos: "determiner",
      rel_prior: 1.0,
      activation: 0.97,
      definition:
        "Possessive determiner meaning belonging to or associated with the person addressed."
    },
    "his" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "her" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "our" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "their" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "and" => %{pos: "conjunction", rel_prior: 1.0, activation: 0.96},
    "or" => %{pos: "conjunction", rel_prior: 1.0, activation: 0.96},
    "but" => %{pos: "conjunction", rel_prior: 1.0, activation: 0.96},
    "nor" => %{pos: "conjunction", rel_prior: 1.0, activation: 0.96},
    "so" => %{pos: "conjunction", rel_prior: 0.98, activation: 0.93},
    "yet" => %{pos: "conjunction", rel_prior: 0.98, activation: 0.93},
    "really" => %{pos: "adverb", rel_prior: 1.0, activation: 0.96},
    "very" => %{pos: "adverb", rel_prior: 1.0, activation: 0.96},
    "quite" => %{pos: "adverb", rel_prior: 0.98, activation: 0.93},
    "some" => %{pos: "determiner", rel_prior: 0.99, activation: 0.94},
    "the" => %{pos: "determiner", rel_prior: 0.99, activation: 0.94},
    "a" => %{pos: "determiner", rel_prior: 0.99, activation: 0.94},
    "an" => %{pos: "determiner", rel_prior: 0.99, activation: 0.94},
    "this" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "that" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "these" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "those" => %{pos: "determiner", rel_prior: 0.99, activation: 0.95},
    "hey" => %{pos: "interjection", rel_prior: 1.0, activation: 0.96},
    "hi" => %{pos: "interjection", rel_prior: 1.0, activation: 0.96},
    "hello" => %{pos: "interjection", rel_prior: 1.0, activation: 0.96},
    "how" => %{
      pos: "adverb",
      rel_prior: 1.0,
      activation: 0.96,
      definition: "Interrogative adverb asking about manner, method, or condition.",
      example: "How do I stimulate you?"
    },
    "what" => %{pos: "pronoun", rel_prior: 0.99, activation: 0.95},
    "who" => %{pos: "pronoun", rel_prior: 0.99, activation: 0.95},
    "whom" => %{pos: "pronoun", rel_prior: 0.99, activation: 0.95},
    "which" => %{pos: "determiner", rel_prior: 0.98, activation: 0.94},
    "any" => %{
      pos: "determiner",
      rel_prior: 0.99,
      activation: 0.95,
      definition: "Determiner used before a noun to indicate an indefinite amount or choice.",
      example: "Any ideas?"
    },
    "do" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "does" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "did" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "can" => %{
      pos: "auxiliary",
      rel_prior: 1.0,
      activation: 0.96,
      definition: "Modal auxiliary marking ability, possibility, or permission.",
      example: "I can sleep."
    },
    "cannot" => %{
      pos: "auxiliary",
      rel_prior: 1.0,
      activation: 0.97,
      definition: "Negative modal auxiliary meaning can not; marks inability or impossibility.",
      example: "I cannot sleep."
    },
    "can't" => %{
      pos: "auxiliary",
      rel_prior: 1.0,
      activation: 0.97,
      definition: "Contraction of cannot; negative modal auxiliary marking inability.",
      example: "I can't sleep."
    },
    "am" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "is" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "are" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "was" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "were" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "be" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "being" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "been" => %{
      pos: "auxiliary",
      rel_prior: 1.0,
      activation: 0.96,
      definition: "Past participle of be used as an auxiliary in verb phrases."
    },
    "have" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "has" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "had" => %{pos: "auxiliary", rel_prior: 0.98, activation: 0.94},
    "now" => %{
      pos: "adverb",
      rel_prior: 1.0,
      activation: 0.96,
      definition: "Temporal adverb meaning at the present time.",
      example: "Now I cannot sleep."
    },
    "not" => %{
      pos: "particle",
      rel_prior: 1.0,
      activation: 0.97,
      definition: "Negation particle marking that a proposition or verb phrase is negative.",
      example: "I can not sleep."
    },
    "in" => %{pos: "preposition", rel_prior: 1.0, activation: 0.96},
    "about" => %{pos: "preposition", rel_prior: 1.0, activation: 0.96},
    "of" => %{pos: "preposition", rel_prior: 1.0, activation: 0.96},
    "to" => %{pos: "preposition", rel_prior: 0.98, activation: 0.94},
    "for" => %{pos: "preposition", rel_prior: 0.98, activation: 0.94},
    "with" => %{pos: "preposition", rel_prior: 0.98, activation: 0.94},
    "on" => %{pos: "preposition", rel_prior: 0.98, activation: 0.94},
    "at" => %{pos: "preposition", rel_prior: 0.98, activation: 0.94},
    "from" => %{pos: "preposition", rel_prior: 0.98, activation: 0.94}
  }

  @closed_class_pos_aliases %{
    "conjunction" => ["conjunction", "conj", "connector", "cc"],
    "adverb" => ["adverb", "adv", "intensifier"],
    "determiner" => ["determiner", "det", "article", "possessive", "possessive determiner"],
    "pronoun" => ["pronoun", "pron"],
    "interjection" => ["interjection", "interj", "greeting"],
    "preposition" => ["preposition", "prep", "adposition"],
    "auxiliary" => ["auxiliary", "aux", "modal"],
    "particle" => ["particle", "negation", "neg", "negative"]
  }

  @entity_defaults %{
    "quetiapine" => %{
      pos: "entity",
      entity_type: :medication,
      activation: 0.95,
      source: :medical_entity_fallback,
      definition: "Medication or drug name mentioned by the user.",
      example: "I forgot my quetiapine."
    },
    "seroquel" => %{
      pos: "entity",
      entity_type: :medication,
      activation: 0.95,
      source: :medical_entity_fallback,
      definition: "Medication or drug brand name mentioned by the user.",
      example: "I forgot my Seroquel."
    }
  }

  @greeting_lemmas MapSet.new([
                     "hey how is",
                     "how are you",
                     "how is life treating you",
                     "how is life treating"
                   ])

  # Default event for MWE fallback telemetry (tests attach to this)

  def ensure_closed_class_candidates(%{} = si, opts) do
    tokens = Safe.get(si, :tokens, []) || []

    sc0 =
      case Safe.get(si, :sense_candidates, %{}) do
        %{} = m -> m
        _ -> %{}
      end

    caller_supplied_partial_slate? = map_size(sc0) > 0
    activation_only? = activation_only_scoring?(opts)

    sc =
      tokens
      |> Enum.with_index()
      |> Enum.reduce(sc0, fn {tok, fallback_idx}, acc ->
        idx = token_index(tok, fallback_idx)
        phrase = tok |> token_raw_phrase() |> norm()

        cond do
          token_mwe?(tok) ->
            acc

          explicit_aligned_candidates?(acc, idx, phrase) ->
            acc

          activation_only? and sense_candidates_for_idx(acc, idx) != [] ->
            acc

          MapSet.member?(@closed_class_pronouns, phrase) ->
            upsert_closed_class_candidate(acc, idx, closed_class_pronoun_candidate(phrase))

          Map.has_key?(@closed_class_defaults, phrase) ->
            upsert_closed_class_candidate(acc, idx, closed_class_default_candidate(phrase))

          Map.has_key?(@entity_defaults, phrase) ->
            upsert_entity_candidate(acc, idx, entity_default_candidate(phrase))

          caller_supplied_partial_slate? ->
            acc

          self_name?(phrase) and identity_candidate_present?(acc, idx, phrase) ->
            acc

          self_name?(phrase) ->
            upsert_self_name_candidate(acc, idx, self_name_candidate(phrase))

          true ->
            acc
        end
      end)

    Map.put(si, :sense_candidates, sc)
  end

  def ensure_closed_class_candidates(other, _opts), do: other

  def activation_only_scoring?(opts) when is_list(opts) do
    weights =
      Application.get_env(:brain, :lifg_stage1_weights, @default_weights)
      |> Map.merge(Map.new(Keyword.get(opts, :weights, [])))

    get_num(weights, :activation, 0.0) == 1.0 and
      get_num(weights, :lex_fit, 0.0) == 0.0 and
      get_num(weights, :rel_prior, 0.0) == 0.0 and
      get_num(weights, :intent_bias, 0.0) == 0.0
  end

  def activation_only_scoring?(_opts), do: false

  def self_name?(phrase) when is_binary(phrase) do
    phrase in self_names()
  end

  def self_name?(_), do: false

  def self_names do
    :brain
    |> Application.get_env(:self_names, [])
    |> List.wrap()
    |> Enum.map(&norm/1)
    |> Enum.reject(&(&1 == ""))
  end

  def self_name_candidate(phrase) do
    %{
      id: "#{phrase}|proper_noun|self",
      lemma: phrase,
      norm: phrase,
      word: phrase,
      pos: "proper_noun",
      source: :self_name,
      activation: 0.98,
      score: 0.98,
      rel_prior: 1.0,
      definition: "Configured assistant self-name.",
      example: "Hello #{phrase}."
    }
  end

  def identity_candidate_present?(sc, idx, phrase) when is_map(sc) do
    sc
    |> sense_candidates_for_idx(idx)
    |> Enum.any?(fn cand ->
      cand = Safe.to_plain(cand)
      source = Safe.get(cand, :source) || Safe.get(cand, "source")
      from = Safe.get(cand, :from) || Safe.get(cand, "from")
      id = Safe.get(cand, :id) || Safe.get(cand, "id")
      pos = pos_of(cand)

      candidate_aligned_to_phrase?(cand, phrase) and
        (source in [:self_name, "self_name"] or
           from in [:assistant_identity, "assistant_identity"] or
           pos in ["assistant", "self_name"] or
           String.contains?(to_string(id || ""), "|assistant|"))
    end)
  end

  def identity_candidate_present?(_sc, _idx, _phrase), do: false

  def explicit_aligned_candidates?(sc, idx, phrase) when is_map(sc) do
    sc
    |> sense_candidates_for_idx(idx)
    |> Enum.any?(fn cand ->
      cand = Safe.to_plain(cand)
      source = Safe.get(cand, :source) || Safe.get(cand, "source")
      id = Safe.get(cand, :id) || Safe.get(cand, "id")

      source not in [:closed_class, "closed_class", :entity_default, "entity_default"] and
        external_sense_id?(id) and
        candidate_aligned_to_phrase?(cand, phrase)
    end)
  end

  def explicit_aligned_candidates?(_sc, _idx, _phrase), do: false

  def sense_candidates_for_idx(sc, idx) when is_map(sc) do
    sc
    |> Map.get(idx, Map.get(sc, to_string(idx), []))
    |> List.wrap()
  end

  def candidate_aligned_to_phrase?(cand, phrase) when is_map(cand) and is_binary(phrase) do
    id = Safe.get(cand, :id) || Safe.get(cand, "id")

    lemma =
      Safe.get(cand, :lemma) ||
        Safe.get(cand, "lemma") ||
        Safe.get(cand, :norm) ||
        Safe.get(cand, "norm") ||
        guess_cell_lemma(id)

    norm(to_string(lemma || "")) == phrase
  end

  def candidate_aligned_to_phrase?(_cand, _phrase), do: false

  def external_sense_id?(id) when is_binary(id), do: not String.contains?(id, "|")

  def external_sense_id?(_id), do: false

  def upsert_entity_candidate(sc, idx, cand) when is_map(sc) do
    key =
      cond do
        Map.has_key?(sc, idx) -> idx
        Map.has_key?(sc, to_string(idx)) -> to_string(idx)
        true -> idx
      end

    Map.update(sc, key, [cand], fn
      list when is_list(list) ->
        if Enum.any?(list, &same_sense_id?(&1, cand)), do: list, else: list ++ [cand]

      %{} = existing ->
        if same_sense_id?(existing, cand), do: existing, else: [existing, cand]

      other ->
        [other, cand]
    end)
  end

  def upsert_self_name_candidate(sc, idx, cand) when is_map(sc) do
    upsert_entity_candidate(sc, idx, cand)
  end

  def same_sense_id?(existing, override) when is_map(existing) and is_map(override) do
    (Safe.get(existing, :id) || Safe.get(existing, "id")) ==
      (Safe.get(override, :id) || Safe.get(override, "id"))
  end

  def same_sense_id?(_existing, _override), do: false

  def upsert_closed_class_candidate(sc, idx, cand) when is_map(sc) do
    key =
      cond do
        Map.has_key?(sc, idx) -> idx
        Map.has_key?(sc, to_string(idx)) -> to_string(idx)
        true -> idx
      end

    Map.update(sc, key, [cand], fn
      list when is_list(list) ->
        cond do
          Enum.any?(list, &same_closed_class_candidate?(&1, cand)) ->
            Enum.map(list, fn
              %{} = existing ->
                if same_closed_class_candidate?(existing, cand) do
                  upgrade_closed_class_candidate(existing, cand)
                else
                  existing
                end

              existing ->
                existing
            end)

          Enum.any?(list, &same_closed_class_family?(&1, cand)) ->
            list

          true ->
            list ++ [cand]
        end

      %{} = existing ->
        cond do
          same_closed_class_candidate?(existing, cand) ->
            upgrade_closed_class_candidate(existing, cand)

          same_closed_class_family?(existing, cand) ->
            existing

          true ->
            [existing, cand]
        end

      other ->
        [other, cand]
    end)
  end

  def upgrade_closed_class_candidate(existing, override) do
    features =
      (Safe.get(existing, :features) || Safe.get(existing, "features") || %{})
      |> Map.merge(Safe.get(override, :features, %{}))

    existing
    |> Map.put(
      :id,
      Safe.get(override, :id) || Safe.get(override, "id") || Safe.get(existing, :id)
    )
    |> Map.put(
      :lemma,
      Safe.get(override, :lemma) || Safe.get(override, "lemma") || Safe.get(existing, :lemma)
    )
    |> Map.put(
      :norm,
      Safe.get(override, :norm) || Safe.get(override, "norm") || Safe.get(existing, :norm)
    )
    |> Map.put(
      :pos,
      Safe.get(override, :pos) || Safe.get(override, "pos") || Safe.get(existing, :pos)
    )
    |> maybe_put_closed_class_field(:definition, override)
    |> maybe_put_closed_class_field(:example, override)
    |> Map.put(:features, features)
    |> Map.put(:activation, Safe.get(override, :activation, 0.95))
    |> Map.put(:score, Safe.get(override, :score, 0.95))
    |> Map.put(:source, :closed_class)
  end

  def maybe_put_closed_class_field(existing, key, override) do
    case Safe.get(override, key) || Safe.get(override, Atom.to_string(key)) do
      value when is_binary(value) and value != "" -> Map.put(existing, key, value)
      _ -> existing
    end
  end

  def same_closed_class_candidate?(existing, override)
       when is_map(existing) and is_map(override) do
    existing_lemma =
      norm(
        Safe.get(existing, :norm) || Safe.get(existing, :lemma) || Safe.get(existing, "norm") ||
          Safe.get(existing, "lemma") ||
          phrase_from_id(Safe.get(existing, :id) || Safe.get(existing, "id"))
      )

    override_lemma =
      norm(
        Safe.get(override, :norm) || Safe.get(override, :lemma) || Safe.get(override, "norm") ||
          Safe.get(override, "lemma") ||
          phrase_from_id(Safe.get(override, :id) || Safe.get(override, "id"))
      )

    existing_lemma == override_lemma and
      closed_class_pos_family(pos_of(existing)) == closed_class_pos_family(pos_of(override)) and
      same_closed_class_tag_family?(existing, override)
  end

  def same_closed_class_candidate?(_existing, _override), do: false

  def same_closed_class_family?(existing, override) when is_map(existing) and is_map(override) do
    existing_lemma =
      norm(
        Safe.get(existing, :norm) || Safe.get(existing, :lemma) || Safe.get(existing, "norm") ||
          Safe.get(existing, "lemma") ||
          phrase_from_id(Safe.get(existing, :id) || Safe.get(existing, "id"))
      )

    override_lemma =
      norm(
        Safe.get(override, :norm) || Safe.get(override, :lemma) || Safe.get(override, "norm") ||
          Safe.get(override, "lemma") ||
          phrase_from_id(Safe.get(override, :id) || Safe.get(override, "id"))
      )

    existing_lemma == override_lemma and
      closed_class_pos_family(pos_of(existing)) == closed_class_pos_family(pos_of(override))
  end

  def same_closed_class_family?(_existing, _override), do: false

  def same_closed_class_tag_family?(existing, override) do
    {_existing_lemma, _existing_pos, existing_tag} =      parse_sense_id(to_string(Safe.get(existing, :id) || Safe.get(existing, "id") || ""))

    {_override_lemma, _override_pos, override_tag} =
      parse_sense_id(to_string(Safe.get(override, :id) || Safe.get(override, "id") || ""))

    collapseable_sense_tag(existing_tag) == collapseable_sense_tag(override_tag)
  end

  def closed_class_pronoun_candidate(phrase) do
    %{
      id: "#{phrase}|pronoun|0",
      lemma: phrase,
      norm: phrase,
      mw: false,
      pos: "pronoun",
      activation: 0.20,
      score: 0.20,
      source: :closed_class,
      definition: pronoun_definition(phrase),
      example: pronoun_example(phrase),
      features: %{
        lex_fit: 1.0,
        rel_prior: 1.0,
        activation: 0.20,
        intent_bias: 0.0
      }
    }
  end

  def closed_class_default_candidate(phrase) do
    spec = Map.fetch!(@closed_class_defaults, phrase)
    pos = spec.pos

    %{
      id: "#{phrase}|#{pos}|0",
      lemma: phrase,
      norm: phrase,
      mw: false,
      pos: pos,
      activation: 0.20,
      score: 0.20,
      source: :closed_class,
      definition: Map.get(spec, :definition) || closed_class_definition(phrase, pos),
      example: Map.get(spec, :example) || closed_class_example(phrase, pos),
      features: %{
        lex_fit: 1.0,
        rel_prior: spec.rel_prior,
        activation: 0.20,
        intent_bias: 0.0
      }
    }
  end

  def entity_default_candidate(phrase) do
    spec = Map.fetch!(@entity_defaults, phrase)
    pos = spec.pos

    %{
      id: "#{phrase}|#{pos}|#{spec.entity_type}",
      lemma: phrase,
      norm: phrase,
      mw: false,
      pos: pos,
      entity_type: spec.entity_type,
      activation: spec.activation,
      score: spec.activation,
      source: spec.source,
      definition: spec.definition,
      example: spec.example,
      features: %{
        lex_fit: 1.0,
        rel_prior: 1.0,
        activation: spec.activation,
        intent_bias: 0.0
      }
    }
  end

  def pronoun_definition("i"),
    do: "First-person singular pronoun; speaker self-reference."

  def pronoun_definition("me"),
    do: "First-person singular pronoun referring to the speaker as object."

  def pronoun_definition("you"),
    do: "Second-person pronoun referring to the person or people being addressed."

  def pronoun_definition(_phrase),
    do: "Pronoun used as a grammatical participant in the utterance."

  def pronoun_example("i"), do: "I forgot my medication."

  def pronoun_example("me"), do: "This affects me."

  def pronoun_example("you"), do: "You can ask a pharmacist."

  def pronoun_example(_phrase), do: ""

  def closed_class_definition("and", _pos),
    do: "Coordinating conjunction linking clauses, phrases, or items."

  def closed_class_definition("or", _pos), do: "Coordinating conjunction marking an alternative."

  def closed_class_definition("but", _pos), do: "Coordinating conjunction marking contrast."

  def closed_class_definition("now", _pos),
    do: "Temporal adverb meaning at the present time."

  def closed_class_definition("not", _pos),
    do: "Negation particle marking that a proposition or verb phrase is negative."

  def closed_class_definition(_phrase, "preposition"),
    do: "Function word marking a grammatical relation."

  def closed_class_definition(_phrase, "auxiliary"),
    do: "Auxiliary verb supporting tense, question, negation, or verb phrase structure."

  def closed_class_definition(_phrase, "determiner"),
    do: "Determiner specifying reference for a noun phrase."

  def closed_class_definition(_phrase, "adverb"),
    do: "Adverb modifying a verb, adjective, or clause."

  def closed_class_definition(_phrase, "particle"),
    do: "Particle marking grammatical meaning such as negation."

  def closed_class_definition(_phrase, "interjection"), do: "Social interjection or greeting."

  def closed_class_definition(_phrase, _pos), do: "Closed-class grammatical function word."

  def closed_class_example("and", _pos),
    do: "I forgot my medication and I cannot sleep."

  def closed_class_example(_phrase, _pos), do: ""

  def closed_class_candidate?(cand) when is_map(cand) do
    pos = cand |> pos_of() |> String.downcase()
    id = cand |> sense_id_for("") |> to_string()

    pos in [
      "pronoun",
      "pron",
      "determiner",
      "det",
      "possessive",
      "conjunction",
      "conj",
      "connector",
      "cc",
      "adverb",
      "adv",
      "intensifier",
      "interjection",
      "interj",
      "greeting",
      "preposition",
      "prep",
      "adposition",
      "auxiliary",
      "aux",
      "modal",
      "particle",
      "negation",
      "neg",
      "negative"
    ] or
      String.contains?(id, "|pronoun|") or
      String.contains?(id, "|pron|") or
      String.contains?(id, "|determiner|") or
      String.contains?(id, "|det|") or
      String.contains?(id, "|possessive|") or
      String.contains?(id, "|conjunction|") or
      String.contains?(id, "|conj|") or
      String.contains?(id, "|connector|") or
      String.contains?(id, "|adverb|") or
      String.contains?(id, "|adv|") or
      String.contains?(id, "|intensifier|") or
      String.contains?(id, "|interjection|") or
      String.contains?(id, "|interj|") or
      String.contains?(id, "|greeting|") or
      String.contains?(id, "|preposition|") or
      String.contains?(id, "|prep|") or
      String.contains?(id, "|adposition|") or
      String.contains?(id, "|auxiliary|") or
      String.contains?(id, "|aux|") or
      String.contains?(id, "|modal|") or
      String.contains?(id, "|particle|") or
      String.contains?(id, "|negation|") or
      String.contains?(id, "|neg|")
  end

  def closed_class_candidate?(_), do: false

  def closed_class_pos_family(pos) do
    p = pos |> to_string() |> String.downcase()

    Enum.find_value(@closed_class_pos_aliases, p, fn {family, aliases} ->
      if p in aliases, do: family, else: nil
    end)
  end

  def closed_class_pos_bias(token_phrase, pos) do
    cond do
      Map.has_key?(@closed_class_defaults, token_phrase) ->
        %{pos: expected_pos} = Map.get(@closed_class_defaults, token_phrase)

        if closed_class_pos_family(pos) == closed_class_pos_family(expected_pos) do
          0.12
        else
          -0.45
        end

      MapSet.member?(@closed_class_pronouns, token_phrase) ->
        if closed_class_pos_family(pos) == closed_class_pos_family("pronoun") do
          0.12
        else
          -0.45
        end

      true ->
        0.0
    end
  end

  def pos_anomaly?(token_phrase, chosen_id) do
    case Map.get(@closed_class_defaults, token_phrase) do
      %{pos: expected_pos} ->
        {_lemma, chosen_pos, _tag} = parse_sense_id(to_string(chosen_id || ""))
        closed_class_pos_family(chosen_pos) != closed_class_pos_family(expected_pos)

      _ ->
        false
    end
  end

  def greeting_lemma?(lemma), do: MapSet.member?(@greeting_lemmas, lemma)

  defp token_index(tok, fallback_idx) do
    raw =
      Safe.get(tok, :token_index) ||
        Safe.get(tok, "token_index") ||
        Safe.get(tok, :index) ||
        Safe.get(tok, "index")

    idx =
      cond do
        is_integer(raw) ->
          raw

        is_float(raw) ->
          trunc(raw)

        is_binary(raw) ->
          case Integer.parse(String.trim(raw)) do
            {n, _} -> n
            :error -> fallback_idx
          end

        true ->
          fallback_idx
      end

    if is_integer(idx) and idx >= 0, do: idx, else: fallback_idx
  end

  defp token_raw_phrase(tok) do
    Safe.get(tok, :phrase) ||
      Safe.get(tok, "phrase") ||
      Safe.get(tok, :lemma) ||
      Safe.get(tok, "lemma") ||
      Safe.get(tok, :word) ||
      Safe.get(tok, "word") ||
      phrase_from_id(Safe.get(tok, :id) || Safe.get(tok, "id")) ||
      ""
  end

  defp phrase_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [ph, _rest] -> ph
      _ -> nil
    end
  end

  defp phrase_from_id(_), do: nil

  defp token_mwe?(tok) do
    n_val = Safe.get(tok, :n) || Safe.get(tok, "n") || 1
    has_mw_flag = Safe.get(tok, :mw, false) || Safe.get(tok, "mw", false)
    id = to_string(Safe.get(tok, :id) || Safe.get(tok, "id") || "")
    has_mw_flag || (is_integer(n_val) and n_val > 1) || String.contains?(id, "|phrase|")
  end

  defp guess_cell_lemma(id) when is_binary(id) do
    id = String.trim(id)

    cond do
      id == "" ->
        ""

      String.contains?(id, "|") ->
        case String.split(id, "|", parts: 2) do
          [lemma, _rest] -> lemma
          _ -> id
        end

      String.contains?(id, "/") ->
        case String.split(id, "/", parts: 2) do
          [lemma, _rest] -> lemma
          _ -> id
        end

      true ->
        id
    end
  end

  defp guess_cell_lemma(id), do: to_string(id)

  # ---------- Boundary helpers ----------

  defp pos_from_id(id) when is_binary(id) do
    case String.split(id, "|") do
      [_lemma, pos | _] -> pos
      _ -> nil
    end
  end

  defp pos_from_id(_), do: nil

  defp pos_of(c) do
    p =
      Safe.get(c, :pos) ||
        Safe.get(c, "pos") ||
        pos_from_id(Safe.get(c, :id) || Safe.get(c, "id")) ||
        "other"

    p |> to_string() |> String.downcase()
  end

  defp sense_id_for(c, token_phrase) do
    id = Safe.get(c, :id) || Safe.get(c, "id")

    if is_nil(id) do
      lemma = Safe.get(c, :lemma) || Safe.get(c, :word) || token_phrase
      pos = pos_of(c)
      "#{lemma}|#{pos}|0"
    else
      to_string(id)
    end
  end

  defp collapseable_sense_tag("fallback"), do: :fallback

  defp collapseable_sense_tag(tag) when is_binary(tag) do
    case Integer.parse(tag) do
      {n, ""} -> {:numbered_sense, n}
      _ -> {:explicit_tag, tag}
    end
  end

  defp collapseable_sense_tag(tag), do: {:explicit_tag, tag}

  defp parse_sense_id(id_str) when is_binary(id_str) do
    case String.split(id_str, "|") do
      [lemma, pos, tag] -> {lemma, String.downcase(pos || "other"), String.downcase(tag || "")}
      [lemma, pos] -> {lemma, String.downcase(pos || "other"), ""}
      [lemma] -> {lemma, "other", ""}
      _ -> {id_str, "other", ""}
    end
  end

  defp parse_sense_id(other), do: parse_sense_id(to_string(other))

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(v), do: v |> to_string() |> norm()

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v) -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v) -> v
      _ -> default * 1.0
    end
  end
end
