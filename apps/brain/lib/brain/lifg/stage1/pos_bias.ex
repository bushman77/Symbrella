defmodule Brain.LIFG.Stage1.PosBias do
  @moduledoc """
  POS biasing for Stage1 scoring.

  Computes positional/syntactic bias adjustments:
    - Local context bias (previous choice → current candidate family)
    - Token window bias (fine-day patterns, greeting chains)
  """

  alias Brain.Utils.Safe

  # Common copular and change-of-state verb forms.
  # This is grammatical vocabulary, not a sentence-specific exception.
  @linking_verb_forms MapSet.new(~w(
    am is are was were be been being
    become becomes became
    get gets got gotten
    feel feels felt
    seem seems seemed
    look looks looked
    sound sounds sounded
    grow grows grew grown
    turn turns turned
    remain remains remained
    stay stays stayed
  ))

  # Grammatical function words, not domain-specific command vocabulary.
  # Detect a predicate slot only; individual verb *senses* still compete.
  @predicate_auxiliaries MapSet.new(~w(
    can could may might must shall should will would do does did
  ))
  @subject_pronouns MapSet.new(~w(i you we they he she it))
  @subject_determiners MapSet.new(~w(
    a an the this that these those my your his her its our their
  ))
  @predicate_modifiers MapSet.new(~w(please not))
  @question_prefixes MapSet.new(~w(what when where why how which who))
  @imperative_followers MapSet.new(~w(
    a an the some any this that these those my your his her its our their
    me him her us them it
  ))

  # ===== Public API =====

  def local_context(acc, tok, pos) when is_map(acc) and is_map(tok) do
    current_start = token_start(tok)
    candidate_family = syntactic_pos_family(pos)

    acc
    |> Map.get(:choices, [])
    |> nearest_left_choice(current_start)
    |> previous_choice_pos_family()
    |> context_bias_for(candidate_family)
  end

  def local_context(_acc, _tok, _pos), do: 0.0

  def window(ctx, tok, pos) when is_map(ctx) and is_map(tok) do
    token_phrase = tok |> token_raw_phrase() |> norm()
    family = syntactic_pos_family(pos)

    # Core role adjudication is authoritative for predicate POS support.
    # Preserve the old local heuristics only for standalone Stage1 calls that
    # lack such evidence, rather than counting the same pattern twice.
    predicate_fallback =
      if supported_predicate_role?(ctx, tok) do
        0.0
      else
        auxiliary_predicate_bias(ctx, tok, family) +
          imperative_predicate_bias(ctx, tok, family)
      end

    fine_day_pos_bias(token_phrase, next_token_phrase(ctx, tok), family) +
      greeting_chain_pos_bias(ctx, tok, token_phrase, family) +
      infinitive_object_bias(ctx, tok, family) +
      predicate_fallback +
      coordinated_predicate_bias(ctx, tok, token_phrase, family) +
      predicate_adjective_bias(ctx, tok, family)
  end

  def window(_ctx, _tok, _pos), do: 0.0

  defp supported_predicate_role?(ctx, tok) do
    idx = token_index(tok, -1)
    case get_in(ctx, [:si, :role_competition]) do
      %{source: :core_syntax_and_attested_dictionary_pos, roles: roles}
      when is_map(roles) ->
        case Map.get(roles, idx) do
          %{role: :predicate, pos: "verb", status: :supported,
            basis: basis, evidence: evidence}
          when basis in [:modal_question_predicate, :modal_declarative_predicate,
                         :imperative_predicate, :plural_present_predicate,
                         :finite_present_svo_predicate] and is_list(evidence) ->
            :dictionary_pos_witness in evidence and
              :bounded_predicate_slot in evidence

          _ -> false
        end

      _ -> false
    end
  end

  # Detect high-confidence infinitive constructions:
  # "to get some cash", "to buy a car", etc.
  #
  # Avoid changing ordinary prepositional constructions,
  # such as "to the bank" or "to school".

  defp infinitive_object_bias(ctx, tok, family) do
    phrases = token_phrases(ctx)
    index = token_index(tok, -1)

    previous = Map.get(phrases, index - 1)
    following = Map.get(phrases, index + 1)

    object_determiner? =
      following in ~w(
      a an the some any this that these those
      my your his her its our their
    )

    if previous == "to" and object_determiner? do
      case family do
        :verb -> 0.30
        :noun -> -0.15
        :proper_noun -> -0.15
        _ -> 0.0
      end
    else
      0.0
    end
  end

  # An inverted auxiliary + subject requires a lexical predicate even
  # when the word (e.g. "write", "book", "draw") also has noun senses.
  # Also cover subject + auxiliary + predicate and polite interruptions.
  # This is POS evidence only: never assert a particular dictionary sense.
  defp auxiliary_predicate_bias(ctx, tok, family) do
    phrases = token_phrases(ctx)
    idx = token_index(tok, -1)
    core_idx = skip_predicate_modifiers(phrases, idx)
    predicate? =
      inverted_subject?(phrases, core_idx) or
        inverted_determined_subject?(phrases, core_idx) or
        declarative_subject?(phrases, core_idx) or
        declarative_determined_subject?(phrases, core_idx)

    if predicate? do
      case family do
        :verb -> 0.34
        :noun -> -0.14
        :proper_noun -> -0.14
        _ -> 0.0
      end
    else
      0.0
    end
  end

  # "Write a note." / "Please write me a note." Strong local imperative
  # context, but no boost for isolated words or "the write ...".
  defp imperative_predicate_bias(ctx, tok, family) do
    phrases = token_phrases(ctx)
    idx = token_index(tok, -1)
    imperative? =
      (idx == 0 or (idx == 1 and Map.get(phrases, 0) == "please")) and
        MapSet.member?(@imperative_followers, Map.get(phrases, idx + 1))

    if imperative? do
      case family do
        :verb -> 0.30
        :noun -> -0.12
        :proper_noun -> -0.12
        _ -> 0.0
      end
    else
      0.0
    end
  end

  # Only skip up to two immediately preceding "please"/"not" tokens.
  # A distant modal cannot turn an unrelated later noun into a verb.
  defp skip_predicate_modifiers(phrases, idx) when is_integer(idx) and idx >= 0 do
    Enum.reduce(1..2, idx, fn _, current ->
      if current > 0 and MapSet.member?(@predicate_modifiers, Map.get(phrases, current - 1)),
        do: current - 1,
        else: current
    end)
  end

  defp skip_predicate_modifiers(_phrases, idx), do: idx

  defp inverted_subject?(phrases, idx) when is_integer(idx) and idx >= 2 do
    aux_idx = idx - 2
    question_prefix?(phrases, aux_idx) and
      MapSet.member?(@predicate_auxiliaries, Map.get(phrases, aux_idx)) and
      MapSet.member?(@subject_pronouns, Map.get(phrases, idx - 1))
  end

  defp inverted_subject?(_phrases, _idx), do: false

  defp inverted_determined_subject?(phrases, idx) when is_integer(idx) and idx >= 3 do
    aux_idx = idx - 3
    question_prefix?(phrases, aux_idx) and
      MapSet.member?(@predicate_auxiliaries, Map.get(phrases, aux_idx)) and
      MapSet.member?(@subject_determiners, Map.get(phrases, idx - 2)) and
      noun_witness?(phrases, idx - 1)
  end

  defp inverted_determined_subject?(_phrases, _idx), do: false

  defp declarative_subject?(phrases, idx) when is_integer(idx) and idx >= 2 do
    MapSet.member?(@subject_pronouns, Map.get(phrases, idx - 2)) and
      MapSet.member?(@predicate_auxiliaries, Map.get(phrases, idx - 1))
  end

  defp declarative_subject?(_phrases, _idx), do: false

  defp declarative_determined_subject?(phrases, idx)
       when is_integer(idx) and idx >= 3 do
    MapSet.member?(@subject_determiners, Map.get(phrases, idx - 3)) and
      noun_witness?(phrases, idx - 2) and
      MapSet.member?(@predicate_auxiliaries, Map.get(phrases, idx - 1))
  end

  defp declarative_determined_subject?(_phrases, _idx), do: false

  defp question_prefix?(_phrases, 0), do: true
  defp question_prefix?(phrases, 1),
    do: MapSet.member?(@question_prefixes, Map.get(phrases, 0))
  defp question_prefix?(_phrases, _idx), do: false

  # This is only a surface shape check, not a new noun-sense assertion.
  defp noun_witness?(phrases, idx) do
    case Map.get(phrases, idx) do
      word when is_binary(word) and word != "" ->
        Regex.match?(~r/\A\p{L}[\p{L}'-]*\z/u, word) and
          not MapSet.member?(@predicate_auxiliaries, word) and
          not MapSet.member?(@predicate_modifiers, word)
      _ -> false
    end
  end

  # A linking/change-of-state verb following a conjunction
  # is a plausible coordinated predicate.
  #
  # Examples:
  #   "I went to the doctor and got sick."
  #   "He stumbled and became dizzy."
  #
  # This is deliberately limited to recognized verb forms.
  # It does NOT favor every word following "and".
  defp coordinated_predicate_bias(ctx, tok, token_phrase, family) do
    phrases = token_phrases(ctx)
    idx = token_index(tok, -1)

    previous = Map.get(phrases, idx - 1)
    following = Map.get(phrases, idx + 1)

    coordinated? =
      previous in ["and", "but", "or"] and
        is_binary(following) and
        MapSet.member?(@linking_verb_forms, token_phrase)

    if coordinated? do
      case family do
        :verb -> 0.30
        :noun -> -0.12
        :proper_noun -> -0.12
        _ -> 0.0
      end
    else
      0.0
    end
  end

  # A predicate adjective may follow a copular or
  # change-of-state verb:
  #
  #   "became tired"
  #   "felt ill"
  #   "got sick"
  #
  # Only reward adjective candidates here; don't
  # automatically penalize nouns, because constructions
  # like "got cash" are also grammatical.
  defp predicate_adjective_bias(ctx, tok, :adjective) do
    phrases = token_phrases(ctx)
    idx = token_index(tok, -1)

    previous = Map.get(phrases, idx - 1)

    if MapSet.member?(@linking_verb_forms, previous) do
      0.34
    else
      0.0
    end
  end

  defp predicate_adjective_bias(_ctx, _tok, _family), do: 0.0

  def token_start(tok) when is_map(tok) do
    case Safe.get(tok, :span) || Safe.get(tok, "span") do
      {start, _stop} when is_integer(start) -> start
      [start, _stop] when is_integer(start) -> start
      _ -> token_index(tok, 0) * 10_000
    end
  end

  def token_start(_), do: 0

  # ===== Internal helpers =====

  defp fine_day_pos_bias("fine", "day", :adjective), do: 0.34
  defp fine_day_pos_bias("fine", "day", :verb), do: -0.34
  defp fine_day_pos_bias("fine", "day", :noun), do: -0.08
  defp fine_day_pos_bias(_phrase, _next_phrase, _family), do: 0.0

  defp greeting_chain_pos_bias(ctx, tok, token_phrase, family) do
    case greeting_expected_family(ctx, tok, token_phrase) do
      nil -> 0.0
      ^family -> 0.28
      expected when expected in [:adverb, :auxiliary, :pronoun, :verb] -> -0.24
    end
  end

  defp greeting_expected_family(ctx, tok, token_phrase) do
    phrases = token_phrases(ctx)
    idx = token_index(tok, -1)
    window = Enum.map(idx..(idx + 3), &Map.get(phrases, &1))

    cond do
      token_phrase == "how" and window == ["how", "are", "you", "doing"] ->
        :adverb

      token_phrase == "are" and
          Enum.map((idx - 1)..(idx + 2), &Map.get(phrases, &1)) == ["how", "are", "you", "doing"] ->
        :auxiliary

      token_phrase == "you" and
          Enum.map((idx - 2)..(idx + 1), &Map.get(phrases, &1)) == ["how", "are", "you", "doing"] ->
        :pronoun

      token_phrase == "doing" and
          Enum.map((idx - 3)..idx, &Map.get(phrases, &1)) == ["how", "are", "you", "doing"] ->
        :verb

      true ->
        nil
    end
  end

  defp token_phrases(ctx) when is_map(ctx) do
    ctx
    |> Map.get(:si, %{})
    |> Safe.get(:tokens, [])
    |> List.wrap()
    |> Enum.reduce(%{}, fn tok, acc ->
      if is_map(tok) do
        Map.put(acc, token_index(tok, map_size(acc)), tok |> token_raw_phrase() |> norm())
      else
        acc
      end
    end)
  end

  defp next_token_phrase(ctx, tok) do
    idx = token_index(tok, -1)
    ctx |> token_phrases() |> Map.get(idx + 1)
  end

  defp nearest_left_choice(choices, current_start) when is_list(choices) do
    choices
    |> Enum.filter(fn ch -> choice_start(ch) < current_start end)
    |> Enum.max_by(&choice_start/1, fn -> nil end)
  end

  defp previous_choice_pos_family(nil), do: nil

  defp previous_choice_pos_family(choice) when is_map(choice) do
    id =
      Safe.get(choice, :chosen_id) || Safe.get(choice, :id) ||
        Safe.get(choice, "chosen_id") || Safe.get(choice, "id")

    {_lemma, pos, _tag} = parse_sense_id(to_string(id || ""))
    syntactic_pos_family(pos)
  end

  defp previous_choice_pos_family(_), do: nil

  defp context_bias_for(:preposition, fam) when fam in [:noun, :proper_noun, :pronoun, :phrase],
    do: 0.18

  defp context_bias_for(:preposition, fam) when fam in [:verb, :auxiliary, :preposition],
    do: -0.16

  defp context_bias_for(:determiner, fam) when fam in [:noun, :proper_noun, :adjective, :phrase],
    do: 0.22

  defp context_bias_for(:determiner, fam)
       when fam in [:verb, :auxiliary, :preposition, :conjunction],
       do: -0.20

  defp context_bias_for(:auxiliary, :verb), do: 0.18
  defp context_bias_for(:auxiliary, :auxiliary), do: -0.12
  defp context_bias_for(:auxiliary, fam) when fam in [:noun, :proper_noun], do: -0.08
  defp context_bias_for(:pronoun, fam) when fam in [:verb, :auxiliary], do: 0.08
  defp context_bias_for(_prev, _cand), do: 0.0

  defp syntactic_pos_family(pos) do
    p = pos |> to_string() |> String.downcase()

    cond do
      p in ["noun", "n", "common_noun"] -> :noun
      p in ["proper_noun", "proper noun", "propn", "proper"] -> :proper_noun
      p in ["verb", "v"] -> :verb
      p in ["adjective", "adj"] -> :adjective
      p in ["phrase", "mwe", "multiword"] -> :phrase
      p in ["pronoun", "pron"] -> :pronoun
      p in ["determiner", "det", "article"] -> :determiner
      p in ["preposition", "prep", "adposition"] -> :preposition
      p in ["auxiliary", "aux", "modal"] -> :auxiliary
      p in ["conjunction", "conj", "connector", "cc"] -> :conjunction
      p in ["adverb", "adv", "intensifier"] -> :adverb
      p in ["particle", "negation", "neg", "negative"] -> :particle
      p in ["interjection", "interj", "greeting"] -> :interjection
      true -> :other
    end
  end

  defp choice_start(choice) when is_map(choice) do
    Safe.get(choice, :span_start) || Safe.get(choice, "span_start") ||
      (Safe.get(choice, :token_index) || Safe.get(choice, :index) ||
         Safe.get(choice, "token_index") || Safe.get(choice, "index") || 0) * 10_000
  end

  defp choice_start(_), do: 0

  # ===== Shared utilities (copies) =====

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

  defp token_index(tok, fallback_idx) do
    raw =
      Safe.get(tok, :token_index) || Safe.get(tok, "token_index") ||
        Safe.get(tok, :index) || Safe.get(tok, "index")

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
    Safe.get(tok, :phrase) || Safe.get(tok, "phrase") ||
      Safe.get(tok, :lemma) || Safe.get(tok, "lemma") ||
      Safe.get(tok, :word) || Safe.get(tok, "word") ||
      phrase_from_id(Safe.get(tok, :id) || Safe.get(tok, "id")) || ""
  end

  defp phrase_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [ph, _rest] -> ph
      _ -> nil
    end
  end

  defp phrase_from_id(_), do: nil

  defp parse_sense_id(id_str) when is_binary(id_str) do
    case String.split(id_str, "|") do
      [lemma, pos, tag] -> {lemma, String.downcase(pos || "other"), String.downcase(tag || "")}
      [lemma, pos] -> {lemma, String.downcase(pos || "other"), ""}
      [lemma] -> {lemma, "other", ""}
      _ -> {id_str, "other", ""}
    end
  end

  defp parse_sense_id(other), do: parse_sense_id(to_string(other))
end
