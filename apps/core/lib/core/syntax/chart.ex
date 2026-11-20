defmodule Core.Syntax.Chart do
  @moduledoc """
  Bounded, pure bottom-up constituent chart for externally observed sentences.

  L1 only: returns evidence-backed grammatical signatures, not meanings,
  passive events, verified claims, memory writes or actions. On any budget
  overflow it returns no signature; it never silently truncates a parse.
  """

  alias Core.Syntax.Grammar
  alias Core.MWE.Boundaries

  @max_tokens 48
  @max_items_per_span 6
  @max_items 400
  @max_results 12
  @max_nominal_modifiers 2
  @max_adjuncts 2

  @subject_modifier_keys [:subject_modifier_1, :subject_modifier_2]
  @predicate_preposition_keys [:predicate_adjunct_1_preposition, :predicate_adjunct_2_preposition]
  @predicate_object_keys [:predicate_adjunct_1_object, :predicate_adjunct_2_object]
  @predicate_adverb_keys [:predicate_adjunct_1_adverb, :predicate_adjunct_2_adverb]
  @predicate_nested_preposition_keys [:predicate_adjunct_1_nested_preposition, :predicate_adjunct_2_nested_preposition]
  @predicate_nested_object_keys [:predicate_adjunct_1_nested_object, :predicate_adjunct_2_nested_object]
  @subject_preposition_keys [:subject_adjunct_1_preposition, :subject_adjunct_2_preposition]
  @subject_object_keys [:subject_adjunct_1_object, :subject_adjunct_2_object]
  @subject_nested_preposition_keys [:subject_adjunct_1_nested_preposition, :subject_adjunct_2_nested_preposition]
  @subject_nested_object_keys [:subject_adjunct_1_nested_object, :subject_adjunct_2_nested_object]

  # P-039: closed subject_* -> object_* role-key map, built at compile time
  # from the fixed key inventory above (never from input text).
  @object_rekey (([:subject_head, :subject_determiner, :subject_quantifier,
                   :subject_possessor] ++ @subject_modifier_keys ++
                    @subject_preposition_keys ++ @subject_object_keys ++
                    @subject_nested_preposition_keys ++ @subject_nested_object_keys)
                 |> Map.new(fn key ->
                   {key, String.to_atom("object_" <>
                     String.replace_prefix(Atom.to_string(key), "subject_", ""))}
                 end))

  @doc "Generate full-span grammatical hypotheses or an empty list. No side effects."
  @spec clause_signatures(map(), list(), list()) :: [map()]
  def clause_signatures(si, nodes, possessive_signatures \\ []) do
    diagnose(si, nodes, possessive_signatures).signatures
  end

  @doc """
  Observation-only chart result. The reason is a closed diagnostic category;
  it must never be interpreted as a claim about the external text's truth.
  """
  @spec diagnose(map(), list(), list(), keyword()) :: map()
  def diagnose(si, nodes, possessive_signatures \\ [], opts \\ [])

  def diagnose(%{} = si, nodes, possessive, opts)
      when is_list(nodes) and is_list(possessive) and is_list(opts) do
    limits = limits(opts)
    rule_set = rule_set(opts)
    harness? = rule_set == :clause_v1
    sorted = Enum.sort_by(nodes, &Map.get(&1, :index, -1))
    indices = Enum.map(sorted, &Map.get(&1, :index))
    sentence = Map.get(si, :sentence, "")

    # Classify forbidden sentence shapes before the source-gap checker:
    # canonical tokenization correctly reports punctuation gaps, but those
    # gaps must not obscure structured records or embedded commands.
    # Either path abstains; only fully validated inputs reach chart fill.
    reason =
      cond do
        Map.get(si, :source) != :colony_external and not harness? ->
          :unsupported_source

        true ->
          case rejection_reason(sentence, sorted, indices, limits.tokens, harness?) do
            nil -> if valid_original_tokens?(si, sorted), do: nil, else: :coverage_gap
            rejection -> rejection
          end
      end

    if reason do
      abstain(reason, 0)
    else
      case fill(sorted, possessive, limits, rule_set) do
        {:error, reason, count}
        when reason in [:chart_budget_exceeded, :lexical_ambiguity_budget] ->
          abstain(reason, count)

        {:ok, chart, count} ->
          last = length(sorted) - 1

          signatures =
            chart
            |> Map.get({0, last}, [])
            |> Enum.filter(&(&1.kind == :sentence))
            |> Enum.map(&signature/1)
            |> Enum.uniq_by(&{&1.id, &1.span, &1.roles})
            |> Enum.sort_by(&signature_sort_key/1)

          cond do
            length(signatures) > limits.results ->
              abstain(:chart_budget_exceeded, count)

            signatures != [] ->
              %{status: :candidate, reason: nil, signatures: signatures,
                chart_items: count, factual_authority?: false}

            true ->
              abstain(failure_reason(sorted), count)
          end
      end
    end
  end

  def diagnose(_, _, _, _), do: abstain(:invalid_input, 0)

  @doc false
  def limits do
    %{tokens: @max_tokens, per_span: @max_items_per_span,
      total: @max_items, results: @max_results}
  end

  # Test-only reductions can lower but never raise the production budgets.
  defp limits(opts) do
    for {key, default} <- Map.to_list(limits()), into: %{} do
      proposed = Keyword.get(opts, key, default)
      {key, if(is_integer(proposed) and proposed > 0, do: min(default, proposed), else: default)}
    end
  end

  # P-039: the clause rule set is honoured ONLY together with an explicit
  # harness override. Any other combination is the unchanged default chart.
  defp rule_set(opts) do
    if Keyword.get(opts, :rule_set) == :clause_v1 and
         Keyword.get(opts, :harness_override) == true,
       do: :clause_v1,
       else: :default
  end

  defp abstain(reason, count),
    do: %{status: :abstained, reason: reason, signatures: [],
      chart_items: count, factual_authority?: false}

  defp rejection_reason(sentence, nodes, indices, max_tokens, harness?) do
    words = Enum.map(nodes, & &1.word)
    cond do
      not is_binary(sentence) -> :invalid_input
      # The token cap remains unconditional, including for rejected input.
      length(indices) > max_tokens -> :chart_budget_exceeded
      # The following closed, unsafe/structured shapes always abstain.
      # Classify them before generic index/gap checks: canonical tokenization
      # can omit punctuation and URL components while retaining source text.
      Regex.match?(~r/\A\s*[\p{L}\p{M}][\p{L}\p{M} ]{0,32}:\s*\S/u, sentence) and
        not Enum.any?(words, &Grammar.copula?/1) -> :structured_record
      Enum.any?(words, &Grammar.subordinator?/1) -> :instruction_shaped
      Regex.match?(~r/[,;:]|\s[-—–]\s|\n/u, sentence) -> :multiple_clauses
      indices == [] -> :no_clause_core
      indices != Enum.to_list(0..(length(indices) - 1)) -> :coverage_gap
      Enum.any?(nodes, &(not is_binary(Map.get(&1, :surface)))) -> :coverage_gap
      String.ends_with?(String.trim(sentence), "?") or
        Grammar.copula?(hd(words)) -> :interrogative
      # P-039 harness only: negation and non-copular clauses reach the chart;
      # the clause grammar itself admits only "do + not + base verb".
      Enum.any?(words, &Grammar.negative?/1) and
          not (harness? and do_not_only?(words)) -> :negation
      Enum.any?(words, &Grammar.modal?/1) -> :modal
      not harness? and not Enum.any?(words, &Grammar.copula?/1) ->
        if verb_initial?(hd(nodes)), do: :instruction_shaped, else: :no_clause_core
      not Boundaries.valid_window?(Enum.map(nodes,
        &%{phrase: &1.surface, index: &1.index})) -> :coverage_gap
      true -> nil
    end
  end

  # Harness-only: every negative cue must be a bare "not" directly after a
  # do-auxiliary. "nobody", "no", "never", "n't" contractions etc. still
  # abstain, so a negated clause can never be read with positive polarity.
  defp do_not_only?(words) do
    words
    |> Enum.with_index()
    |> Enum.all?(fn {word, i} ->
      not Grammar.negative?(word) or
        (Grammar.clause_negator?(word) and i > 0 and
           Grammar.do_auxiliary?(Enum.at(words, i - 1)))
    end)
  end

  # Match the real Brain base-index validity conditions; do not accept an
  # index invented by base_nodes/2 for a malformed source token.
  defp valid_original_tokens?(si, nodes) do
    valid =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(fn
        %{index: index, phrase: phrase} = token
        when is_integer(index) and index >= 0 and is_binary(phrase) ->
          Map.get(token, :n, 1) == 1 and
            Map.get(token, :kind) not in [:punct, "punct"]

        _ -> false
      end)

    Enum.map(valid, & &1.index) == Enum.map(nodes, & &1.index) and
      # Canonical tokenization supplies end-exclusive spans and original gaps.
      # Legacy tests without this metadata retain the lexical-window check.
      Boundaries.valid_window?(valid, si)
  end

  defp verb_initial?(node), do: has_pos?(node, "verb")

  defp failure_reason(nodes) do
    cop = Enum.find(nodes, &Grammar.copula?(&1.word))

    cond do
      is_nil(cop) -> :no_clause_core
      Enum.any?(nodes, &(Grammar.copula?(&1.word) and not copula_pos?(&1))) ->
        :lexical_witness_missing
      Enum.any?(nodes, &(Grammar.quantifier?(&1.word) and
        not has_pos?(&1, "determiner"))) -> :lexical_witness_missing
      agreement_conflict?(nodes, cop) -> :agreement_conflict
      missing_core_witness?(nodes, cop) -> :lexical_witness_missing
      true -> :coverage_gap
    end
  end

  defp agreement_conflict?(nodes, cop) do
    prior = Enum.filter(nodes, &(&1.index < cop.index))
    head = List.last(prior)
    specifier = List.first(prior)

    if is_map(head) and (has_pos?(head, "noun") or has_pos?(head, "pronoun")) do
      number =
        if has_pos?(head, "pronoun"),
          do: Grammar.pronoun_number(head.word),
          else: if(Map.get(head, :plural_noun?, false), do: :plural, else: :singular)

      specifier_number =
        cond do
          is_nil(specifier) -> :unknown
          Grammar.determiner?(specifier.word) ->
            Grammar.determiner_number(specifier.word)
          Grammar.quantifier?(specifier.word) ->
            Grammar.quantifier_number(specifier.word)
          true -> :unknown
        end

      invalid_specifier? =
        specifier_number in [:singular, :plural] and
          number in [:singular, :plural] and
          specifier_number != number

      invalid_copula? = number != :unknown and not agreement?(number, cop.word)
      invalid_specifier? or invalid_copula?
    else
      false
    end
  end

  defp missing_core_witness?(nodes, cop) do
    before = Enum.filter(nodes, &(&1.index < cop.index))
    after_copula = Enum.filter(nodes, &(&1.index > cop.index))
    not Enum.any?(before, &(has_pos?(&1, "noun") or has_pos?(&1, "pronoun") or
      has_pos?(&1, "proper_noun"))) or
      not Enum.any?(after_copula, &(has_pos?(&1, "adjective") or
        (has_pos?(&1, "verb") and MapSet.member?(Map.get(&1, :morphology, MapSet.new()), :past_participle))))
  end

  defp fill(nodes, possessive, limits, rule_set) do
    n = length(nodes)
    seeds =
      (Enum.flat_map(nodes, &terminals(&1, rule_set)) ++
        Enum.flat_map(possessive, &possessive_seed(&1, nodes)))
      |> Enum.group_by(&{&1.first, &1.last})

    Enum.reduce_while(1..n, {:ok, %{}, 0}, fn width, {:ok, chart, total} ->
      starts = 0..(n - width)
      result =
        Enum.reduce_while(starts, {:ok, chart, total}, fn first, {:ok, acc, count} ->
          last = first + width - 1
          initial = Map.get(seeds, {first, last}, [])
          joined =
            if width == 1 do
              []
            else
              for split <- first..(last - 1),
                  left <- Map.get(acc, {first, split}, []),
                  right <- Map.get(acc, {split + 1, last}, []),
                  rule <- Grammar.binary_rules(rule_set),
                  rule.rhs == [left.kind, right.kind],
                  item <- List.wrap(combine(rule.op, left, right)) do
                item
              end
            end

          case close(initial ++ joined, limits.per_span, rule_set) do
            {:error, :chart_budget_exceeded} ->
              # A production width-1 overflow witnesses too many lexical
              # alternatives, not an excessively long sentence. Keep the
              # existing generic reason for tests using reduced limits.
              reason =
                if width == 1 and limits.per_span == @max_items_per_span,
                  do: :lexical_ambiguity_budget,
                  else: :chart_budget_exceeded

              {:halt, {:error, reason, count}}

            {:ok, items} ->
              new_total = count + length(items)
              if new_total > limits.total do
                {:halt, {:error, :chart_budget_exceeded, new_total}}
              else
                {:cont, {:ok, Map.put(acc, {first, last}, items), new_total}}
              end
          end
        end)

      case result do
        {:ok, next, count} -> {:cont, {:ok, next, count}}
        {:error, reason, count} -> {:halt, {:error, reason, count}}
      end
    end)
  end

  defp close(items, limit, rule_set) do
    Enum.reduce_while(1..6, {:ok, sorted_items(items)}, fn _, {:ok, current} ->
      if length(current) > limit do
        {:halt, {:error, :chart_budget_exceeded}}
      else
        expanded = for item <- current, rule <- Grammar.unary_rules(rule_set),
          rule.rhs == [item.kind], derived <- List.wrap(unary(rule.op, item)),
          do: derived
        next = sorted_items(current ++ expanded)

        cond do
          length(next) > limit -> {:halt, {:error, :chart_budget_exceeded}}
          next == current -> {:halt, {:ok, current}}
          true -> {:cont, {:ok, next}}
        end
      end
    end)
  end

  defp sorted_items(items) do
    items
    |> Enum.uniq_by(&item_key/1)
    |> Enum.sort_by(&item_key/1)
  end

  defp item_key(i) do
    {i.kind, i.first, i.last, i.head, Enum.sort(Map.to_list(i.roles)),
      Enum.sort(Map.to_list(i.required)), i.modifiers, i.adjuncts,
      i.reading, i.number, i.copula, i.predicate, i.clause}
  end

  defp item(kind, first, last, overrides) do
    Map.merge(%{
      kind: kind, first: first, last: last, head: nil, roles: %{},
      required: %{}, modifiers: [], adjuncts: [], reading: [],
      number: :unknown, copula: nil, predicate: nil, clause: nil
    }, overrides)
  end

  defp terminals(node, :default), do: terminals(node)
  defp terminals(node, :clause_v1), do: terminals(node) ++ clause_terminals(node)

  # P-039 verb-group terminals. Each requires dictionary-attested verb POS
  # (any sense) plus the stated morphology; closed function words and the
  # whole BE paradigm never become lexical verbs (BE takes no direct object).
  defp clause_terminals(node) do
    i = node.index
    word = node.word
    morphology = Map.get(node, :morphology, MapSet.new())

    function_surface? =
      Grammar.determiner?(word) or Grammar.quantifier?(word) or
        Grammar.pronoun?(word) or Grammar.copula?(word) or Grammar.be_form?(word)

    verb? = has_pos?(node, "verb") and not function_surface?
    make = fn kind, extra -> item(kind, i, i, Map.merge(%{head: i, word: word}, extra)) end

    vgroup = fn tense, agreement ->
      make.(:vgroup, %{
        clause: %{
          tense: tense, aspect: :simple, polarity: :positive, voice: :active,
          agreement: agreement, predicate: i, auxiliaries: [], negation: nil
        }
      })
    end

    aux_do =
      if Grammar.do_auxiliary?(word) and
           (has_pos?(node, "verb") or has_pos?(node, "auxiliary")),
         do: [make.(:aux_do, %{})], else: []

    negator =
      if Grammar.clause_negator?(word) and
           (has_pos?(node, "adverb") or has_pos?(node, "particle")),
         do: [make.(:negator, %{})], else: []

    past =
      if verb? and MapSet.member?(morphology, :simple_past),
        do: [vgroup.(:past, :any)], else: []

    third_singular =
      if verb? and Map.get(node, :finite_present_verb?, false),
        do: [vgroup.(:present, :singular)], else: []

    base =
      if verb? and Map.get(node, :base_present_verb?, false),
        do: [vgroup.(:present, :non_third_singular), make.(:verb_base, %{})],
        else: []

    present_participle =
      if verb? and MapSet.member?(morphology, :present_participle),
        do: [make.(:participle_present, %{})], else: []

    past_participle =
      if verb? and MapSet.member?(morphology, :past_participle),
        do: [make.(:participle_past, %{})], else: []

    aux_do ++ negator ++ past ++ third_singular ++ base ++
      present_participle ++ past_participle
  end

  defp terminals(node) do
    i = node.index
    make = fn kind -> item(kind, i, i, %{head: i, word: node.word}) end

    # This initial copular grammar treats closed function-word surfaces as
    # grammatical categories only. Otherwise an alternative noun/adjective
    # POS row for "a", "each" or "i" could bypass the specifier or person
    # agreement gate by becoming an unrelated nominal modifier.
    function_surface? =
      Grammar.determiner?(node.word) or Grammar.quantifier?(node.word) or
        Grammar.pronoun?(node.word) or Grammar.copula?(node.word)

    noun = if has_pos?(node, "noun") and not function_surface?,
      do: [item(:noun, i, i, %{head: i, word: node.word,
        number: if(Map.get(node, :plural_noun?, false), do: :plural, else: :singular)})],
      else: []
    adjective = if has_pos?(node, "adjective") and not function_surface?,
      do: [make.(:adjective)], else: []
    pronoun = if has_pos?(node, "pronoun") and Grammar.pronoun?(node.word),
      do: [item(:pronoun, i, i, %{head: i, word: node.word,
        number: Grammar.pronoun_number(node.word)})], else: []
    proper = if has_pos?(node, "proper_noun") and not function_surface?,
      do: [make.(:proper_noun)], else: []
    determiner = if has_pos?(node, "determiner") and Grammar.determiner?(node.word),
      do: [make.(:determiner)], else: []
    quantifier =
      if Grammar.quantifier?(node.word) and has_pos?(node, "determiner"),
        do: [make.(:quantifier)],
        else: []
    copula = if Grammar.copula?(node.word) and copula_pos?(node),
      do: [make.(:copula)], else: []
    reading =
      (if has_pos?(node, "adjective"), do: [:adjectival], else: []) ++
      (if has_pos?(node, "verb") and
        MapSet.member?(Map.get(node, :morphology, MapSet.new()), :past_participle),
        do: [:participial], else: [])
    complement =
      if reading != [] do
        [item(:complement, i, i, %{head: i, word: node.word, reading: reading})]
      else
        []
      end
    preposition = if has_pos?(node, "preposition") and not function_surface?,
      do: [make.(:preposition)], else: []
    adverb = if has_pos?(node, "adverb") and not function_surface?,
      do: [make.(:adverb)], else: []

    noun ++ adjective ++ pronoun ++ proper ++ determiner ++ quantifier ++
      copula ++ complement ++ preposition ++ adverb
  end

  defp possessive_seed(%{
    id: :possessive_noun_phrase, span: {first, last},
    source: :core_syntax, roles: %{head: head, possessor: possessor}
  } = sig, nodes) when is_integer(first) and is_integer(last) and first <= last do
    head_node = Enum.find(nodes, &(&1.index == head))

    if is_map(head_node) and has_pos?(head_node, "noun") do
      [item(:possessive_np, first, last, %{head: head,
        roles: %{subject_head: head, subject_possessor: possessor},
        required: %{subject_head: [:pos_noun]},
        number: if(Map.get(head_node, :plural_noun?, false), do: :plural, else: :singular),
        evidence: Map.get(sig, :evidence, [])})]
    else
      []
    end
  end

  defp possessive_seed(_, _), do: []

  defp unary(:nominal_head, noun),
    do: item(:nominal, noun.first, noun.last,
      %{head: noun.head, number: noun.number, modifiers: []})

  defp unary(:bare_subject, nominal), do: noun_subject(nominal, %{}, %{})
  defp unary(:pronoun_subject, p), do: item(:subject, p.first, p.last,
    %{head: p.head, number: p.number,
      roles: %{subject_head: p.head}, required: %{subject_head: [:pos_pronoun]}})
  defp unary(:proper_subject, p), do: item(:subject, p.first, p.last,
    %{head: p.head, number: :unknown,
      roles: %{subject_head: p.head}, required: %{subject_head: [:pos_proper_noun]}})
  defp unary(:possessive_subject, p), do: %{p | kind: :subject}
  # P-039: a passive verb group with no by-phrase; the agent is unexpressed.
  defp unary(:agentless_passive, pv),
    do: item(:cvp, pv.first, pv.last,
      %{head: pv.head, clause: Map.merge(pv.clause, %{object: nil, agent: :unexpressed})})
  defp unary(_, _), do: nil

  defp noun_subject(np, extras, requirements) do
    {roles, required} =
      np.modifiers
      |> Enum.with_index()
      |> Enum.reduce({%{subject_head: np.head}, %{subject_head: [:pos_noun]}},
        fn {{type, index}, ordinal}, {rs, fs} ->
          key = Enum.at(@subject_modifier_keys, ordinal)
          feature = if(type == :adjective, do: :pos_adjective, else: :pos_noun)
          {Map.put(rs, key, index), Map.put(fs, key, [feature])}
        end)

    item(:subject, np.first, np.last, %{head: np.head, number: np.number,
      roles: Map.merge(roles, extras), required: Map.merge(required, requirements)})
  end

  defp combine(:adjective_modifier, modifier, nominal),
    do: prefix_nominal(modifier, nominal, :adjective)
  defp combine(:noun_modifier, modifier, nominal),
    do: prefix_nominal(modifier, nominal, :noun)

  defp combine(:determined_subject, determiner, nominal) do
    if Grammar.number_compatible?(Grammar.determiner_number(determiner.word),
         nominal.number) do
      noun_subject(%{nominal | first: determiner.first},
        %{subject_determiner: determiner.head},
        %{subject_determiner: [:pos_determiner]})
    end
  end

  defp combine(:determined_possessive_subject, determiner, subject) do
    if Map.has_key?(subject.roles, :subject_possessor) and
         not Map.has_key?(subject.roles, :subject_determiner) and
         Grammar.number_compatible?(Grammar.determiner_number(determiner.word),
           subject.number) do
      %{subject | first: determiner.first,
        roles: Map.put(subject.roles, :subject_determiner, determiner.head),
        required: Map.put(subject.required, :subject_determiner, [:pos_determiner])}
    end
  end

  defp combine(:quantified_subject, quantifier, nominal) do
    if Grammar.number_compatible?(Grammar.quantifier_number(quantifier.word),
         nominal.number) do
      noun_subject(%{nominal | first: quantifier.first},
        %{subject_quantifier: quantifier.head},
        %{subject_quantifier: [:pos_determiner]})
    end
  end

  defp combine(:pp, preposition, subject) do
    # One nested PP level can be encoded in fixed role keys. Rather than
    # collapse deeper attachment differences, abstain from such hypotheses.
    if length(subject.adjuncts) <= 1 and
         Enum.all?(subject.adjuncts, &(Map.get(&1, :adjuncts, []) == [])) do
      item(:pp, preposition.first, subject.last,
        %{head: subject.head, roles: %{preposition: preposition.head, object: subject.head},
          adjuncts: subject.adjuncts, word: preposition.word})
    end
  end

  defp combine(:subject_adjunct, subject, pp) do
    if length(subject.adjuncts) < @max_adjuncts do
      item(:subject, subject.first, pp.last,
        %{head: subject.head, number: subject.number, roles: subject.roles,
          required: subject.required, adjuncts: subject.adjuncts ++ [pp]})
    end
  end

  defp combine(:copular_vp, cop, complement) do
    item(:vp, cop.first, complement.last,
      %{copula: cop.head, copula_word: cop.word, predicate: complement.head,
        reading: complement.reading, adjuncts: []})
  end

  defp combine(:predicate_pp, vp, pp) do
    if length(vp.adjuncts) < @max_adjuncts do
      %{vp | last: pp.last, adjuncts: vp.adjuncts ++ [pp]}
    end
  end

  defp combine(:predicate_adverb, vp, adverb) do
    if length(vp.adjuncts) < @max_adjuncts do
      %{vp | last: adverb.last, adjuncts: vp.adjuncts ++ [adverb]}
    end
  end

  defp combine(:predication, subject, vp) do
    if agreement?(subject.number, vp.copula_word) do
      {sub_roles, sub_required} = subject_adjunct_roles(subject.adjuncts)
      {vp_roles, vp_required} = predicate_adjunct_roles(vp.adjuncts)

      features = if :adjectival in vp.reading and :participial in vp.reading,
        do: [:pos_adjective, :pos_verb],
        else: if(:adjectival in vp.reading, do: [:pos_adjective], else: [:pos_verb])

      roles =
        subject.roles
        |> Map.merge(sub_roles)
        |> Map.merge(vp_roles)
        |> Map.merge(%{copula: vp.copula, predicate: vp.predicate})

      required =
        subject.required
        |> Map.merge(sub_required)
        |> Map.merge(vp_required)
        |> Map.merge(%{copula: [:pos_auxiliary], predicate: features})

      item(:sentence, subject.first, vp.last, %{head: subject.head,
        roles: roles, required: required, reading: vp.reading,
        number: subject.number, copula: vp.copula, predicate: vp.predicate,
        copula_word: vp.copula_word,
        adjuncts: subject.adjuncts ++ vp.adjuncts})
    end
  end

  # -- P-039 clause rules (reachable only through Grammar.rules(:clause_v1)) --

  defp combine(:negated_do, aux, negator) do
    item(:neg_do, aux.first, negator.last,
      %{head: aux.head, word: aux.word, clause: %{auxiliary: aux.head, negation: negator.head}})
  end

  defp combine(:do_support_negative, neg_do, verb) do
    {tense, agreement} =
      case Grammar.do_number(neg_do.word) do
        :any -> {:past, :any}
        :singular -> {:present, :singular}
        :plural -> {:present, :non_third_singular}
        _ -> {nil, nil}
      end

    if tense do
      item(:vgroup, neg_do.first, verb.last, %{head: verb.head, clause: %{
        tense: tense, aspect: :simple, polarity: :negative, voice: :active,
        agreement: agreement, predicate: verb.head,
        auxiliaries: [neg_do.clause.auxiliary], negation: neg_do.clause.negation}})
    end
  end

  defp combine(:progressive, cop, participle),
    do: be_group(:vgroup, cop, participle, :progressive, :active)

  defp combine(:passive, cop, participle),
    do: be_group(:pvgroup, cop, participle, :simple, :passive)

  defp combine(:transitive, vgroup, object) do
    if vgroup.clause.voice == :active do
      item(:cvp, vgroup.first, object.last, %{head: vgroup.head,
        clause: Map.merge(vgroup.clause, %{object: object, agent: :not_applicable})})
    end
  end

  defp combine(:passive_agent, pvgroup, pp) do
    # Only an un-nested "by" phrase is an agent; any other PP stays an adjunct.
    if Map.get(pp, :word) == "by" and pp.adjuncts == [] do
      item(:cvp, pvgroup.first, pp.last, %{head: pvgroup.head,
        clause: Map.merge(pvgroup.clause, %{object: nil, agent: {:by, pp}})})
    end
  end

  defp combine(:clause_predication, subject, cvp) do
    clause = cvp.clause

    with true <- is_map(clause) and clause_agreement?(subject.number, clause.agreement),
         {:ok, object_roles, object_required} <- object_roles(clause.object) do
      {sub_roles, sub_required} = subject_adjunct_roles(subject.adjuncts)
      {vp_roles, vp_required} = predicate_adjunct_roles(cvp.adjuncts)
      {agent_roles, agent_required} = agent_roles(clause.agent)
      {aux_roles, aux_required} = auxiliary_roles(clause)

      roles =
        subject.roles
        |> Map.merge(sub_roles)
        |> Map.merge(object_roles)
        |> Map.merge(agent_roles)
        |> Map.merge(vp_roles)
        |> Map.merge(aux_roles)
        |> Map.put(:predicate, clause.predicate)

      required =
        subject.required
        |> Map.merge(sub_required)
        |> Map.merge(object_required)
        |> Map.merge(agent_required)
        |> Map.merge(vp_required)
        |> Map.merge(aux_required)
        |> Map.put(:predicate, [:pos_verb])

      item(:sentence, subject.first, cvp.last, %{head: subject.head,
        roles: roles, required: required, number: subject.number,
        predicate: clause.predicate,
        adjuncts: subject.adjuncts ++ cvp.adjuncts,
        clause: %{
          tense: clause.tense, aspect: clause.aspect, polarity: clause.polarity,
          voice: clause.voice, agent: agent_status(clause.agent),
          attachments: attachments(subject, clause.object, cvp.adjuncts)
        }})
    else
      _ -> nil
    end
  end

  defp combine(_, _, _), do: nil

  defp prefix_nominal(modifier, nominal, type) do
    if length(nominal.modifiers) < @max_nominal_modifiers do
      item(:nominal, modifier.first, nominal.last,
        %{head: nominal.head, number: nominal.number,
          modifiers: [{type, modifier.head} | nominal.modifiers]})
    end
  end

  # -- P-039 clause helpers ---------------------------------------------------

  defp be_group(kind, cop, participle, aspect, voice) do
    tense = if cop.word in ~w(is are), do: :present, else: :past

    item(kind, cop.first, participle.last, %{head: participle.head, clause: %{
      tense: tense, aspect: aspect, polarity: :positive, voice: voice,
      agreement: {:copula, cop.word}, predicate: participle.head,
      auxiliaries: [cop.head], negation: nil}})
  end

  # Past forms (and "did") agree with any subject, including an unknown
  # proper-noun number. Present forms keep the copular chart's strictness:
  # an unknown subject number never licenses a present-tense clause.
  defp clause_agreement?(_number, :any), do: true
  defp clause_agreement?(number, :singular), do: number == :singular
  defp clause_agreement?(number, :non_third_singular),
    do: number in [:plural, :first_person_singular]
  defp clause_agreement?(number, {:copula, word}), do: agreement?(number, word)
  defp clause_agreement?(_, _), do: false

  # An object (or by-agent) NP is an ordinary :subject constituent; its role
  # keys are re-keyed from subject_* to object_* through a closed, compile-time
  # map. An unexpected key abstains rather than colliding with subject roles.
  defp object_roles(nil), do: {:ok, %{}, %{}}

  defp object_roles(np) do
    {adjunct_roles, adjunct_required} = subject_adjunct_roles(np.adjuncts)

    with {:ok, roles} <- rekey(Map.merge(np.roles, adjunct_roles)),
         {:ok, required} <- rekey(Map.merge(np.required, adjunct_required)) do
      {:ok, roles, required}
    end
  end

  defp rekey(map) do
    Enum.reduce_while(map, {:ok, %{}}, fn {key, value}, {:ok, acc} ->
      case Map.fetch(@object_rekey, key) do
        {:ok, object_key} -> {:cont, {:ok, Map.put(acc, object_key, value)}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp agent_roles({:by, pp}),
    do: {%{agent_preposition: pp.roles.preposition, agent_head: pp.roles.object},
         %{agent_preposition: [:pos_preposition], agent_head: [:pos_noun_or_pronoun]}}
  defp agent_roles(_), do: {%{}, %{}}

  defp agent_status({:by, _}), do: :expressed
  defp agent_status(other), do: other

  defp auxiliary_roles(clause) do
    {roles, required} =
      case clause.auxiliaries do
        [aux] -> {%{auxiliary: aux}, %{auxiliary: [:pos_auxiliary]}}
        _ -> {%{}, %{}}
      end

    if is_integer(clause.negation),
      do: {Map.put(roles, :negation, clause.negation),
           Map.put(required, :negation, [:pos_adverb])},
      else: {roles, required}
  end

  # Where each top-level PP is attached. Competing attachments are separate
  # full-span signatures; none is chosen here.
  defp attachments(subject, object, vp_adjuncts) do
    pps = fn adjuncts, site ->
      for %{kind: :pp} = pp <- adjuncts,
          do: %{preposition: pp.roles.preposition, object: pp.roles.object, attached_to: site}
    end

    pps.(subject.adjuncts, :subject) ++
      pps.(if(is_map(object), do: object.adjuncts, else: []), :object) ++
      pps.(vp_adjuncts, :predicate)
  end

  # Unknown subject number cannot license a positive clause.
  defp agreement?(:unknown, _), do: false
  # The witnessed first-person singular "I" takes "was", never "is".
  # The subjunctive "I were" is not admitted by this indicative-only chart.
  defp agreement?(:first_person_singular, "was"), do: true
  defp agreement?(:first_person_singular, _), do: false
  defp agreement?(subject_number, copula), do: subject_number == Grammar.agreement(copula)

  defp subject_adjunct_roles(adjuncts) do
    adjuncts
    |> Enum.with_index()
    |> Enum.reduce({%{}, %{}}, fn {pp, ordinal}, {roles, requirements} ->
      preposition_key = Enum.at(@subject_preposition_keys, ordinal)
      object_key = Enum.at(@subject_object_keys, ordinal)
      roles = roles
        |> Map.put(preposition_key, pp.roles.preposition)
        |> Map.put(object_key, pp.roles.object)
      requirements = requirements
        |> Map.put(preposition_key, [:pos_preposition])
        |> Map.put(object_key, [:pos_noun_or_pronoun])
      nested = List.first(pp.adjuncts)
      if is_map(nested) and nested.kind == :pp do
        nested_preposition_key = Enum.at(@subject_nested_preposition_keys, ordinal)
        nested_object_key = Enum.at(@subject_nested_object_keys, ordinal)
        {roles |> Map.put(nested_preposition_key, nested.roles.preposition)
          |> Map.put(nested_object_key, nested.roles.object),
         requirements |> Map.put(nested_preposition_key, [:pos_preposition])
          |> Map.put(nested_object_key, [:pos_noun_or_pronoun])}
      else
        {roles, requirements}
      end
    end)
  end

  defp predicate_adjunct_roles(adjuncts) do
    adjuncts
    |> Enum.with_index()
    |> Enum.reduce({%{}, %{}}, fn {adj, ordinal}, {roles, requirements} ->
      if adj.kind == :pp do
        prep_key = Enum.at(@predicate_preposition_keys, ordinal)
        obj_key = Enum.at(@predicate_object_keys, ordinal)
        roles = roles |> Map.put(prep_key, adj.roles.preposition)
          |> Map.put(obj_key, adj.roles.object)
        requirements = requirements |> Map.put(prep_key, [:pos_preposition])
          |> Map.put(obj_key, [:pos_noun_or_pronoun])
        nested = List.first(adj.adjuncts)

        if is_map(nested) and nested.kind == :pp do
          nested_prep = Enum.at(@predicate_nested_preposition_keys, ordinal)
          nested_obj = Enum.at(@predicate_nested_object_keys, ordinal)
          {roles |> Map.put(nested_prep, nested.roles.preposition)
            |> Map.put(nested_obj, nested.roles.object),
           requirements |> Map.put(nested_prep, [:pos_preposition])
            |> Map.put(nested_obj, [:pos_noun_or_pronoun])}
        else
          {roles, requirements}
        end
      else
        key = Enum.at(@predicate_adverb_keys, ordinal)
        {Map.put(roles, key, adj.head), Map.put(requirements, key, [:pos_adverb])}
      end
    end)
  end

  # P-039: one construction id for every finite-clause realization. Tense,
  # aspect, polarity and voice are features of the parse, not separate ids.
  defp signature(%{clause: %{voice: _} = clause} = parse) do
    attachment_evidence =
      clause.attachments
      |> Enum.map(fn
        %{attached_to: :subject} -> :subject_pp_attachment
        %{attached_to: :object} -> :object_pp_attachment
        %{attached_to: :predicate} -> :predicate_pp_attachment
      end)
      |> Enum.uniq()

    evidence =
      Enum.concat([
        [:chart_finite_clause, :attested_verb_morphology, :source_aligned_full_span,
         :clause_rules_v1_harness_only],
        if(clause.voice == :passive, do: [:passive_be_participle], else: []),
        if(clause.aspect == :progressive, do: [:progressive_be_participle], else: []),
        if(clause.polarity == :negative, do: [:do_support_negation], else: []),
        if(clause.agent == :expressed, do: [:by_agent_phrase], else: []),
        if(Enum.any?(Map.keys(parse.roles), &(&1 in [:subject_quantifier, :object_quantifier])),
          do: [:quantifier_unapplied], else: []),
        attachment_evidence
      ])

    %{
      id: :finite_clause,
      source: :core_syntax,
      span: {parse.first, parse.last},
      roles: parse.roles,
      required_features: parse.required,
      evidence: evidence,
      clause: Map.take(clause, [:tense, :aspect, :polarity, :voice, :agent, :attachments]),
      factual_authority?: false
    }
  end

  defp signature(parse) do
    evidence =
      Enum.concat([
        [:chart_copular_predication, :attested_copula,
          :attested_complement, :source_aligned_full_span,
          :no_negation_cue_in_closed_list],
        if(:adjectival in parse.reading, do: [:attested_adjective], else: []),
        if(:participial in parse.reading, do: [:attested_past_participle], else: []),
        if(parse.copula_word in ["is", "are"], do: [:present_copula], else: [:past_copula]),
        if(Map.has_key?(parse.roles, :subject_quantifier), do: [:quantifier_unapplied], else: []),
        if(Enum.any?(parse.adjuncts, fn adj ->
          adj.kind == :pp and Map.get(adj, :word) == "by"
        end), do: [:by_phrase_adjunct_uninterpreted], else: [])
      ])

    %{
      id: :copular_predication,
      source: :core_syntax,
      span: {parse.first, parse.last},
      roles: parse.roles,
      required_features: parse.required,
      evidence: evidence,
      factual_authority?: false
    }
  end

  defp signature_sort_key(s),
    do: {s.span, Enum.sort(Map.to_list(s.roles)), Enum.sort(Map.to_list(s.required_features))}

  defp has_pos?(node, pos),
    do: MapSet.member?(Map.get(node, :pos, MapSet.new()), pos)

  defp copula_pos?(node),
    do: has_pos?(node, "auxiliary") or has_pos?(node, "verb")
end
