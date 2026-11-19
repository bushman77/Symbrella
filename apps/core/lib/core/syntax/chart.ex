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
    sorted = Enum.sort_by(nodes, &Map.get(&1, :index, -1))
    indices = Enum.map(sorted, &Map.get(&1, :index))
    sentence = Map.get(si, :sentence, "")

    reason =
      cond do
        Map.get(si, :source) != :colony_external -> :unsupported_source
        not valid_original_tokens?(si, sorted) -> :coverage_gap
        true -> rejection_reason(sentence, sorted, indices, limits.tokens)
      end

    if reason do
      abstain(reason, 0)
    else
      case fill(sorted, possessive, limits) do
        {:error, :chart_budget_exceeded, count} ->
          abstain(:chart_budget_exceeded, count)

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

  defp abstain(reason, count),
    do: %{status: :abstained, reason: reason, signatures: [],
      chart_items: count, factual_authority?: false}

  defp rejection_reason(sentence, nodes, indices, max_tokens) do
    words = Enum.map(nodes, & &1.word)
    cond do
      not is_binary(sentence) -> :invalid_input
      indices == [] -> :no_clause_core
      length(indices) > max_tokens -> :chart_budget_exceeded
      indices != Enum.to_list(0..(length(indices) - 1)) -> :coverage_gap
      Enum.any?(nodes, &(not is_binary(Map.get(&1, :surface)))) -> :coverage_gap
      not Boundaries.valid_window?(Enum.map(nodes,
        &%{phrase: &1.surface, index: &1.index})) -> :coverage_gap
      Regex.match?(~r/\A\s*[\p{L}\p{M}][\p{L}\p{M} ]{0,32}:\s*\S/u, sentence) and
        not Enum.any?(words, &Grammar.copula?/1) -> :structured_record
      Regex.match?(~r/[,;:]|\s[-—–]\s|\n/u, sentence) -> :multiple_clauses
      String.ends_with?(String.trim(sentence), "?") or
        Grammar.copula?(hd(words)) -> :interrogative
      Enum.any?(words, &Grammar.negative?/1) -> :negation
      Enum.any?(words, &Grammar.modal?/1) -> :modal
      Enum.any?(words, &Grammar.subordinator?/1) -> :instruction_shaped
      not Enum.any?(words, &Grammar.copula?/1) ->
        if verb_initial?(hd(nodes)), do: :instruction_shaped, else: :no_clause_core
      true -> nil
    end
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
      agreement_conflict?(nodes, cop) -> :agreement_conflict
      missing_core_witness?(nodes, cop) -> :lexical_witness_missing
      true -> :coverage_gap
    end
  end

  defp agreement_conflict?(nodes, cop) do
    prior = Enum.filter(nodes, &(&1.index < cop.index))
    head = List.last(prior)

    if is_map(head) and (has_pos?(head, "noun") or has_pos?(head, "pronoun")) do
      subject = if(has_pos?(head, "pronoun"),
        do: Grammar.pronoun_number(head.word),
        else: if(Map.get(head, :plural_noun?, false), do: :plural, else: :singular))
      subject != :unknown and subject != Grammar.agreement(cop.word)
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

  defp fill(nodes, possessive, limits) do
    n = length(nodes)
    seeds =
      (Enum.flat_map(nodes, &terminals/1) ++
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
                  rule <- Grammar.binary_rules(),
                  rule.rhs == [left.kind, right.kind],
                  item <- List.wrap(combine(rule.op, left, right)) do
                item
              end
            end

          case close(initial ++ joined, limits.per_span) do
            {:error, :chart_budget_exceeded} ->
              {:halt, {:error, :chart_budget_exceeded, count}}

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

  defp close(items, limit) do
    Enum.reduce_while(1..6, {:ok, sorted_items(items)}, fn _, {:ok, current} ->
      if length(current) > limit do
        {:halt, {:error, :chart_budget_exceeded}}
      else
        expanded = for item <- current, rule <- Grammar.unary_rules(),
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
      i.reading, i.number, i.copula, i.predicate}
  end

  defp item(kind, first, last, overrides \\ %{}) do
    Map.merge(%{
      kind: kind, first: first, last: last, head: nil, roles: %{},
      required: %{}, modifiers: [], adjuncts: [], reading: [],
      number: :unknown, copula: nil, predicate: nil
    }, overrides)
  end

  defp terminals(node) do
    i = node.index
    make = fn kind -> item(kind, i, i, %{head: i, word: node.word}) end
    noun = if has_pos?(node, "noun"),
      do: [item(:noun, i, i, %{head: i, word: node.word,
        number: if(Map.get(node, :plural_noun?, false), do: :plural, else: :singular)})],
      else: []
    adjective = if has_pos?(node, "adjective"), do: [make.(:adjective)], else: []
    pronoun = if has_pos?(node, "pronoun") and Grammar.pronoun?(node.word),
      do: [item(:pronoun, i, i, %{head: i, word: node.word,
        number: Grammar.pronoun_number(node.word)})], else: []
    proper = if has_pos?(node, "proper_noun"), do: [make.(:proper_noun)], else: []
    determiner = if has_pos?(node, "determiner") and Grammar.determiner?(node.word),
      do: [make.(:determiner)], else: []
    quantifier = if Grammar.quantifier?(node.word), do: [make.(:quantifier)], else: []
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
    preposition = if has_pos?(node, "preposition"), do: [make.(:preposition)], else: []
    adverb = if has_pos?(node, "adverb"), do: [make.(:adverb)], else: []

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

  defp prefix_nominal(modifier, nominal, type) do
    if length(nominal.modifiers) < @max_nominal_modifiers do
      item(:nominal, modifier.first, nominal.last,
        %{head: nominal.head, number: nominal.number,
          modifiers: [{type, modifier.head} | nominal.modifiers]})
    end
  end

  defp combine(:determined_subject, determiner, nominal),
    do: noun_subject(%{nominal | first: determiner.first},
      %{subject_determiner: determiner.head},
      %{subject_determiner: [:pos_determiner]})

  defp combine(:determined_possessive_subject, determiner, subject) do
    if Map.has_key?(subject.roles, :subject_possessor) and
         not Map.has_key?(subject.roles, :subject_determiner) do
      %{subject | first: determiner.first,
        roles: Map.put(subject.roles, :subject_determiner, determiner.head),
        required: Map.put(subject.required, :subject_determiner, [:pos_determiner])}
    end
  end

  defp combine(:quantified_subject, quantifier, nominal),
    do: noun_subject(%{nominal | first: quantifier.first},
      %{subject_quantifier: quantifier.head}, %{})

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

  defp combine(_, _, _), do: nil

  # Unknown subject number cannot license a positive clause.
  defp agreement?(:unknown, _), do: false
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

  defp signature(parse) do
    evidence =
      Enum.concat([
        [:chart_copular_predication, :attested_copula,
          :attested_complement, :source_aligned_full_span, :positive_polarity],
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
