defmodule Core.Semantic.ClauseProposition do
  @moduledoc """
  P-039: map one `:finite_clause` chart signature (rule set `:clause_v1`)
  to a Proposition-shaped, non-factual structural hypothesis.

  Pure and NOT wired into the pipeline; used by the R-004 S-1 harness only.

  Two role layers are kept apart (COLLAB §34.3 item 4):

    * `syntactic_roles` — subject / object / by-object constituents, exactly
      as the grammar placed them;
    * `semantic_roles` — agent / patient under the voice mapping of a
      transitive-event hypothesis (active: subject → agent, object →
      patient; passive: subject → patient, by-object → agent, else
      `:unexpressed`). `semantic_role_basis: :voice_mapping_hypothesis`
      records that this is NOT a verb-specific thematic analysis.

  The structure never depends on LIFG. Only the predicate lemma does, through
  a `Core.Semantic.LexicalWitness` `:selected_exact` witness (P-040) and
  `Core.Semantic.VerbLemma`; without that proof the lemma is `nil` with
  `lemma_basis: :unresolved` or `:no_selected_verb_witness`. Nothing here
  verifies a word sense, a referent or a fact.
  """

  alias Core.Semantic.{AssertionBoundary, LexicalWitness, VerbLemma}

  @subject_prefix "subject_"
  @object_prefix "object_"
  @agent_prefix "agent_"

  @spec build(map(), map()) :: {:ok, map()} | :error
  def build(%{} = si, %{id: :finite_clause, roles: %{predicate: predicate} = roles,
                        clause: %{voice: voice} = clause, span: {first, last}})
      when is_integer(predicate) and voice in [:active, :passive] and
             is_integer(first) and is_integer(last) do
    tokens = indexed_tokens(si)

    with surface when is_binary(surface) <- Map.get(tokens, predicate),
         {:ok, subject} <- constituent(roles, @subject_prefix, tokens),
         object <- optional_constituent(roles, @object_prefix, tokens),
         agent_np <- agent_constituent(roles, tokens) do
      {lemma, basis} = lemma(si, predicate, surface, clause)

      {agent, patient} =
        case voice do
          :active -> {subject, object}
          :passive -> {agent_np || :unexpressed, subject}
        end

      {:ok,
       %{
         version: 1,
         type: :event_clause,
         construction: :finite_clause,
         span: {first, last},
         predicate: %{
           surface: surface,
           token_index: predicate,
           lemma: lemma,
           lemma_basis: basis
         },
         syntactic_roles: %{subject: subject, object: object, by_object: agent_np},
         semantic_roles: %{agent: agent, patient: patient},
         semantic_role_basis: :voice_mapping_hypothesis,
         tense: Map.get(clause, :tense),
         aspect: Map.get(clause, :aspect),
         polarity: Map.get(clause, :polarity),
         voice: voice,
         attachments: Map.get(clause, :attachments, []),
         assertion_admissible_svo_v1:
           AssertionBoundary.admissible?(Map.get(si, :sentence) || "", :svo_v1),
         epistemic_status: :structural_hypothesis_not_verified,
         grounding: %{
           grammatical_roles: :chart_clause_rules_v1,
           predicate_lemma: basis,
           referents: :not_resolved,
           external_facts: :not_checked
         },
         factual_authority?: false
       }}
    else
      _ -> :error
    end
  end

  def build(_, _), do: :error

  # -- constituents ----------------------------------------------------------

  defp constituent(roles, prefix, tokens) do
    case optional_constituent(roles, prefix, tokens) do
      %{} = c -> {:ok, c}
      nil -> :error
    end
  end

  defp optional_constituent(roles, prefix, tokens) do
    head = Map.get(roles, head_key(prefix))
    indices = for {key, index} <- roles, prefixed?(key, prefix), is_integer(index), do: index

    if is_integer(head) and indices != [] do
      {lo, hi} = Enum.min_max(indices)
      surface = span_surface(tokens, lo, hi)
      head_surface = Map.get(tokens, head)

      if is_binary(surface) and is_binary(head_surface),
        do: %{head: head, head_surface: head_surface, span: {lo, hi}, surface: surface},
        else: nil
    end
  end

  # The chart records only the preposition and head of a by-agent; the agent
  # NP is everything strictly after "by" up to and including its head.
  defp agent_constituent(roles, tokens) do
    with prep when is_integer(prep) <- Map.get(roles, :agent_preposition),
         head when is_integer(head) and head > prep <- Map.get(roles, :agent_head),
         surface when is_binary(surface) <- span_surface(tokens, prep + 1, head),
         head_surface when is_binary(head_surface) <- Map.get(tokens, head) do
      %{head: head, head_surface: head_surface, span: {prep + 1, head}, surface: surface}
    else
      _ -> nil
    end
  end

  defp head_key(@subject_prefix), do: :subject_head
  defp head_key(@object_prefix), do: :object_head
  defp head_key(@agent_prefix), do: :agent_head

  defp prefixed?(key, prefix) when is_atom(key),
    do: String.starts_with?(Atom.to_string(key), prefix)

  defp prefixed?(_, _), do: false

  defp span_surface(tokens, lo, hi) do
    words = for i <- lo..hi, do: Map.get(tokens, i)

    if Enum.all?(words, &is_binary/1),
      do: words |> Enum.join(" ") |> String.downcase(),
      else: nil
  end

  defp indexed_tokens(si) do
    si
    |> Map.get(:tokens, [])
    |> List.wrap()
    |> Enum.with_index()
    |> Enum.flat_map(fn
      {%{} = token, position} ->
        index = Map.get(token, :index, position)
        phrase = Map.get(token, :phrase)

        if is_integer(index) and is_binary(phrase) and Map.get(token, :n, 1) == 1 and
             Map.get(token, :kind) not in [:punct, "punct"],
           do: [{index, phrase}],
           else: []

      _ ->
        []
    end)
    |> Map.new()
  end

  # -- lemma -------------------------------------------------------------------

  defp lemma(si, index, surface, clause) do
    case LexicalWitness.witness(si, index, "verb", surface, :selected_exact,
           choice_source: :semantic_preferred
         ) do
      {:ok, %{choice: choice, selected_id: id}} -> derive(si, choice, id, surface, clause)
      :error -> {nil, :no_selected_verb_witness}
    end
  end

  # Progressive participles have no VerbLemma proof path in V1.
  defp derive(_si, _choice, _id, _surface, %{aspect: :progressive}), do: {nil, :unresolved}

  # Simple past and passive participles: only an explicit dictionary
  # "simple past (and past participle) of X" gloss counts.
  defp derive(si, choice, _id, _surface, %{voice: voice, tense: tense})
       when voice == :passive or tense == :past do
    case VerbLemma.canonical_past(si, choice) do
      {:ok, base} -> {base, :attested_past_form_of}
      :error -> base_form(si, choice)
    end
  end

  defp derive(si, choice, _id, surface, _clause) do
    case VerbLemma.canonical(si, choice) do
      {:ok, base} ->
        if base != String.downcase(surface),
          do: {base, :attested_present_form_of},
          else: base_form(si, choice)

      :error ->
        {nil, :unresolved}
    end
  end

  # A selected verb row without any form-of tag is its own base form
  # ("did not chase", "the dogs chase"). Anything else stays unresolved.
  defp base_form(si, choice) do
    id = Map.get(choice, :id) || Map.get(choice, :chosen_id)

    rows =
      si
      |> Map.get(:active_cells, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :id) == id))

    with [_ | _] <- rows,
         true <- Enum.all?(rows, &("tag:form-of" not in List.wrap(Map.get(&1, :semantic_atoms)))),
         [norm, "verb", _] <- String.split(id, "|", parts: 3) do
      {String.downcase(norm), :base_form}
    else
      _ -> {nil, :unresolved}
    end
  end
end
