defmodule Core.Syntax.Signatures do
  @moduledoc """
  Read-only grammatical evidence for Core.SemanticInput.

  Uses base tokens and *all* POS/morphological alternatives from active_cells.
  It does not choose BrainCell IDs, assign confidence, or update memory.

  Each signature's `span` is an inclusive range of original token indices.
  A detected construction is a hypothesis, not a semantic conclusion.
  """

  alias Core.Pipeline.Trace

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

    (finite_past(nodes) ++ perfect_predicates(nodes) ++ have_verb_infinitives(nodes))
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
        [%{index: index, word: word, pos: entry.pos, morphology: entry.morphology}]
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
