defmodule Core.Semantic.ProvisionalInterpretation do
  @moduledoc """
  Read-only, non-factual hypotheses about bounded question/fragment surfaces.
  Never verifies a sense, binds a referent, asserts a location, or writes memory.
  """
  alias Core.Pipeline.Trace

  @lexeme ~r/\A[\p{L}][\p{L}\p{M}'-]{0,39}\z/u

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    hypothesis = detect(si)
    si
    |> Map.put(:provisional_interpretation, hypothesis)
    |> Trace.append(:provisional_interpretation,
      decision: if(is_map(hypothesis), do: :provisional, else: :abstained),
      reason: :bounded_original_surface,
      meta: if(is_map(hypothesis), do: %{
        operation: hypothesis.operation,
        target: hypothesis.target.lemma, factual_authority?: false
      }, else: %{}))
  end
  def attach(other), do: other

  @spec detect(map()) :: map() | nil
  def detect(%{} = si) do
    if established?(Map.get(si, :sentence_meaning)) do
      nil
    else
      case spatial_gap_question(si) do
        nil ->
          case original_words(si) do
            {:ok, ["where", "is", article, noun]}
            when article in ["the", "a", "an"] ->
              make(si, :locate_entity, :question, noun, "#{article} #{noun}",
                :location, [:wh_location, :copula, :nominal_surface],
                [:referent_binding, :location])

            {:ok, ["what", "is", article, noun]}
            when article in ["a", "an"] ->
              make(si, :define_concept, :question, noun, "#{article} #{noun}",
                :definition, [:wh_definition, :copula, :indefinite_nominal_surface],
                [:concept_grounding])

            {:ok, [word]} ->
              make(si, :ambiguous_topic, :fragment, word, word, nil,
                [:isolated_lexical_surface], [:speech_act, :requested_information])

            _ -> nil
          end

        hypothesis ->
          hypothesis
      end
    end
  end
  def detect(_), do: nil

  # A V1 hypothesis for a WH question whose requested participant is omitted
  # from a verb-particle construction. The signature supplies grammatical
  # roles; the discourse reference remains a tentative antecedent candidate.
  defp spatial_gap_question(si) do
    tokens = base_tokens(si)
    signatures = List.wrap(Map.get(si, :sentence_signatures))
    reference = Map.get(si, :discourse_reference)

    Enum.find_value(signatures, fn
      %{id: :verb_particle_construction, source: :core_syntax,
        roles: %{verb: verb, particle: particle}, evidence: evidence}
      when is_integer(verb) and is_integer(particle) and is_list(evidence) ->
        with true <- :adjacent_particle in evidence,
             true <- verb == 3 and particle == 4,
             "what" <- token_word(tokens, 0),
             "is" <- token_word(tokens, 1),
             "it" <- token_word(tokens, 2),
             predicate when is_binary(predicate) <- token_word(tokens, verb),
             position when position in ["on", "in", "under", "beside", "above", "below"] <-
               token_word(tokens, particle) do
          antecedent =
            case reference do
              %{source: :recent_user_turn, antecedent: name} when is_binary(name) -> name
              _ -> nil
            end

          %{
            version: 1,
            status: :provisional,
            speech_act: :question,
            operation: :retrieve_spatial_participant,
            target: %{lemma: "it", surface: "it",
              referent_status: if(antecedent, do: :contextual_candidate, else: :unresolved),
              antecedent: antecedent},
            requested_relation: :spatial_location,
            predicate: predicate,
            position: position,
            evidence: [:original_surface, :verb_particle_sentence_signature],
            missing: [:event_grounding] ++ if(antecedent, do: [], else: [:referent_binding]),
            factual_authority?: false,
            source: :sentence_signature_v1,
            original_sentence: Map.get(si, :sentence)
          }
        else
          _ -> nil
        end

      _ -> nil
    end)
  end

  defp base_tokens(si) do
    si
    |> Map.get(:tokens, [])
    |> List.wrap()
    |> Enum.filter(fn token ->
      Map.get(token, :n, 1) == 1 and Map.get(token, :kind) not in [:punct, "punct"]
    end)
    |> Enum.sort_by(&Map.get(&1, :index, -1))
  end

  defp token_word(tokens, index) do
    case Enum.find(tokens, &(Map.get(&1, :index) == index)) do
      %{phrase: phrase} when is_binary(phrase) -> String.downcase(phrase)
      _ -> nil
    end
  end

  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    hypothesis = Map.get(si, :provisional_interpretation)
    if is_map(hypothesis) and hypothesis == detect(si) and
         hypothesis.status == :provisional and
         hypothesis.factual_authority? == false do
      {:ok, hypothesis}
    else
      :not_applicable
    end
  end
  def verified(_), do: :not_applicable

  defp established?(%{status: status})
       when status in [:structurally_interpreted, :verified, :factually_verified],
       do: true
  defp established?(_), do: false

  defp make(si, operation, speech_act, lemma, surface, requested, evidence, missing) do
    %{
      version: 1, status: :provisional, speech_act: speech_act,
      operation: operation,
      target: %{lemma: lemma, surface: surface, referent_status: :unresolved,
        lexical_evidence: noun_witness(si, lemma)},
      requested_relation: requested, evidence: evidence, missing: missing,
      factual_authority?: false, source: :original_surface,
      original_sentence: Map.get(si, :sentence)
    }
  end

  defp noun_witness(si, lemma) do
    found? =
      si
      |> Map.get(:active_cells, [])
      |> List.wrap()
      |> Enum.any?(fn
        %{} = cell ->
          norm = Map.get(cell, :norm) || Map.get(cell, "norm")
          pos = Map.get(cell, :pos) || Map.get(cell, "pos")
          id = Map.get(cell, :id) || Map.get(cell, "id")
          is_binary(norm) and String.downcase(norm) == lemma and
            pos in ["noun", :noun] and is_binary(id) and
            String.starts_with?(id, lemma <> "|noun|")
        _ -> false
      end)
    if found?, do: :noun_pos_attested, else: :not_verified
  end

  defp original_words(si) do
    sentence = Map.get(si, :sentence)
    tokens = Map.get(si, :tokens)
    if is_binary(sentence) and byte_size(sentence) <= 180 and
         is_list(tokens) and tokens != [] do
      base =
        tokens
        |> Enum.filter(fn t ->
          is_map(t) and Map.get(t, :n, 1) == 1 and
            Map.get(t, :kind) not in [:punct, "punct"]
        end)
        |> Enum.sort_by(&Map.get(&1, :index, -1))
      indexes = Enum.map(base, &Map.get(&1, :index))
      words = Enum.map(base, &normalize(Map.get(&1, :phrase)))
      surface =
        sentence
        |> String.trim()
        |> String.replace(~r/[?!.]+\z/u, "")
        |> String.trim()
        |> String.downcase()
        |> String.split(~r/\s+/u, trim: true)
      if length(words) in 1..5 and
           indexes == Enum.to_list(0..(length(words) - 1)) and
           words == surface and
           Enum.all?(words, &(is_binary(&1) and Regex.match?(@lexeme, &1))) do
        {:ok, words}
      else
        :error
      end
    else
      :error
    end
  end

  defp normalize(s) when is_binary(s) do
    s |> String.downcase() |> String.trim()
      |> String.replace(~r/^\p{P}+|\p{P}+\z/u, "")
  end
  defp normalize(_), do: nil
end
