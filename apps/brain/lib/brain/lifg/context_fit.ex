defmodule Brain.LIFG.ContextFit do
  @moduledoc """
  Deterministic candidate-specific semantic context scoring for LIFG.

  ContextFit compares the surrounding sentence context with lexical evidence
  attached to one exact dictionary sense.

  It does not choose a winner and it does not mutate cognitive state.

  Its job is deliberately narrow:

      sentence context + exact-sense evidence -> score in 0.0..1.0

  Evidence currently considered:

    * definition      — strongest signal
    * example         — contextual usage signal
    * synonyms        — supporting semantic signal
    * semantic_atoms  — domain/tag signal
    * gram_function   — weak grammatical signal

  Antonyms are retained in diagnostics but are not currently used as a
  negative score. Co-occurrence with an antonym does not necessarily imply
  that a sense is wrong.

  This module intentionally uses deterministic lexical overlap first.
  Embeddings or learned semantic scoring can later be added as separate
  features without replacing the explicit evidence path.
  """

  alias Brain.Utils.Safe

  @default_weights %{
    definition: 0.58,
    example: 0.18,
    synonyms: 0.12,
    semantic_atoms: 0.08,
    gram_function: 0.04
  }

  # Common grammatical/scaffolding words are poor semantic discriminators.
  #
  # Stage1 already has separate machinery for POS/function-word behavior,
  # so ContextFit should concentrate on semantic content.
  # "about" is a relation marker in topic questions, not discriminating
  # evidence that a particular dictionary sense of "know" is correct.
  @stopwords MapSet.new(~w(
    a about an and are as at be been being but by
    can could did do does for from had has have
    he her hers him his how i if in into is it its
    may me might mine must my nor not of on or our ours
    she should so some than that the their theirs them
    then there these they this those to too us was we
    were what when where which who whom whose why will
    with would you your yours
  ))

  @token_re ~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u

  @type explanation :: %{
          score: float(),
          context_terms: [String.t()],
          matches: map(),
          component_scores: map(),
          contributions: map(),
          weights: map(),
          antonym_matches: [String.t()]
        }

  @doc """
  Return candidate-specific context fit in the range `0.0..1.0`.

  `sentence` is the complete sentence or local textual context.

  `target` is the surface word being disambiguated and is removed from the
  context terms before comparison so that every sense does not receive free
  credit merely for containing the target word.

  `candidate` is the exact-sense candidate map/struct.

  ## Example

      ContextFit.score(
        "I sat on the bank beside the river.",
        "bank",
        candidate
      )
  """
  @spec score(String.t() | nil, String.t() | nil, map(), keyword()) :: float()
  def score(sentence, target, candidate, opts \\ []) do
    explain(sentence, target, candidate, opts).score
  end

  @doc """
  Return the context score plus its deterministic evidence breakdown.

  This is primarily useful for tests, telemetry, and debugging why one sense
  scored above another.
  """
  @spec explain(String.t() | nil, String.t() | nil, map(), keyword()) :: explanation()
  def explain(sentence, target, candidate, opts \\ [])

  def explain(sentence, target, candidate, opts)
      when is_map(candidate) and is_list(opts) do
    context =
      context_terms(sentence, target)
      |> MapSet.union(additional_context_terms(Keyword.get(opts, :extra_context, [])))

    evidence = evidence_terms(candidate)
    weights = effective_weights(opts)

    related_morphology? = Keyword.get(opts, :related_morphology?, false) == true

    matches = %{
      definition: overlap(context, evidence.definition, related_morphology?),
      example: overlap(context, evidence.example, related_morphology?),
      synonyms: overlap(context, evidence.synonyms, related_morphology?),
      semantic_atoms: overlap(context, evidence.semantic_atoms, related_morphology?),
      gram_function: overlap(context, evidence.gram_function, related_morphology?)
    }

    component_scores =
      Map.new(matches, fn {field, terms} ->
        # Related word-family matches contribute half the incremental evidence
        # of an exact token match, rather than masquerading as exact identity.
        exact_terms = overlap(context, Map.fetch!(evidence, field))
        exact_strength = overlap_strength(exact_terms)
        total_strength = overlap_strength(terms)

        {field, exact_strength + 0.5 * max(total_strength - exact_strength, 0.0)}
      end)

    contributions =
      Map.new(@default_weights, fn {field, _default_weight} ->
        weight = Map.get(weights, field, 0.0)
        component = Map.get(component_scores, field, 0.0)

        {field, weight * component}
      end)

    score =
      contributions
      |> Map.values()
      |> Enum.sum()
      |> clamp01()

    %{
      score: round_score(score),
      context_terms: context |> MapSet.to_list() |> Enum.sort(),
      matches: matches,
      component_scores: round_map(component_scores),
      contributions: round_map(contributions),
      weights: weights,
      antonym_matches: overlap(context, evidence.antonyms)
    }
  end

  def explain(_sentence, _target, _candidate, _opts) do
    %{
      score: 0.0,
      context_terms: [],
      matches: empty_match_map(),
      component_scores: empty_score_map(),
      contributions: empty_score_map(),
      weights: @default_weights,
      antonym_matches: []
    }
  end

  # ---------------------------------------------------------------------------
  # Context extraction
  # ---------------------------------------------------------------------------

  defp context_terms(sentence, target) do
    words = raw_tokens(sentence)
    target_words = raw_tokens(target)

    # Locate the target phrase in the sentence.
    start =
      words
      |> Enum.with_index()
      |> Enum.find_value(fn {_word, index} ->
        if target_words != [] and
             Enum.slice(words, index, length(target_words)) == target_words do
          index
        end
      end)

    # Keep up to three words before and after the target.
    # Fall back to existing whole-sentence behavior if
    # the target cannot be located.
    local_words =
      if is_integer(start) do
        left = max(start - 3, 0)
        right = min(start + length(target_words) + 2, length(words) - 1)

        Enum.slice(words, left, right - left + 1)
      else
        words
      end

    excluded = MapSet.new(target_words)

    local_words
    |> reject_stopwords()
    |> Enum.reject(&MapSet.member?(excluded, &1))
    |> MapSet.new()
  end

  # A selected noun object's dictionary gloss may provide type evidence
  # (e.g. "fan" -> "device") absent from the literal sentence. This is
  # opt-in, bounded, and never expands from unselected candidate senses.
  defp additional_context_terms(values) do
    values
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.take(2)
    |> Enum.flat_map(&raw_tokens/1)
    |> reject_stopwords()
    |> Enum.take(28)
    |> MapSet.new()
  end

  # ---------------------------------------------------------------------------
  # Candidate evidence
  # ---------------------------------------------------------------------------

  defp evidence_terms(candidate) do
    %{
      definition:
        candidate
        |> Safe.get(:definition)
        |> content_terms(),
      example:
        candidate
        |> Safe.get(:example)
        |> content_terms(),
      synonyms:
        candidate
        |> Safe.get(:synonyms, [])
        |> list_content_terms(),
      antonyms:
        candidate
        |> Safe.get(:antonyms, [])
        |> list_content_terms(),
      semantic_atoms:
        candidate
        |> Safe.get(:semantic_atoms, [])
        |> semantic_atom_terms(),
      gram_function:
        candidate
        |> Safe.get(:gram_function, [])
        |> list_content_terms()
    }
  end

  defp content_terms(text) do
    text
    |> raw_tokens()
    |> reject_stopwords()
    |> MapSet.new()
  end

  defp list_content_terms(value) do
    value
    |> Safe.ensure_list()
    |> Enum.flat_map(fn
      v when is_binary(v) -> raw_tokens(v)
      v when is_atom(v) -> raw_tokens(Atom.to_string(v))
      _ -> []
    end)
    |> reject_stopwords()
    |> MapSet.new()
  end

  # Semantic atoms contain both useful domain information and bookkeeping.
  #
  # Useful:
  #   cat:en:hydrology -> hydrology
  #   cat:en:computing -> computing
  #   tag:countable    -> countable
  #
  # Not useful for lexical context:
  #   ety:2
  #   pos_raw:noun
  #   cat:english terms with usage examples
  defp semantic_atom_terms(value) do
    value
    |> Safe.ensure_list()
    |> Enum.flat_map(&semantic_atom_to_terms/1)
    |> reject_stopwords()
    |> MapSet.new()
  end

  defp semantic_atom_to_terms(atom) when is_binary(atom) do
    atom = String.trim(atom)

    cond do
      String.starts_with?(atom, "cat:en:") ->
        atom
        |> String.replace_prefix("cat:en:", "")
        |> raw_tokens()

      String.starts_with?(atom, "domain:") ->
        atom
        |> String.replace_prefix("domain:", "")
        |> raw_tokens()

      String.starts_with?(atom, "topic:") ->
        atom
        |> String.replace_prefix("topic:", "")
        |> raw_tokens()

      String.starts_with?(atom, "tag:") ->
        atom
        |> String.replace_prefix("tag:", "")
        |> raw_tokens()

      true ->
        []
    end
  end

  defp semantic_atom_to_terms(_), do: []

  # ---------------------------------------------------------------------------
  # Matching
  # ---------------------------------------------------------------------------

  defp overlap(context, evidence), do: overlap(context, evidence, false)

  defp overlap(%MapSet{} = context, %MapSet{} = evidence, related_morphology?) do
    evidence
    |> MapSet.to_list()
    |> Enum.filter(fn term ->
      MapSet.member?(context, term) or
        (related_morphology? and
           Enum.any?(context, &related_word_form?(&1, term)))
    end)
    |> Enum.sort()
  end

  # A low-weight lexical bridge for productive word-family variants such as
  # "electronic"/"electrical" or "computer"/"computing".
  # It is opt-in, used by structural dictionary verification only. Similar
  # spelling is evidence, not proof of equivalent word senses.
  defp related_word_form?(a, b) when is_binary(a) and is_binary(b) do
    String.length(a) >= 8 and String.length(b) >= 8 and
      String.slice(a, 0, 6) == String.slice(b, 0, 6) and
      Regex.match?(~r/^\p{L}+$/u, a) and Regex.match?(~r/^\p{L}+$/u, b)
  end

  defp related_word_form?(_, _), do: false

  # A single strong lexical hit should matter.
  #
  # 0 matches -> 0.000
  # 1 match   -> 0.500
  # 2 matches -> 0.750
  # 3 matches -> 0.875
  # 4 matches -> 0.938
  #
  # This deliberately saturates instead of dividing by definition length.
  # Dictionary definitions vary substantially in verbosity; a concise exact
  # cue such as "river" should not be punished because another definition is
  # much longer.
  defp overlap_strength(matches) when is_list(matches) do
    case length(matches) do
      0 ->
        0.0

      n ->
        1.0 - :math.pow(0.5, n)
    end
  end

  # ---------------------------------------------------------------------------
  # Tokenization
  # ---------------------------------------------------------------------------

  defp raw_tokens(text) when is_binary(text) do
    text
    |> String.downcase()
    |> then(&Regex.scan(@token_re, &1))
    |> Enum.map(fn
      [token | _] -> normalize_token(token)
      _ -> ""
    end)
    |> Enum.reject(&(&1 == ""))
  end

  defp raw_tokens(_), do: []

  defp normalize_token(token) when is_binary(token) do
    normalized =
      token
      |> String.trim()
      |> String.replace("’", "'")

    # Lightweight, deterministic English inflection matching. This is
    # lexical *comparison* only: dictionary IDs and original token surfaces
    # are never rewritten. "lights" must match "light" in a sense gloss.
    cond do
      String.length(normalized) > 5 and String.ends_with?(normalized, "ies") ->
        String.replace_suffix(normalized, "ies", "y")

      String.length(normalized) > 5 and
          Enum.any?(["sses", "ches", "shes", "xes", "zes"], &String.ends_with?(normalized, &1)) ->
        String.slice(normalized, 0, String.length(normalized) - 2)

      String.length(normalized) > 4 and String.ends_with?(normalized, "s") and
          not String.ends_with?(normalized, "ss") ->
        String.slice(normalized, 0, String.length(normalized) - 1)

      true ->
        normalized
    end
  end

  defp reject_stopwords(terms) do
    Enum.reject(terms, &MapSet.member?(@stopwords, &1))
  end

  # ---------------------------------------------------------------------------
  # Configuration / numeric helpers
  # ---------------------------------------------------------------------------

  defp effective_weights(opts) do
    overrides =
      case Keyword.get(opts, :weights, %{}) do
        %{} = map ->
          map

        list when is_list(list) ->
          Map.new(list)

        _ ->
          %{}
      end

    @default_weights
    |> Map.merge(overrides)
    |> Map.new(fn {field, value} ->
      {field, nonnegative_float(value)}
    end)
    |> normalize_weights()
  end

  defp normalize_weights(weights) do
    total =
      weights
      |> Map.values()
      |> Enum.sum()

    if total > 0.0 do
      Map.new(weights, fn {field, weight} ->
        {field, weight / total}
      end)
    else
      @default_weights
    end
  end

  defp nonnegative_float(v) when is_float(v), do: max(v, 0.0)
  defp nonnegative_float(v) when is_integer(v), do: max(v * 1.0, 0.0)
  defp nonnegative_float(_), do: 0.0

  defp clamp01(v) when is_number(v) do
    v
    |> max(0.0)
    |> min(1.0)
  end

  defp clamp01(_), do: 0.0

  defp round_score(v), do: Float.round(v * 1.0, 6)

  defp round_map(map) do
    Map.new(map, fn {key, value} ->
      {key, round_score(value)}
    end)
  end

  defp empty_match_map do
    %{
      definition: [],
      example: [],
      synonyms: [],
      semantic_atoms: [],
      gram_function: []
    }
  end

  defp empty_score_map do
    %{
      definition: 0.0,
      example: 0.0,
      synonyms: 0.0,
      semantic_atoms: 0.0,
      gram_function: 0.0
    }
  end
end
