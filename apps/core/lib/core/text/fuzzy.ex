defmodule Core.Text.Fuzzy do
  @moduledoc """
  Inspectable fuzzy text repair for pre-intent and recall boundaries.

  This module intentionally returns corrected words and evidence, not character
  n-gram tokens. It is safe to use before symbolic intent selection, recall, and
  search helpers; do not feed its internal distance mechanics into LIFG.
  """

  @type correction :: %{
          original: String.t(),
          replacement: String.t(),
          confidence: float(),
          reason: atom()
        }

  @type interpretation :: %{
          original: String.t(),
          normalized: String.t(),
          text: String.t(),
          corrections: [correction()],
          aliases: [atom()],
          confidence: float(),
          evidence: [map()]
        }

  @known_corrections %{
    "adress" => "address",
    "agian" => "again",
    "answr" => "answer",
    "bcuz" => "because",
    "becuase" => "because",
    "beleive" => "believe",
    "cal" => "call",
    "cant" => "cannot",
    "cna" => "can",
    "colur" => "color",
    "couldnt" => "could not",
    "definately" => "definitely",
    "defintely" => "definitely",
    "didnt" => "did not",
    "doesnt" => "does not",
    "ddo" => "do",
    "dont" => "do not",
    "exlain" => "explain",
    "exlpain" => "explain",
    "fav" => "favorite",
    "favourite" => "favorite",
    "gonna" => "going to",
    "gotta" => "got to",
    "hav" => "have",
    "helo" => "hello",
    "helllo" => "hello",
    "idk" => "i do not know",
    "im" => "i am",
    "ive" => "i have",
    "knwo" => "know",
    "kno" => "know",
    "liv" => "live",
    "locaton" => "location",
    "memry" => "memory",
    "memeory" => "memory",
    "msg" => "message",
    "nmae" => "name",
    "naem" => "name",
    "plese" => "please",
    "pleae" => "please",
    "pls" => "please",
    "plz" => "please",
    "quitiapine" => "quetiapine",
    "quietapine" => "quetiapine",
    "recrod" => "record",
    "recal" => "recall",
    "remeber" => "remember",
    "rember" => "remember",
    "remmeber" => "remember",
    "remembber" => "remember",
    "rmeember" => "remember",
    "sotre" => "store",
    "teh" => "the",
    "thier" => "their",
    "thign" => "thing",
    "thnks" => "thanks",
    "thx" => "thanks",
    "transalte" => "translate",
    "tranlsate" => "translate",
    "u" => "you",
    "waht" => "what",
    "wahts" => "whats",
    "wat" => "what",
    "wats" => "whats",
    "wer" => "where",
    "wher" => "where",
    "whre" => "where",
    "whta" => "what",
    "wht" => "what",
    "wont" => "will not",
    "wouldnt" => "would not",
    "wut" => "what",
    "ya" => "you",
    "youre" => "you are",
    "yuor" => "your",
    "yuo" => "you"
  }

  @lexicon ~w(
    about address again am are answer bad because believe blue brain call can
    color could did do does drugs english explain favorite fix forgot have hello
    help hippocampus home homework house how i in is issue know live location me
    mean memory message mix my name place please quetiapine recall remember
    richmond risks run save show sister sleep sleeping state store tell test
    thanks that the their there thing think this time to translate trouble what whats
    when where who why will you your
  )

  @context_frames [
    %{pattern: ["new", :slot], prefers: ~w(place house home apartment)},
    %{pattern: ["my", "new", :slot], prefers: ~w(place house home apartment)},
    %{pattern: ["move", "into", :slot], prefers: ~w(place house home apartment)},
    %{pattern: ["move", "into", "my", :slot], prefers: ~w(place house home apartment)},
    %{pattern: ["paid", "for", :slot], prefers: ~w(place house home apartment)},
    %{pattern: ["math", :slot], prefers: ~w(test homework exam)},
    %{pattern: ["help", "me", "with", :slot], prefers: []},
    %{pattern: ["can", "you", "help", "me", "with", :slot], prefers: []}
  ]

  @identity_query_phrases [
    "what is my name",
    "whats my name",
    "what's my name",
    "what was my name",
    "what name do you have for me",
    "what do you call me",
    "what should you call me",
    "tell me my name",
    "do you know who i am",
    "do you know my name",
    "do u know my name",
    "who am i"
  ]

  @fact_query_phrases [
    "what is my favorite color",
    "what is my address",
    "what is my location",
    "what do you remember about me",
    "what did i tell you",
    "what did i say",
    "what do you know about me"
  ]

  @location_query_phrases [
    "where do i live",
    "where am i from",
    "what is my location",
    "what is my address"
  ]

  @definition_query_phrases [
    "what does this mean",
    "what does it mean",
    "what is the meaning",
    "define this"
  ]

  @doc """
  Normalize, repair small typos, and attach fuzzy aliases/evidence.
  """
  @spec interpret(term(), Keyword.t()) :: interpretation()
  def interpret(raw, opts \\ []) do
    original = if is_binary(raw), do: raw, else: to_string(raw || "")

    normalized =
      original
      |> Core.Text.normalize()
      |> String.downcase()
      |> String.replace(~r/[^\p{L}\p{N}'\s\?]/u, " ")
      |> String.replace(~r/(\p{L})\1{2,}/u, "\\1\\1")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    {text, corrections} = correct_text(normalized, opts)
    aliases = aliases_for(text)
    confidence = confidence(corrections, aliases)

    %{
      original: original,
      normalized: normalized,
      text: text,
      corrections: corrections,
      aliases: aliases,
      confidence: confidence,
      evidence: evidence(corrections, aliases)
    }
  end

  @doc """
  Convenience wrapper returning only the corrected cue text.
  """
  @spec normalize(term(), Keyword.t()) :: String.t()
  def normalize(raw, opts \\ []), do: interpret(raw, opts).text

  @doc """
  True when the text fuzzily maps to a known alias.
  """
  @spec alias?(term(), atom()) :: boolean()
  def alias?(raw, alias_name) when is_atom(alias_name) do
    raw
    |> interpret()
    |> Map.get(:aliases, [])
    |> Enum.member?(alias_name)
  end

  defp correct_text("", _opts), do: {"", []}

  defp correct_text(text, opts) do
    max_distance = Keyword.get(opts, :max_distance, 2)
    words = String.split(text, " ", trim: true)
    bare_words = Enum.map(words, &bare_word/1)

    words
    |> Enum.with_index()
    |> Enum.map(fn {word, index} -> correct_word(word, index, bare_words, max_distance, opts) end)
    |> Enum.reduce({[], []}, fn {word, correction}, {words, corrections} ->
      corrections = if correction, do: corrections ++ [correction], else: corrections
      {words ++ [word], corrections}
    end)
    |> then(fn {words, corrections} -> {Enum.join(words, " "), corrections} end)
  end

  defp correct_word(word, index, words, max_distance, opts) do
    bare = bare_word(word)
    protected = Keyword.get(opts, :protected_words, MapSet.new([]))

    cond do
      bare == "" ->
        {word, nil}

      known_word?(bare, opts) ->
        {word, nil}

      bare in @lexicon ->
        {word, nil}

      replacement = Map.get(@known_corrections, bare) ->
        corrected = preserve_question_mark(word, replacement)
        {corrected, correction(bare, replacement, 0.93, :known_typo)}

      String.length(bare) < 4 ->
        {word, nil}

      MapSet.member?(protected, bare) ->
        {word, nil}

      true ->
        fuzzy_word_match(word, bare, index, words, max_distance, opts)
    end
  end

  defp bare_word(word), do: String.trim(word, "?")

  defp known_word?(word, opts) do
    case Keyword.get(opts, :known_word?) do
      fun when is_function(fun, 1) -> fun.(word)
      _ -> db_word_exists?(word)
    end
  end

  defp db_word_exists?(word) do
    Code.ensure_loaded?(Db) and function_exported?(Db, :word_exists?, 1) and db_started?() and
      apply(Db, :word_exists?, [word])
  end

  defp db_started? do
    Enum.any?(Application.started_applications(), fn {app, _description, _version} ->
      app == :db
    end)
  end

  defp fuzzy_word_match(word, bare, index, words, max_distance, opts) do
    preferred = context_preferred_candidates(words, index)

    (@lexicon ++ preferred)
    |> normalize_candidate_specs()
    |> merge_candidate_specs(lookup_candidate_specs(bare, opts))
    |> Enum.map(fn candidate -> candidate_score(bare, candidate, preferred) end)
    |> Enum.filter(fn {candidate, distance, _score, _reason} ->
      distance > 0 and distance <= max_distance and plausible_distance?(bare, candidate, distance) and
        same_initial?(bare, candidate)
    end)
    |> Enum.sort_by(fn {candidate, distance, score, reason} ->
      {-score, distance, abs(String.length(candidate) - String.length(bare)), reason_rank(reason)}
    end)
    |> clear_winner()
    |> case do
      nil ->
        {word, nil}

      {replacement, _distance, score, reason} ->
        corrected = preserve_question_mark(word, replacement)
        {corrected, correction(bare, replacement, score, reason)}
    end
  end

  defp candidate_score(bare, %{word: candidate} = candidate_spec, preferred) do
    distance = damerau_levenshtein(bare, candidate)
    base = if distance == 1, do: 0.88, else: 0.79
    frame? = Map.get(candidate_spec, :frame?, false) or candidate in preferred
    semantic_score = Map.get(candidate_spec, :context_score, 0.0)

    score =
      base
      |> add_score(if(frame?, do: 0.08, else: 0.0))
      |> add_score(min(semantic_score, 1.0) * 0.06)
      |> min(0.97)

    reason =
      cond do
        frame? -> :context_frame
        semantic_score > 0.0 -> :pgvector_context
        true -> :edit_distance
      end

    {candidate, distance, score, reason}
  end

  defp add_score(score, amount), do: score + amount

  defp clear_winner([]), do: nil

  defp clear_winner([winner]), do: winner

  defp clear_winner([{_candidate, _distance, score, _reason} = winner | rest]) do
    {_candidate, _distance, second_score, _reason} = List.first(rest)

    if score - second_score >= 0.06 do
      winner
    end
  end

  defp reason_rank(:context_frame), do: 0
  defp reason_rank(:pgvector_context), do: 1
  defp reason_rank(:edit_distance), do: 2
  defp reason_rank(_), do: 3

  defp lookup_candidate_specs(word, opts) do
    case Keyword.get(opts, :candidate_words) do
      words when is_list(words) ->
        normalize_candidate_specs(words)

      _ ->
        case Keyword.get(opts, :candidate_lookup) do
          fun when is_function(fun, 1) -> word |> fun.() |> normalize_candidate_specs()
          _ -> db_candidate_specs(word, opts)
        end
    end
  end

  defp db_candidate_specs(word, opts) do
    cond do
      not db_started?() ->
        []

      Keyword.has_key?(opts, :context_embedding) and
        Code.ensure_loaded?(Db) and function_exported?(Db, :context_word_candidates, 3) ->
        Db
        |> apply(:context_word_candidates, [
          word,
          Keyword.fetch!(opts, :context_embedding),
          db_candidate_opts(opts)
        ])
        |> normalize_candidate_specs()

      Code.ensure_loaded?(Db) and function_exported?(Db, :fuzzy_word_candidates, 2) ->
        Db
        |> apply(:fuzzy_word_candidates, [word, db_candidate_opts(opts)])
        |> normalize_candidate_specs()

      true ->
        []
    end
  end

  defp db_candidate_opts(opts) do
    [
      limit: Keyword.get(opts, :candidate_limit, 12),
      only_active: Keyword.get(opts, :only_active, true)
    ]
  end

  defp normalize_candidate_specs(words) when is_list(words) do
    words
    |> Enum.map(&normalize_candidate_spec/1)
    |> Enum.reject(&is_nil/1)
    |> merge_candidate_specs([])
  end

  defp normalize_candidate_specs(_), do: []

  defp normalize_candidate_spec(word) when is_binary(word) do
    word = normalize_candidate_word(word)
    if word == "", do: nil, else: %{word: word}
  end

  defp normalize_candidate_spec(%{} = candidate) do
    word =
      candidate
      |> candidate_word()
      |> normalize_candidate_word()

    if word == "" do
      nil
    else
      %{
        word: word,
        context_score: candidate_float(candidate, :context_score),
        spelling_score: candidate_float(candidate, :spelling_score)
      }
    end
  end

  defp normalize_candidate_spec(_), do: nil

  defp normalize_candidate_word(word) when is_binary(word) do
    word
    |> Core.Text.normalize()
    |> String.downcase()
  end

  defp normalize_candidate_word(_), do: ""

  defp candidate_word(%{norm: word}) when is_binary(word), do: word
  defp candidate_word(%{"norm" => word}) when is_binary(word), do: word
  defp candidate_word(%{word: word}) when is_binary(word), do: word
  defp candidate_word(%{"word" => word}) when is_binary(word), do: word
  defp candidate_word(%{replacement: word}) when is_binary(word), do: word
  defp candidate_word(%{"replacement" => word}) when is_binary(word), do: word
  defp candidate_word(_), do: ""

  defp candidate_float(candidate, key) do
    value = Map.get(candidate, key) || Map.get(candidate, Atom.to_string(key)) || 0.0
    if is_number(value), do: value * 1.0, else: 0.0
  end

  defp merge_candidate_specs(left, right) do
    (left ++ right)
    |> Enum.reduce(%{}, fn %{word: word} = candidate, acc ->
      Map.update(acc, word, candidate, &merge_candidate_spec(&1, candidate))
    end)
    |> Map.values()
  end

  defp merge_candidate_spec(left, right) do
    %{
      word: left.word,
      frame?: Map.get(left, :frame?, false) or Map.get(right, :frame?, false),
      context_score: max(Map.get(left, :context_score, 0.0), Map.get(right, :context_score, 0.0)),
      spelling_score:
        max(Map.get(left, :spelling_score, 0.0), Map.get(right, :spelling_score, 0.0))
    }
  end

  defp context_preferred_candidates(words, index) do
    @context_frames
    |> Enum.filter(&frame_matches?(&1.pattern, words, index))
    |> Enum.flat_map(& &1.prefers)
    |> Enum.uniq()
  end

  defp frame_matches?(pattern, words, index) do
    slot_index = Enum.find_index(pattern, &(&1 == :slot))

    if is_integer(slot_index) do
      start_index = index - slot_index

      start_index >= 0 and start_index + length(pattern) <= length(words) and
        pattern
        |> Enum.with_index()
        |> Enum.all?(fn
          {:slot, _offset} -> true
          {expected, offset} -> Enum.at(words, start_index + offset) == expected
        end)
    else
      false
    end
  end

  defp plausible_distance?(word, candidate, 1),
    do: abs(String.length(word) - String.length(candidate)) <= 1

  defp plausible_distance?(word, candidate, 2),
    do: min(String.length(word), String.length(candidate)) >= 5

  defp plausible_distance?(_word, _candidate, _distance), do: false

  defp same_initial?(left, right) do
    String.first(left) == String.first(right) or
      (String.length(left) >= 4 and String.length(right) >= 4 and
         String.at(left, 1) == String.first(right) and
         String.first(left) == String.at(right, 1))
  end

  defp preserve_question_mark(word, replacement) do
    if String.ends_with?(word, "?"), do: replacement <> "?", else: replacement
  end

  defp correction(original, replacement, confidence, reason) do
    %{
      original: original,
      replacement: replacement,
      confidence: confidence,
      reason: reason
    }
  end

  defp aliases_for(text) do
    []
    |> maybe_alias(identity_query?(text), :asking_for_user_name)
    |> maybe_alias(memory_write?(text), :memory_write)
    |> maybe_alias(fact_query?(text), :fact_query)
    |> maybe_alias(location_query?(text), :location_query)
    |> maybe_alias(definition_query?(text), :definition_query)
    |> maybe_alias(translation_request?(text), :translation_request)
    |> maybe_alias(help_request?(text), :help_request)
    |> maybe_alias(debug_request?(text), :debug_request)
    |> maybe_alias(health_support?(text), :health_support)
    |> maybe_alias(thanks?(text), :thanks)
  end

  defp identity_query?(text) do
    phrase_match?(text, @identity_query_phrases, 0.86)
  end

  defp memory_write?(text) do
    Regex.match?(~r/^\s*(remember|please remember|save|store|note)\b/u, text)
  end

  defp fact_query?(text) do
    phrase_match?(text, @fact_query_phrases, 0.86) or
      Regex.match?(~r/^\s*what\s+(?:is|was|are|were)\s+my\s+.+\??\s*$/u, text) or
      Regex.match?(~r/^\s*what\s+did\s+i\s+(?:say|tell|mention)\b/u, text)
  end

  defp location_query?(text), do: phrase_match?(text, @location_query_phrases, 0.86)

  defp definition_query?(text) do
    phrase_match?(text, @definition_query_phrases, 0.84) or
      Regex.match?(~r/^\s*(define|what\s+does\s+.+\s+mean)\b/u, text)
  end

  defp translation_request?(text) do
    Regex.match?(~r/\btranslate\b/u, text) or
      Regex.match?(
        ~r/\b(?:to|into)\s+(?:english|spanish|french|german|italian|portuguese)\b/u,
        text
      )
  end

  defp help_request?(text) do
    Regex.match?(~r/^\s*(help|please help|can you help|i need help)\b/u, text)
  end

  defp debug_request?(text) do
    Regex.match?(
      ~r/\b(?:bug|broken|error|failed|failing|fix|issue|not working|does not work)\b/u,
      text
    )
  end

  defp health_support?(text) do
    Regex.match?(
      ~r/\b(?:sleep|sleeping|insomnia|tired|medication|medicine|dose|quetiapine|seroquel)\b/u,
      text
    ) and
      Regex.match?(
        ~r/\b(?:forgot|missed|trouble|cannot|can't|can not|having|need help|unsure)\b/u,
        text
      )
  end

  defp thanks?(text), do: Regex.match?(~r/^\s*(thanks|thank you)\b/u, text)

  defp phrase_match?(text, phrases, threshold) do
    query = String.trim(text, "? ")

    Enum.any?(phrases, fn phrase ->
      query == phrase or phrase_similarity(query, phrase) >= threshold
    end)
  end

  defp maybe_alias(aliases, true, alias_name), do: aliases ++ [alias_name]
  defp maybe_alias(aliases, false, _alias_name), do: aliases

  defp confidence([], []), do: 0.0

  defp confidence(corrections, aliases) do
    correction_conf =
      corrections
      |> Enum.map(& &1.confidence)
      |> Enum.max(fn -> 0.0 end)

    alias_conf = if aliases == [], do: 0.0, else: 0.89
    max(correction_conf, alias_conf)
  end

  defp evidence(corrections, aliases) do
    correction_evidence =
      Enum.map(corrections, fn correction ->
        %{
          role: :fuzzy_correction,
          original: correction.original,
          replacement: correction.replacement,
          confidence: Float.round(correction.confidence, 4),
          reason: correction.reason
        }
      end)

    alias_evidence =
      Enum.map(aliases, fn alias_name ->
        %{role: :fuzzy_alias, alias: alias_name, confidence: 0.89}
      end)

    correction_evidence ++ alias_evidence
  end

  defp phrase_similarity(left, right) do
    distance = damerau_levenshtein(left, right)
    max_len = max(String.length(left), String.length(right))

    if max_len == 0, do: 1.0, else: 1.0 - distance / max_len
  end

  defp damerau_levenshtein(left, right) do
    a = String.graphemes(left)
    b = String.graphemes(right)
    la = length(a)
    lb = length(b)

    cond do
      la == 0 -> lb
      lb == 0 -> la
      true -> damerau_levenshtein(a, b, la, lb)
    end
  end

  defp damerau_levenshtein(a, b, la, lb) do
    initial =
      for i <- 0..la, j <- 0..lb, into: %{} do
        cond do
          i == 0 -> {{i, j}, j}
          j == 0 -> {{i, j}, i}
          true -> {{i, j}, 0}
        end
      end

    matrix =
      Enum.reduce(1..la, initial, fn i, acc ->
        Enum.reduce(1..lb, acc, fn j, acc2 ->
          cost = if Enum.at(a, i - 1) == Enum.at(b, j - 1), do: 0, else: 1

          deletion = Map.fetch!(acc2, {i - 1, j}) + 1
          insertion = Map.fetch!(acc2, {i, j - 1}) + 1
          substitution = Map.fetch!(acc2, {i - 1, j - 1}) + cost

          transposition =
            if i > 1 and j > 1 and Enum.at(a, i - 1) == Enum.at(b, j - 2) and
                 Enum.at(a, i - 2) == Enum.at(b, j - 1) do
              Map.fetch!(acc2, {i - 2, j - 2}) + 1
            else
              la + lb
            end

          Map.put(acc2, {i, j}, Enum.min([deletion, insertion, substitution, transposition]))
        end)
      end)

    Map.fetch!(matrix, {la, lb})
  end
end
