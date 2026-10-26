defmodule Core.Semantic.ConstructionSchema do
  @moduledoc """
  Read-only candidate recognition for a family of lexical-definition requests.

  A construction is a grammatical hypothesis. The whole original token stream
  must match a bounded clause, and its lexical anchors and requested target
  must have exact, current-turn LTM BrainCell witnesses. Imperative candidates
  additionally require the independent Core syntax / LIFG role adjudication.
  This layer never creates a sense ID, chooses a dictionary meaning or answers.
  """

  alias Core.Pipeline.Trace

  @max_tokens 9
  @articles ~w(a an the)
  @eligible_intents [:define, :command, :ask, :ask_info]

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    candidates = detect(si)

    si
    |> Map.put(:construction_candidates, candidates)
    |> Trace.append(:construction_schema,
      decision: if(candidates == [], do: :abstained, else: :candidate),
      reason: :complete_attested_lexical_request,
      meta: %{count: length(candidates), schemas: Enum.map(candidates, & &1.schema)})
  end

  def attach(other), do: other

  @spec detect(map()) :: [map()]
  def detect(%{} = si) do
    with true <- Map.get(si, :intent) in @eligible_intents,
         {:ok, words} <- original_words(si),
         {:ok, schema, target_index, cues} <- parse(words),
         target when is_binary(target) <- Enum.at(words, target_index),
         true <- Enum.all?(cues, fn {index, pos} ->
           witnessed?(si, Enum.at(words, index), pos)
         end),
         true <- target_witnesses(si, target) != [],
         true <- supported_shape?(si, schema, words, target_index) do
      [%{
        version: 1,
        schema: schema,
        operation: :define,
        speech_act: :request,
        span: {0, length(words) - 1},
        target: %{surface: target, lemma: target, index: target_index},
        witnesses: [:complete_original_surface, :attested_lexical_cues,
                    :attested_target, :bounded_grammatical_construction],
        provenance: :core_construction_schema
      }]
    else
      _ -> []
    end
  end

  def detect(_), do: []

  @doc "Return sorted distinct attested POS alternatives, not a chosen sense."
  def target_pos(si, word) do
    si
    |> target_witnesses(word)
    |> Enum.map(&cell(&1, :pos))
    |> Enum.uniq()
    |> Enum.sort()
  end

  @doc "Return exact current-turn LTM rows that identify the requested word."
  def target_witnesses(si, word) when is_map(si) and is_binary(word) do
    si
    |> Map.get(:active_cells, [])
    |> List.wrap()
    |> Enum.filter(fn row ->
      id = cell(row, :id)
      norm = cell(row, :norm)
      pos = cell(row, :pos)

      is_binary(id) and is_binary(norm) and is_binary(pos) and pos != "" and
        cell(row, :source) in [:ltm, "ltm"] and
        String.downcase(norm) == word and
        String.starts_with?(id, word <> "|" <> pos <> "|")
    end)
  end

  def target_witnesses(_, _), do: []

  defp witnessed?(si, word, pos) when is_binary(word) do
    Enum.any?(target_witnesses(si, word), &(cell(&1, :pos) == pos))
  end

  defp witnessed?(_, _, _), do: false

  defp original_words(si) do
    sentence = Map.get(si, :sentence)
    tokens = Map.get(si, :tokens)

    if is_binary(sentence) and byte_size(sentence) <= 260 and
         is_list(tokens) and tokens != [] do
      indexed =
        tokens
        |> Enum.filter(fn t ->
          is_map(t) and Map.get(t, :n, 1) == 1 and
            Map.get(t, :kind) not in [:punct, "punct"]
        end)
        |> Enum.sort_by(&Map.get(&1, :index, -1))

      indexes = Enum.map(indexed, &Map.get(&1, :index))
      words = Enum.map(indexed, &normalize(Map.get(&1, :phrase)))

      surface_words =
        sentence
        |> String.trim()
        |> String.replace(~r/[?!.]+\z/u, "")
        |> String.downcase()
        |> String.split(~r/\s+/u, trim: true)

      if length(words) in 2..@max_tokens and
           indexes == Enum.to_list(0..(length(words) - 1)) and
           words == surface_words and
           Enum.all?(words, &(is_binary(&1) and
             Regex.match?(~r/\A[\p{L}][\p{L}\p{M}'-]{0,39}\z/u, &1))) do
        {:ok, words}
      else
        :error
      end
    else
      :error
    end
  end

  defp normalize(word) when is_binary(word) do
    word
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp normalize(_), do: nil

  # The grammar is factored into reusable optional modifiers and a variable
  # target, rather than a hardcoded response for each surface sentence.
  defp parse(words) do
    {polite, body, offset} =
      case words do
        ["please" | rest] -> {true, rest, 1}
        rest -> {false, rest, 0}
      end

    case body do
      ["define" | object] ->
        case object do
          [term] when is_binary(term) ->
            {:ok, :definition_imperative, offset + 1, [{offset, "verb"}]}

          [article, term] when article in @articles and is_binary(term) ->
            {:ok, :definition_imperative, offset + 2, [{offset, "verb"},
              {offset + 1, "determiner"}]}

          _ ->
            :error
        end

      ["tell", "me", "the", "meaning", "of", term] when is_binary(term) ->
        {:ok, :meaning_request_imperative, offset + 5,
         [{offset, "verb"}, {offset + 3, "noun"}, {offset + 4, "preposition"}]}

      ["what", "does", term, "mean"] when not polite and is_binary(term) ->
        {:ok, :meaning_wh_question, 2, [{3, "verb"}]}

      ["what", "is", "the", "meaning", "of", term] when not polite and is_binary(term) ->
        {:ok, :meaning_wh_question, 5, [{3, "noun"}, {4, "preposition"}]}

      ["what", "is", "the", "definition", "of", term]
      when not polite and is_binary(term) ->
        {:ok, :meaning_wh_question, 5, [{3, "noun"}, {4, "preposition"}]}

      _ ->
        :error
    end
  end

  # Recheck exact independent pre-LIFG predicate/object roles; intent alone
  # cannot elevate a guessed imperative to an interpreted request.
  defp supported_shape?(si, :definition_imperative, words, target) do
    first = if hd(words) == "please", do: 1, else: 0
    signatures = List.wrap(Map.get(si, :sentence_signatures))
    roles = get_in(si, [:role_competition, :roles]) || %{}

    Enum.any?(signatures, fn
      %{id: :definition_imperative, source: :core_syntax,
        span: {0, last}, roles: sig_roles, evidence: evidence}
      when is_map(sig_roles) and is_list(evidence) ->
        last == length(words) - 1 and
          Map.get(sig_roles, :predicate) == first and
          Map.get(sig_roles, :object_head) == target and
          Enum.all?([:complete_imperative_surface, :explicit_definition_command,
                     :attested_verb_alternative, :attested_common_noun_object],
                    &(&1 in evidence)) and
          match?(%{status: :supported, pos: "verb", role: :predicate},
            Map.get(roles, first)) and
          match?(%{status: :supported, pos: "noun", role: :object_head},
            Map.get(roles, target))
      _ -> false
    end)
  end

  defp supported_shape?(_si, schema, _words, _target)
       when schema in [:meaning_request_imperative, :meaning_wh_question], do: true

  defp supported_shape?(_, _, _, _), do: false

  defp cell(%{} = row, key), do: Map.get(row, key, Map.get(row, Atom.to_string(key)))
  defp cell(_, _), do: nil
end
