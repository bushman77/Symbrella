defmodule Brain.Attribution do
  @moduledoc """
  Deterministic self/other/world target attribution for SI-like text.
  """

  @type target :: :self | :assistant | :user | :other | :system | :world | :unknown

  @second_person MapSet.new(~w(you your you're youre u ur))
  @first_person MapSet.new(~w(i me my mine i'm im myself))
  @third_person MapSet.new(~w(he him his she her hers they them their theirs someone somebody))
  @system_terms MapSet.new(~w(system app application service server pipeline runtime process))
  @world_terms MapSet.new(~w(world environment society internet network))

  @spec target(String.t() | [String.t()] | nil, keyword()) :: target()
  def target(input, opts \\ []) do
    classify(input, opts).target
  end

  def classify(input, opts \\ []) do
    words =
      case input do
        words when is_list(words) -> Enum.map(words, &normalize_word/1)
        text when is_binary(text) -> extract_words(text)
        _ -> []
      end

    evidence = evidence_for(words, opts)
    winner = List.first(evidence)

    %{
      target: if(winner, do: target_for_source(winner.source), else: :unknown),
      source: if(winner, do: winner.source, else: :none),
      confidence: if(winner, do: winner.confidence, else: 0.0),
      evidence: evidence,
      version: 1
    }
  end

  defp self_names(opts) do
    opts
    |> Keyword.get(:self_names, Application.get_env(:brain, :self_names, ["symbrella"]))
    |> List.wrap()
    |> Enum.map(&normalize_word/1)
    |> Enum.reject(&(&1 == ""))
    |> MapSet.new()
  end

  defp evidence_for(words, opts) do
    self_names = self_names(opts)

    []
    |> add_matches(words, self_names, :self_name, 1.0)
    |> add_matches(words, @second_person, :second_person, 0.9)
    |> add_matches(words, @third_person, :third_person, 0.8)
    |> add_matches(words, @first_person, :first_person, 0.8)
    |> add_matches(words, @system_terms, :system_term, 0.75)
    |> add_matches(words, @world_terms, :world_term, 0.7)
    |> Enum.reverse()
  end

  defp add_matches(acc, words, terms, source, confidence) do
    matches =
      words
      |> Enum.filter(&MapSet.member?(terms, &1))
      |> Enum.uniq()
      |> Enum.map(fn term ->
        %{term: term, source: source, confidence: confidence}
      end)

    matches ++ acc
  end

  defp target_for_source(:self_name), do: :self
  defp target_for_source(:second_person), do: :assistant
  defp target_for_source(:first_person), do: :user
  defp target_for_source(:third_person), do: :other
  defp target_for_source(:system_term), do: :system
  defp target_for_source(:world_term), do: :world
  defp target_for_source(_), do: :unknown

  defp extract_words(text) do
    Regex.scan(~r/[[:alpha:]]+(?:'[[:alpha:]]+)?/u, String.downcase(text))
    |> List.flatten()
    |> Enum.map(&normalize_word/1)
  end

  defp normalize_word(word) when is_binary(word) do
    word
    |> String.trim()
    |> String.downcase()
  end

  defp normalize_word(word), do: word |> to_string() |> normalize_word()
end
