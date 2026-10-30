defmodule Brain.LIFG.UsageRegister do
  @moduledoc """
  Small, explicitly evidenced usage-register prior for exact dictionary senses.

  Only leading lexicographic labels in the definition (e.g. "(obsolete)")
  indicate restricted historical usage. An incidental occurrence of "obsolete"
  in an ordinary gloss is not a usage label. "Regional" and "dialectal" alone
  are not obsolete and receive no penalty.

  The prior is applied only when the *same word and POS* has another attested,
  unmarked, nonempty definition and the sentence does not explicitly discuss
  historical language. It never filters, fabricates, or rewrites a sense.
  """

  @historical_labels MapSet.new(~w(obsolete archaic dated historical))
  @historical_context ~r/\b(?:obsolete|archaic|historical|historic|etymology|old-fashioned|dated|medieval|old english)\b/ui
  @default_penalty 0.06

  @doc """
  Read-only, bounded logit adjustment for explicitly historical senses.

  Takes actual candidate maps and their current pre-softmax score map.
  Returns the same IDs, with a small penalty and an audit of the exact labels.
  """
  @spec adjust(map(), [map()], String.t() | nil, keyword()) :: {map(), map()}
  def adjust(scores, candidates, sentence, opts \\ [])

  def adjust(scores, candidates, sentence, opts)
      when is_map(scores) and is_list(candidates) and is_list(opts) do
    enabled? = Keyword.get(opts, :enabled?, true) == true
    penalty = bounded_penalty(Keyword.get(opts, :penalty, @default_penalty))
    historical_context? =
      is_binary(sentence) and String.match?(sentence, @historical_context)

    if not enabled? or penalty == 0.0 or historical_context? do
      {scores, %{applied?: false,
                 reason: if(historical_context?, do: :historical_context,
                   else: :disabled), adjustments: []}}
    else
      entries =
        candidates
        |> Enum.filter(&is_map/1)
        |> Enum.map(fn candidate ->
          %{
            id: field(candidate, :id),
            pos: normalize(field(candidate, :pos)),
            norm: normalize(field(candidate, :norm) || field(candidate, :lemma) ||
              field(candidate, :word)),
            definition: field(candidate, :definition),
            labels: historical_labels(field(candidate, :definition))
          }
        end)
        |> Enum.filter(fn entry ->
          is_binary(entry.id) and entry.id != "" and
            Map.has_key?(scores, entry.id) and
            is_binary(entry.definition) and String.trim(entry.definition) != "" and
            entry.pos != "" and entry.norm != ""
        end)

      groups = Enum.group_by(entries, &{&1.norm, &1.pos})

      adjustments =
        groups
        |> Enum.flat_map(fn {{_norm, _pos}, group} ->
          ordinary? = Enum.any?(group, &(&1.labels == []))

          if ordinary? do
            group
            |> Enum.filter(&(&1.labels != []))
            |> Enum.map(fn entry ->
              %{id: entry.id, labels: entry.labels, delta: -penalty,
                source: :explicit_dictionary_usage_label}
            end)
          else
            []
          end
        end)
        |> Enum.uniq_by(& &1.id)
        |> Enum.sort_by(& &1.id)

      adjusted =
        Enum.reduce(adjustments, scores, fn %{id: id, delta: delta}, acc ->
          Map.update!(acc, id, &(&1 + delta))
        end)

      {adjusted,
       %{applied?: adjustments != [],
         reason: if(adjustments == [], do: :no_attested_same_pos_alternative,
           else: :explicit_historical_register),
         adjustments: adjustments}}
    end
  end

  def adjust(scores, _candidates, _sentence, _opts),
    do: {scores, %{applied?: false, reason: :invalid_input, adjustments: []}}

  @doc "Extract explicitly historical labels only from the start of a definition."
  @spec historical_labels(term()) :: [String.t()]
  def historical_labels(definition) when is_binary(definition) do
    case Regex.run(~r/^\s*[\(\[]([^\)\]]{1,100})[\)\]]/u, definition) do
      [_, label_text] ->
        label_text
        |> String.downcase()
        |> String.split(~r/[,;]/u, trim: true)
        |> Enum.map(&String.trim/1)
        |> Enum.filter(&MapSet.member?(@historical_labels, &1))

      _ ->
        []
    end
  end

  def historical_labels(_), do: []

  defp bounded_penalty(n) when is_number(n) and n >= 0,
    do: min(n * 1.0, 0.10)
  defp bounded_penalty(_), do: 0.0

  defp normalize(s) when is_binary(s), do: s |> String.trim() |> String.downcase()
  defp normalize(s) when is_atom(s), do: s |> Atom.to_string() |> normalize()
  defp normalize(_), do: ""

  defp field(map, key) when is_map(map),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
end
