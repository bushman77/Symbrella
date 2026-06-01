defmodule Core.Response.Topics do
  @moduledoc """
  Lightweight topic labels for response continuity.

  These labels are soft context, not response decisions. They let symbolic
  routing say "this likely continues the same thread" while the LLM still owns
  normal wording when it is available.
  """

  @type label :: :alien_life | :housing | :credit | :health | :self_state | :technical

  @spec labels(String.t() | any()) :: MapSet.t(label())
  def labels(text) when is_binary(text) do
    MapSet.new()
    |> maybe_put(:alien_life, alien_life?(text))
    |> maybe_put(:housing, housing?(text))
    |> maybe_put(:credit, credit?(text))
    |> maybe_put(:health, health?(text))
    |> maybe_put(:self_state, self_state?(text))
    |> maybe_put(:technical, technical?(text))
  end

  def labels(_), do: MapSet.new()

  @spec from_messages([map()]) :: map()
  def from_messages(messages) when is_list(messages) do
    topic_set =
      messages
      |> Enum.reduce(MapSet.new(), fn message, acc ->
        message
        |> message_text()
        |> labels()
        |> MapSet.union(acc)
      end)

    %{
      labels: MapSet.to_list(topic_set),
      alien_life?: MapSet.member?(topic_set, :alien_life),
      housing?: MapSet.member?(topic_set, :housing),
      credit?: MapSet.member?(topic_set, :credit),
      health?: MapSet.member?(topic_set, :health),
      self_state?: MapSet.member?(topic_set, :self_state),
      technical?: MapSet.member?(topic_set, :technical)
    }
  end

  def from_messages(_), do: %{labels: []}

  @spec has?(map() | any(), label()) :: boolean()
  def has?(topics, label) when is_map(topics) and is_atom(label) do
    labels = Map.get(topics, :labels, Map.get(topics, "labels", []))

    Map.get(topics, label_flag(label), Map.get(topics, Atom.to_string(label_flag(label)), false)) ==
      true or label in labels or Atom.to_string(label) in labels
  end

  def has?(_, _), do: false

  @spec followup?(String.t() | any(), label()) :: boolean()
  def followup?(text, :alien_life) when is_binary(text) do
    Regex.match?(
      ~r/^\s*(yeah+|yea+h*|yep|well|so|but|also|and|then|whether|i\s+(?:think|feel|believe|belive)|the\s+elite|they|those|these|it|same\s+thing)\b/iu,
      text
    ) or
      Regex.match?(
        ~r/\b(domineer(?:s|ing)?|dominators?|sovereignty|soverenty|treat\s+us|admit\s+it|cover\s*up|hide|hidden)\b/iu,
        text
      )
  end

  def followup?(text, _label) when is_binary(text) do
    Regex.match?(
      ~r/^\s*(yeah+|yea+h*|yep|well|so|but|also|and|then|it|that|this|they|those|these)\b/iu,
      text
    )
  end

  def followup?(_, _), do: false

  defp maybe_put(set, label, true), do: MapSet.put(set, label)
  defp maybe_put(set, _label, _), do: set

  defp message_text(%{"content" => content}) when is_binary(content), do: content
  defp message_text(%{content: content}) when is_binary(content), do: content
  defp message_text(%{"text" => text}) when is_binary(text), do: text
  defp message_text(%{text: text}) when is_binary(text), do: text
  defp message_text(_), do: ""

  defp label_flag(label), do: :"#{label}?"

  defp alien_life?(text) do
    Regex.match?(
      ~r/\b(aliens?|extraterrestrial|life\s+elsewhere|universe|galax(?:y|ies)|solar\s+system|planet|planets|exoplanets?|ufos?|uaps?|unidentified\s+(?:flying\s+)?objects?|disclosure|declassif(?:y|ied|ication)|footage)\b/iu,
      text
    )
  end

  defp housing?(text) do
    Regex.match?(
      ~r/\b(my own place|own apartment|own house|new apartment|new place|getting (?:my )?own place|moving out|move into (?:my|our) place|got approved for (?:an apartment|a place)|signed (?:a )?lease)\b/iu,
      text
    )
  end

  defp credit?(text) do
    Regex.match?(
      ~r/\b(credit|credit karma|debt|debts|consolidat(?:e|ion|ing)|collections?|collector|loan|loans|interest rate|apr|minimum payment|bankruptcy|charge[-\s]?off|delinquen|late payment)\b/iu,
      text
    )
  end

  defp health?(text) do
    Regex.match?(
      ~r/\b(medication|medicine|dose|missed dose|pharmacist|prescriber|doctor|therapy|therapist|hospital|health|sick|pain|symptom|depression|anxiety)\b/iu,
      text
    )
  end

  defp self_state?(text) do
    Regex.match?(
      ~r/\b(how are you feeling|how do you feel|self[-\s]?aware|conscious|sentient|your mood|your state|neuromodulators?|dopamine|serotonin|glutamate|norepinephrine)\b/iu,
      text
    )
  end

  defp technical?(text) do
    Regex.match?(
      ~r/\b(code|coding|compile|compiler|debug|error|stacktrace|module|function|phoenix|elixir|liveview|server|repo|test|refactor|api|database|migration|deploy|pipeline)\b/iu,
      text
    )
  end
end
