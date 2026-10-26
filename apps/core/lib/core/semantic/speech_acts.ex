defmodule Core.Semantic.SpeechActs do
  @moduledoc """
  Read-only post-LIFG binding of narrow direct social speech acts.

  Canonical :semantic_choices supply provisional lexical selection evidence;
  a separately constrained greeting grammar assigns speech act and vocative
  roles. Neither lexical selection nor an assistant-name match proves complete
  sentence understanding. This stage never writes memories, learns, changes
  LIFG choices, or invokes an LLM.

  A greeting explicitly addressed to somebody other than Symbrella gets
  :other as its addressee, not :self. Reported speech is not a direct greeting.
  """

  @assistant_name "symbrella"

  @type frame :: %{
          version: 1,
          speech_act: :greeting | :gratitude | :farewell | :wellbeing_query,
          speaker: :user,
          addressee: :self | :unspecified | :other,
          vocative: map() | nil,
          temporal_anchor: :morning | :afternoon | :evening | :night | :unspecified,
          lexical_evidence: map(),
          source_text: String.t(),
          sentence_meaning_verified?: false
        }

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    case from_si(si) do
      {:ok, frame} -> Map.put(si, :speech_act_frame, frame)
      :not_applicable -> Map.put(si, :speech_act_frame, nil)
    end
  end

  def attach(other), do: other

  @spec from_si(map()) :: {:ok, frame()} | :not_applicable
  def from_si(%{} = si) do
    case Map.get(si, :sentence) do
      text when is_binary(text) and byte_size(text) <= 160 ->
        case Regex.named_captures(greeting_pattern(), text) do
          %{} = captures ->
            {addressee, vocative} = resolve_addressee(Map.get(captures, "vocative"))

            {:ok, social_frame(:greeting, text, addressee, vocative,
              time_of_day(Map.fetch!(captures, "greeting")),
              lexical_evidence(Map.fetch!(captures, "greeting"), si))}

          _ ->
            other_direct_social_act(text)
        end

      _ ->
        :not_applicable
    end
  end

  def from_si(_), do: :not_applicable

  # Full-sentence grammar: short standalone direct greeting with optional
  # "there" and a single vocative. It rejects requests, reported speech,
  # conjunctions, multi-participant addressees and quoted sentences.
  defp greeting_pattern do
    ~r/\A\s*(?<greeting>hello|hi+|heya?|hiya|yo|sup|good\s+(?:morning|afternoon|eve(?:n+i+|i+)ng))(?:\s+there)?(?:\s*,?\s+(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*[.!?]*\s*\z/iu
  end

  # Short whole-utterance grammars. These receive *surface-only* provenance,
  # not invented LIFG winners. Compound turns and quoted speech abstain.
  defp other_direct_social_act(text) do
    matched =
      [
        {:gratitude, gratitude_pattern()},
        {:farewell, farewell_pattern()},
        {:wellbeing_query, wellbeing_pattern()}
      ]
      |> Enum.flat_map(fn {act, pattern} ->
        case Regex.named_captures(pattern, text) do
          %{} = captures -> [{act, captures}]
          _ -> []
        end
      end)

    case matched do
      [{act, captures}] ->
        {addressee, vocative} = resolve_addressee(Map.get(captures, "vocative"))

        anchor =
          if act == :farewell and Regex.match?(~r/\A\s*good\s+night\b/iu, text),
            do: :night, else: :unspecified

        {:ok, social_frame(act, text, addressee, vocative, anchor,
          %{level: :surface_only, status: :provisional, witness: nil})}

      _ ->
        :not_applicable
    end
  end

  defp social_frame(act, text, addressee, vocative, anchor, lexical) do
    %{
      version: 1,
      speech_act: act,
      speaker: :user,
      addressee: addressee,
      vocative: vocative,
      temporal_anchor: anchor,
      lexical_evidence: lexical,
      source_text: text,
      sentence_meaning_verified?: false
    }
  end

  defp gratitude_pattern do
    ~r/\A\s*(?:thank\s+you(?:\s+very\s+much)?|thanks(?:\s+a\s+lot)?|much\s+appreciated)(?:\s*,?\s+(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*[.!]*\s*\z/iu
  end

  defp farewell_pattern do
    ~r/\A\s*(?:bye|goodbye|good\s+night|see\s+you\s+(?:later|soon)|take\s+care)(?:\s*,?\s+(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*[.!]*\s*\z/iu
  end

  defp wellbeing_pattern do
    ~r/\A\s*how\s+are\s+you(?:\s+doing)?(?:\s*,?\s+(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*\?\s*\z/iu
  end

  defp resolve_addressee(nil), do: {:unspecified, nil}
  defp resolve_addressee(""), do: {:unspecified, nil}

  defp resolve_addressee(surface) do
    if String.downcase(surface) == @assistant_name do
      {:self,
       %{surface: surface, role: :addressee, resolution: :assistant_identity}}
    else
      {:other,
       %{surface: surface, role: :addressee, resolution: :unresolved_other}}
    end
  end

  defp lexical_evidence(greeting, si) do
    if String.downcase(greeting) == "hello" do
      case Map.get(si, :semantic_choices) do
        choices when is_list(choices) ->
          case Enum.find(choices, &selected_hello?/1) do
            %{} = winner ->
              %{
                level: :canonical_lifg_selection,
                status: :provisional,
                witness: Map.take(winner, [:id, :token_index, :score, :source])
              }

            _ ->
              %{level: :insufficient, status: :missing_canonical_winner, witness: nil}
          end

        _ ->
          %{level: :insufficient, status: :missing_canonical_winner, witness: nil}
      end
    else
      %{level: :surface_only, status: :provisional, witness: nil}
    end
  end

  defp selected_hello?(%{} = choice) do
    Map.get(choice, :id) == "hello|interjection|1" and
      Map.get(choice, :token_index) == 0 and
      Map.get(choice, :pos) in [:interjection, "interjection"] and
      is_number(Map.get(choice, :score)) and Map.get(choice, :score) > 0
  end

  defp selected_hello?(_), do: false

  defp time_of_day(greeting) do
    case String.downcase(greeting) do
      "good morning" -> :morning
      "good afternoon" -> :afternoon
      phrase when phrase in ["good evening", "good eveing", "good eveniing"] -> :evening
      phrase -> if String.starts_with?(phrase, "good eve"), do: :evening, else: :unspecified
    end
  end
end
