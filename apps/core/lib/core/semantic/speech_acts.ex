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
          speech_act: :greeting | :gratitude | :farewell | :wellbeing_query | :social_update_query,
          speaker: :user,
          addressee: :self | :unspecified | :other,
          vocative: map() | nil,
          temporal_anchor: :morning | :afternoon | :evening | :night | :today | :unspecified,
          lexical_evidence: map(),
          source_text: String.t(),
          sentence_meaning_verified?: false
        }

  @spec attach(map()) :: map()
  def attach(si), do: attach(si, [])

  @spec attach(map(), keyword()) :: map()
  def attach(%{} = si, opts) when is_list(opts) do
    case from_si(si, opts) do
      {:ok, frame} -> Map.put(si, :speech_act_frame, frame)
      :not_applicable -> Map.put(si, :speech_act_frame, nil)
    end
  end

  def attach(other, _opts), do: other

  @spec from_si(map()) :: {:ok, frame()} | :not_applicable
  def from_si(si), do: from_si(si, [])

  @spec from_si(map(), keyword()) :: {:ok, frame()} | :not_applicable
  def from_si(%{} = si, opts) when is_list(opts) do
    case Map.get(si, :sentence) do
      text when is_binary(text) and byte_size(text) <= 160 ->
        case Regex.named_captures(greeting_pattern(), text) do
          %{} = captures ->
            {addressee, vocative} = resolve_addressee(Map.get(captures, "vocative"))

            {:ok, social_frame(:greeting, text, addressee, vocative,
              time_of_day(Map.fetch!(captures, "greeting")),
              lexical_evidence(Map.fetch!(captures, "greeting"), si))}

          _ ->
            case dictionary_greeting(text, opts) do
              {:ok, _} = greeting -> greeting
              :not_applicable ->
                case social_update_act(text) do
                  {:ok, _} = update -> update
                  :not_applicable -> other_direct_social_act(text)
                end
            end
        end

      _ ->
        :not_applicable
    end
  end

  def from_si(_, _), do: :not_applicable

  # Full-sentence grammar: short standalone direct greeting with optional
  # "there" and a single vocative. It rejects requests, reported speech,
  # conjunctions, multi-participant addressees and quoted sentences.
  defp greeting_pattern do
    ~r/\A\s*(?<greeting>hello|hi+|heya?|hiya|yo|sup|good\s+(?:morning|afternoon|eve(?:n+i+|i+)ng))(?:\s+there)?(?:\s*,?\s+(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*[.!?]*\s*\z/iu
  end

  # Reuse the dictionary for a single direct lexical greeting only. This
  # cannot reinterpret reported speech, promote a LIFG winner, or learn.
  defp dictionary_greeting(text, opts) do
    case Regex.named_captures(
           ~r/\A\s*(?<word>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[!.]?\s*\z/u,
           text
         ) do
      %{"word" => word} when byte_size(word) <= 40 ->
        norm = String.downcase(word)
        lookup = Keyword.get(opts, :greeting_lookup, &Db.brain_cells_for_norm/1)

        if is_function(lookup, 1) do
          case lookup.(norm) do
            rows when is_list(rows) and length(rows) <= 32 ->
              interjections = Enum.filter(rows, fn cell ->
                is_map(cell) and Map.get(cell, :norm) == norm and
                  Map.get(cell, :pos) in [:interjection, "interjection"]
              end)

              if interjections != [] and length(interjections) <= 8 and
                   Enum.all?(interjections, &greeting_interjection?(&1, norm)) do
                winner = Enum.min_by(interjections, &Map.fetch!(&1, :id))
                definition = Map.fetch!(winner, :definition)

                lexical = %{
                  level: :dictionary_interjection_greeting,
                  status: :provisional,
                  witness: %{
                    id: winner.id,
                    norm: norm,
                    definition_sha256:
                      (:crypto.hash(:sha256, definition)
                       |> Base.encode16(case: :lower))
                  }
                }

                {:ok, social_frame(:greeting, text, :unspecified, nil,
                  :unspecified, lexical)}
              else
                :not_applicable
              end

            _ -> :not_applicable
          end
        else
          :not_applicable
        end

      _ -> :not_applicable
    end
  rescue
    _ -> :not_applicable
  catch
    _, _ -> :not_applicable
  end

  defp greeting_interjection?(cell, norm) do
    id = Map.get(cell, :id)
    definition = Map.get(cell, :definition)

    is_binary(id) and String.starts_with?(id, norm <> "|interjection|") and
      is_binary(definition) and
      Regex.match?(
        ~r/\A\s*(?:\([^()]*\)\s*)*(?:(?:an?|the)\s+)?(?:(?:informal|formal|casual|colloquial|friendly|polite)\s+)?(?:greeting|salutation)\s*[.!]?\s*\z/iu,
        definition
      )
  end

  # A closed, whole-utterance check-in grammar. We separately record a
  # greeting (when present) and an informal social-update question. Neither
  # unit is promoted to a dictionary-verified meaning or a new Syntax
  # signature: this is provisional, surface-only conversational evidence.
  # Requiring the complete question and a final '?' prevents prefix matches,
  # quotations, reported speech and substantive follow-on instructions.
  defp social_update_act(text) do
    case Regex.named_captures(social_update_pattern(), text) do
      %{} = captures ->
        greeting = Map.get(captures, "greeting")
        components =
          if is_binary(greeting) and greeting != "",
            do: [:greeting, :social_update_query],
            else: [:social_update_query]

        case social_update_vocative(captures) do
          :ambiguous ->
            :not_applicable

          surface ->
            {addressee, vocative} = resolve_addressee(surface)

            frame =
              social_frame(:social_update_query, text, addressee, vocative,
                :unspecified, %{level: :surface_only, status: :provisional, witness: nil})
              |> Map.put(:components, components)
              |> Map.put(:construction,
                if(length(components) == 2,
                  do: :greeting_plus_social_update,
                  else: :social_update_question))

            {:ok, frame}
        end

      _ ->
        :not_applicable
    end
  end

  # P-035: a vocative may precede the question ("hey symbrella what…") or
  # follow it ("…with you, Symbrella?"), but not both. Only the assistant's
  # own name is accepted in the pre-question position, so content words
  # cannot be swallowed as a name.
  defp social_update_vocative(captures) do
    pre = Map.get(captures, "vocative_pre", "")
    post = Map.get(captures, "vocative", "")

    cond do
      pre != "" and post != "" -> :ambiguous
      pre != "" -> pre
      true -> post
    end
  end

  # Accept common chat contraction spellings ("what's", "whats", "what is").
  # P-035 adds "what are/'re you up to" and "what are/'re you doing".
  # There is deliberately no wildcard tail or quoted-content matching.
  defp social_update_pattern do
    ~r/\A\s*(?:(?<greeting>hey|hello|hi+|heya?|hiya|yo)(?:\s*[,!]\s*|\s+))?(?:(?<vocative_pre>symbrella)(?:\s*,\s*|\s+))?what(?:(?:'s|’s|s|\s+is)\s+new\s+with\s+you|(?:\s+are|'re|’re)\s+you\s+(?:up\s+to|doing))(?:\s*,\s*(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*\?\s*\z/iu
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
          cond do
            act == :farewell and Regex.match?(~r/\A\s*good\s+night\b/iu, text) ->
              :night

            act == :wellbeing_query ->
              wellbeing_anchor(Map.get(captures, "temporal"))

            true ->
              :unspecified
          end

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

  # "Today" is a temporal adjunct to the idiomatic check-in, not an
  # addressee or an unresolved predicate. The final question mark is optional
  # in chat; a period is deliberately not accepted as a question marker.
  # This remains a closed whole-utterance grammar, not a prefix match.
  defp wellbeing_pattern do
    ~r/\A\s*how\s+are\s+you(?:\s+doing)?(?:\s+(?<temporal>today|tonight|this\s+(?:morning|afternoon|evening)))?(?:\s*,?\s+(?<vocative>[\p{L}][\p{L}'-]{0,47}))?\s*\??\s*\z/iu
  end

  defp wellbeing_anchor(nil), do: :unspecified
  defp wellbeing_anchor(""), do: :unspecified

  defp wellbeing_anchor(temporal) when is_binary(temporal) do
    case String.downcase(temporal) do
      "today" -> :today
      "tonight" -> :night
      "this morning" -> :morning
      "this afternoon" -> :afternoon
      "this evening" -> :evening
      _ -> :unspecified
    end
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
