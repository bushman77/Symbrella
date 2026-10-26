defmodule Core.Response.SymbolicGeneration do
  @moduledoc """
  First cognitive-to-language bridge for reciprocal greetings.

  Input meaning comes from Core's conversational intent and the canonical
  LIFG/MWE semantic_choices carried by SemanticInput. Canonical lexical
  selection is provisional evidence, NOT a verified sentence meaning.

  A bounded standalone-greeting boundary prevents converting mixed requests
  into a greeting. Other established greetings retain an explicitly labelled
  surface-only realization until they have their own semantic witness.
  """

  alias Core.Response.GreetingTurn

  @type greeting_frame :: %{
          speech_act: :reciprocate_greeting,
          temporal_anchor: :morning | :afternoon | :evening | :unspecified,
          addressee: :user,
          lexical_witness: map() | nil
        }

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(user_text, %{risk_bucket: :low, intent: intent}, %{action: action}, si)
      when is_binary(user_text) and is_map(si) and
             intent in [:greet, :greeting, :smalltalk, :unknown] and
             action in [:answer, :offer_options] do
    with true <- GreetingTurn.standalone?(user_text),
         {:ok, witness} <- lexical_evidence(user_text, si) do
      frame = plan(user_text, witness)

      {:ok, realize(frame),
       %{
         status: :symbolic_realized,
         speech_act: frame.speech_act,
         temporal_anchor: frame.temporal_anchor,
         addressee: frame.addressee,
         provenance: if(witness, do: :canonical_lifg_choice, else: :surface_greeting),
         lexical_witness: witness,
         semantic_verified?: false,
         factual_claims?: false,
         llm_called?: false
       }}
    else
      _ -> :not_applicable
    end
  end

  def generate(_text, _features, _decision, _si), do: :not_applicable

  # "Hello" is our first strict bridge. It cannot be realized from surface
  # matching alone: the canonical winner must select its interjection sense
  # at the observed first token. Other existing greetings are explicitly
  # marked surface-only, pending their own lexical rules.
  defp lexical_evidence(user_text, si) do
    if Regex.match?(~r/\A\s*hello\b/iu, user_text) do
      case Map.get(si, :semantic_choices) do
        choices when is_list(choices) ->
          choices
          |> Enum.find(&hello_winner?/1)
          |> case do
            %{} = winner ->
              {:ok, Map.take(winner, [:id, :token_index, :score, :source])}

            nil ->
              :missing_lexical_evidence
          end

        _ ->
          :missing_lexical_evidence
      end
    else
      {:ok, nil}
    end
  end

  defp hello_winner?(%{} = choice) do
    Map.get(choice, :id) == "hello|interjection|1" and
      Map.get(choice, :token_index) == 0 and
      Map.get(choice, :pos) in [:interjection, "interjection"] and
      is_number(Map.get(choice, :score)) and Map.get(choice, :score) > 0
  end

  defp hello_winner?(_), do: false

  @spec plan(String.t(), map() | nil) :: greeting_frame()
  defp plan(text, witness) do
    %{
      speech_act: :reciprocate_greeting,
      temporal_anchor: time_of_day(text),
      addressee: :user,
      lexical_witness: witness
    }
  end

  @spec realize(greeting_frame()) :: String.t()
  defp realize(%{speech_act: :reciprocate_greeting, temporal_anchor: time}) do
    greeting =
      case time do
        :morning -> "Good morning"
        :afternoon -> "Good afternoon"
        :evening -> "Good evening"
        :unspecified -> "Hello"
      end

    greeting <> "."
  end

  defp time_of_day(text) do
    case Regex.run(~r/\bgood\s+(morning|afternoon|eve(?:n+i+|i+)ng)\b/iu, text) do
      [_, phrase] ->
        case String.downcase(phrase) do
          "morning" -> :morning
          "afternoon" -> :afternoon
          _ -> :evening
        end

      _ ->
        :unspecified
    end
  end
end
