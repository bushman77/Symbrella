defmodule Core.Response.SymbolicGeneration do
  @moduledoc """
  The V1.1 realization boundary consumes a post-LIFG speech-act frame,
  never reparses the user's sentence or silently substitutes a greeting.

  "Hello" requires a selected, canonical LIFG interjection witness.
  Other previously supported greetings remain explicitly surface-only.
  A foreign addressee or unsupported action must not generate a reciprocal
  greeting addressed to Symbrella.
  """

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(user_text, %{risk_bucket: :low, intent: intent}, %{action: action}, si)
      when is_binary(user_text) and is_map(si) and
             intent in [:greet, :greeting, :smalltalk, :unknown] and
             action in [:answer, :offer_options] do
    case Map.get(si, :speech_act_frame) do
      %{
        version: 1,
        speech_act: :greeting,
        speaker: :user,
        addressee: addressee,
        temporal_anchor: temporal_anchor,
        lexical_evidence: lexical,
        source_text: ^user_text
      } = frame when addressee in [:self, :unspecified] and is_map(lexical) ->
        if supported_lexical_evidence?(lexical) do
          {:ok, realize(temporal_anchor), realization(frame)}
        else
          :not_applicable
        end

      _ ->
        :not_applicable
    end
  end

  def generate(_text, _features, _decision, _si), do: :not_applicable

  defp supported_lexical_evidence?(%{level: :surface_only, status: :provisional}),
    do: true

  defp supported_lexical_evidence?(%{
         level: :canonical_lifg_selection,
         status: :provisional,
         witness: %{id: "hello|interjection|1", token_index: 0}
       }),
       do: true

  defp supported_lexical_evidence?(_), do: false

  defp realization(frame) do
    lexical = frame.lexical_evidence

    %{
      status: :symbolic_realized,
      speech_act: :reciprocate_greeting,
      interpreted_speech_act: frame.speech_act,
      speaker: frame.speaker,
      addressee: frame.addressee,
      vocative: frame.vocative,
      temporal_anchor: frame.temporal_anchor,
      provenance:
        if(lexical.level == :canonical_lifg_selection,
          do: :canonical_lifg_choice,
          else: :surface_greeting
        ),
      lexical_witness: Map.get(lexical, :witness),
      semantic_verified?: false,
      factual_claims?: false,
      llm_called?: false
    }
  end

  defp realize(:morning), do: "Good morning."
  defp realize(:afternoon), do: "Good afternoon."
  defp realize(:evening), do: "Good evening."
  defp realize(:unspecified), do: "Hello."
end
