defmodule Core.Response.SymbolicGeneration do
  @moduledoc """
  First symbolic speech-realization pilot: a bounded reciprocal greeting.

  This module does not infer facts, feelings, identities, or conversation
  history. It receives an already classified safe social turn, constructs a
  speech-act frame, and realizes only the greeting it can support. Unsupported
  inputs must return :not_applicable rather than invent a response.
  """

  alias Core.Response.GreetingTurn

  @type greeting_frame :: %{
          speech_act: :reciprocate_greeting,
          temporal_anchor: :morning | :afternoon | :evening | :unspecified,
          addressee: :user
        }

  @spec generate(String.t(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(user_text, %{risk_bucket: :low}, decision)
      when is_binary(user_text) and is_map(decision) do
    # Exact standalone surface evidence is sufficient even when upstream
    # intent classification reports :smalltalk or :unknown.
    if GreetingTurn.standalone?(user_text) do
      frame = plan(user_text)
      text = realize(frame)

      {:ok, text,
       %{
         status: :symbolic_realized,
         speech_act: frame.speech_act,
         temporal_anchor: frame.temporal_anchor,
         provenance: :surface_greeting,
         factual_claims?: false,
         llm_called?: false
       }}
    else
      :not_applicable
    end
  end

  def generate(_user_text, _features, _decision), do: :not_applicable

  @spec plan(String.t()) :: greeting_frame()
  defp plan(user_text) do
    %{
      speech_act: :reciprocate_greeting,
      temporal_anchor: time_of_day(user_text),
      addressee: :user
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
        normalized = String.downcase(phrase)

        cond do
          normalized == "morning" -> :morning
          normalized == "afternoon" -> :afternoon
          true -> :evening
        end

      _ ->
        :unspecified
    end
  end
end
