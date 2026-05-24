# apps/core/test/core/health_support_cognition_regression_test.exs
defmodule Core.HealthSupportCognitionRegressionTest do
  use ExUnit.Case, async: false

  @moduletag :health_support
  @moduletag :cognition_regression

  @input "Good morning, I forgot my quitiapine and now I can not sleep."

  test "end-to-end health support cognition survives fuzzy text, opener, senses, event frame, and response planning" do
    si =
      Core.resolve_input(@input,
        mode: :prod,
        max_wordgram_n: 3,
        response: :auto,
        persist_episodes: false
      )

    assert %Core.SemanticInput{} = si

    # Intent / fuzzy social-opener behavior
    assert si.intent == :health_support
    assert is_number(si.confidence)
    assert si.confidence > 0.70

    assert is_binary(si.keyword)
    assert si.keyword =~ "quetiapine"

    # Symbolic health event frame
    assert %{
             type: :health_support_event,
             subject: :user,
             event: :forgot_medication,
             medication: "quetiapine",
             consequence: :sleep_inability,
             temporal_context: :now,
             domain: :health_support,
             polarity: :negative
           } = si.symbolic_frame

    # Agentic action selection should choose a bounded supportive-care action.
    assert si.selected_action == :safe_support
    assert is_list(si.action_candidates)

    assert Enum.any?(si.action_candidates, fn
             %{action: :store_memory} -> true
             _ -> false
           end)

    assert %{selected: :safe_support, safety_gate: :approved} = si.action_meta
    assert is_number(si.symbolic_frame.confidence)
    assert si.symbolic_frame.confidence >= 0.75

    # LIFG / sense slate should preserve the medically meaningful entity.
    assert has_lifg_choice?(si, fn choice ->
             choice_text(choice) == "quetiapine" and
               choice_pos(choice) in ["entity", :entity] and
               medication_choice?(choice)
           end)

    # Function-word / closed-class choices should not fall back to dictionary-literal junk.
    assert has_lifg_choice?(si, fn choice ->
             choice_text(choice) == "not" and choice_pos(choice) in ["particle", :particle]
           end)

    assert has_lifg_choice?(si, fn choice ->
             choice_text(choice) == "can" and choice_pos(choice) in ["auxiliary", :auxiliary]
           end)

    # Response planner must stay in supportive-care mode, not engineering collaborator mode.
    assert %{} = meta = si.response_meta
    assert meta.intent_inferred == :health_support
    assert meta.mode == :supportive_care
    assert meta.action == :safe_support
    assert meta.profile == :supportive_care
    assert meta.agent_selected_action == :safe_support

    assert is_binary(si.response_text)
    assert si.response_text != ""

    assert si.response_text =~ "pharmacist" or si.response_text =~ "prescriber"
    assert si.response_text =~ "missed" or si.response_text =~ "medication"

    refute si.response_text =~ "module"
    refute si.response_text =~ "file"
    refute si.response_text =~ "failing output"
    refute si.response_text =~ "engineering move"

    # Trace should prove the event-frame stage actually attached structured meaning.
    assert Enum.any?(si.trace, fn
             %{stage: :event_frame, decision: :attached, meta: %{medication: "quetiapine"}} ->
               true

             _ ->
               false
           end)
  end

  defp has_lifg_choice?(%{lifg_choices: choices}, fun)
       when is_list(choices) and is_function(fun, 1) do
    Enum.any?(choices, fun)
  end

  defp has_lifg_choice?(_, _), do: false

  defp choice_text(choice) when is_map(choice) do
    choice
    |> first_present([:lemma, "lemma", :norm, "norm", :phrase, "phrase", :id, "id"])
    |> case do
      nil ->
        ""

      id when is_binary(id) ->
        id
        |> String.split("|", parts: 2)
        |> List.first()
        |> String.downcase()
        |> String.trim()

      other ->
        other
        |> to_string()
        |> String.downcase()
        |> String.trim()
    end
  end

  defp choice_text(_), do: ""

  defp choice_pos(choice) when is_map(choice) do
    first_present(choice, [:pos, "pos"])
  end

  defp choice_pos(_), do: nil

  defp medication_choice?(choice) when is_map(choice) do
    id = first_present(choice, [:id, "id"])
    src = first_present(choice, [:src, "src", :source, "source"])
    entity_type = first_present(choice, [:entity_type, "entity_type"])

    (is_binary(id) and String.contains?(id, "medication")) or
      src in [:medical_entity_fallback, "medical_entity_fallback"] or
      entity_type in [:medication, "medication"]
  end

  defp medication_choice?(_), do: false

  defp first_present(map, keys) when is_map(map) and is_list(keys) do
    Enum.find_value(keys, fn key ->
      case Map.get(map, key) do
        nil -> nil
        "" -> nil
        value -> value
      end
    end)
  end
end
