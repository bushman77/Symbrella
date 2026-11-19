defmodule Core.Response.DictionaryGreetingPipelineTest do
  use ExUnit.Case, async: false

  alias Db.BrainCell

  test "tagged dictionary howdy reaches symbolic greeting through Core" do
    previous =
      Application.get_env(
        :core,
        :llm_inference_enabled?,
        :unset
      )

    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if previous == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(
          :core,
          :llm_inference_enabled?,
          previous
        )
      end
    end)

    id = "howdy|interjection|p013"

    cell =
      BrainCell.changeset(%BrainCell{}, %{
        id: id,
        word: "howdy",
        pos: "interjection",
        definition: "(us, informal, regional) An informal greeting."
      })
      |> Db.insert!()

    on_exit(fn -> Db.delete!(cell) end)

    si =
      Core.resolve_input(
        "howdy!",
        mode: :prod,
        session_id:
          "p013-howdy-#{System.unique_integer([:positive])}",
        response: :auto,
        persist_episodes: false,
        execute_agency_commands?: false
      )

    assert %{
             speech_act: :greeting,
             lexical_evidence: %{
               level: :dictionary_interjection_greeting
             }
           } = si.speech_act_frame

    assert si.response_text == "Hello.",
           "selected_action=#{inspect(si.selected_action)} " <>
             "response_meta=#{inspect(si.response_meta)}"

    assert si.response_meta.response_source ==
             :symbolic_generation

    assert si.response_meta.response_reflection.provenance ==
             :dictionary_greeting_gloss

    assert si.response_meta.response_reflection.lexical_witness.id ==
             id

    refute si.response_meta.response_reflection.llm_called?
  end
end
