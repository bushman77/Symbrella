defmodule Core.Response.UserNameMemoryTest do
  use ExUnit.Case, async: false

  alias Core.Response

  setup do
    if Code.ensure_loaded?(Brain.Hippocampus) do
      Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
      Brain.Hippocampus.reset()
    end

    :ok
  end

  test "name question reads user_name fact from hippocampal memory" do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Curtis|proper|0", lemma: "Curtis"}]},
      %{kind: :fact, key: :user_name, value: "Curtis", tags: ["fact"]}
    )

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "what is my name?"
      })

    assert text == "Your name is Curtis."
    assert meta.action == :identity
    assert meta.intent_inferred == :name_query
    assert meta.response_source == :memory
    assert meta.memory_key == :user_name
    assert meta.memory_source == :hippocampus_fact
    assert meta.user_name == "Curtis"
    refute text =~ "I don"
  end

  test "fuzzy name question reads user_name fact from hippocampal memory" do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Curtis|proper|0", lemma: "Curtis"}]},
      %{kind: :fact, key: :user_name, value: "Curtis", tags: ["fact"]}
    )

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "wahts my naem?"
      })

    assert text == "Your name is Curtis."
    assert meta.action == :identity
    assert meta.user_name == "Curtis"
  end

  test "conversational name question reads user_name fact from hippocampal memory" do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Curtis|proper|0", lemma: "Curtis"}]},
      %{kind: :fact, key: :user_name, value: "Curtis", tags: ["fact"]}
    )

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "do you know my name?"
      })

    assert text == "Your name is Curtis."
    assert meta.action == :identity
    assert meta.intent_inferred == :name_query
    assert meta.user_name == "Curtis"
  end

  test "assistant identity is not recalled as the user's name" do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Symbrella|proper|0", lemma: "Symbrella"}]},
      %{kind: :fact, key: :user_name, value: "Symbrella", tags: ["fact", "user_name"]}
    )

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "what is my name?"
      })

    assert text =~ "I don’t know your name yet"
    assert meta.action == :identity
    assert meta.user_name == nil
    refute text =~ "Your name is Symbrella"
  end

  test "assistant identity is not accepted as a new user name claim" do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Bradley|proper|0", lemma: "Bradley"}]},
      %{
        kind: :fact,
        key: :user_name,
        value: "Bradley",
        subject: :user,
        tags: ["fact", "user_name"]
      }
    )

    {_tone, text, meta} =
      Response.plan(%{
        intent: :statement,
        confidence: 0.9,
        text: "my name is Symbrella"
      })

    assert text == "I should not store Symbrella as your name because that is my assistant identity."
    assert meta.user_name == nil
    refute text =~ "Nice to meet you, Symbrella"

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "what is my name?"
      })

    assert text == "Your name is Bradley."
    assert meta.user_name == "Bradley"
    refute text =~ "Your name is Symbrella"
  end

  test "name question falls back to persisted fact after memory window reset" do
    {_tone, text, meta} =
      Response.plan(%{
        intent: :statement,
        confidence: 0.9,
        text: "my name is Harriet"
      })

    assert text == "Nice to meet you, Harriet. I’ll remember that."
    assert meta.user_name == "Harriet"

    Brain.Hippocampus.reset()

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "do you know my name?"
      })

    assert text == "Your name is Harriet."
    assert meta.action == :identity
    assert meta.user_name == "Harriet"
  end

  test "explicit remember directive stores and recalls a user fact" do
    {_tone, text, meta} =
      Response.plan(%{
        intent: :statement,
        confidence: 0.9,
        text: "remember that my favorite color is blue"
      })

    assert text == "I’ll remember that your favorite color is blue."
    assert meta.action == :remember_fact
    assert meta.fact_key == "favorite_color"

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "what is my favorite color?"
      })

    assert text == "Your favorite color is blue."
    assert meta.action == :recall_fact
    assert meta.fact_key == "favorite_color"
  end

  test "direct user fact claim stores and recalls without remember prefix" do
    {_tone, text, meta} =
      Response.plan(%{
        intent: :statement,
        confidence: 0.9,
        text: "my sisters name is Mary-Anne"
      })

    assert text == "I’ve noted that your sisters name is Mary-Anne."
    assert meta.action == :remember_fact
    assert meta.fact_key == "sisters_name"

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "what is my sisters name"
      })

    assert text == "Your sisters name is Mary-Anne."
    assert meta.action == :recall_fact
    assert meta.fact_key == "sisters_name"
  end

  test "definition questions are not stored as direct user facts" do
    assert Response.memory_reply(%{
             intent: :unknown,
             confidence: 1.0,
             text: "what is the meaning of poop"
           }) == nil
  end

  test "embedded definition questions are not stored as direct user facts" do
    assert Response.memory_reply(%{
             intent: :unknown,
             confidence: 1.0,
             text: "plus what is symbrella"
           }) == nil
  end

  test "location memory stores live-in phrasing and recalls where-do-i-live" do
    {_tone, text, meta} =
      Response.plan(%{
        intent: :statement,
        confidence: 0.9,
        text: "i live in Richmond, BC, please remeber that"
      })

    assert text == "I’ve noted that your location is Richmond, BC."
    assert meta.action == :remember_fact
    assert meta.fact_key == "location"

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "where do i live"
      })

    assert text == "Your location is Richmond, BC."
    assert meta.action == :recall_fact
    assert meta.fact_key == "location"
  end
end
