defmodule Core.Syntax.WhBeActivityQuestionTest do
  @moduledoc """
  P-033 (staged): vocative-tolerant "what are you doing / up to?" frame and
  its provisional :activity_question sentence meaning. Core signature only;
  no ATL verification, sense choice or response change.
  """
  use ExUnit.Case, async: true

  alias Core.Semantic.SentenceMeaning
  alias Core.Syntax.Signatures

  @cells [
    %{id: "what|pronoun|1", norm: "what", pos: "pronoun"},
    %{id: "are|auxiliary|0", norm: "are", pos: "auxiliary"},
    %{id: "is|auxiliary|0", norm: "is", pos: "auxiliary"},
    %{id: "you|pronoun|1", norm: "you", pos: "pronoun"},
    %{id: "she|pronoun|1", norm: "she", pos: "pronoun"},
    %{id: "up|preposition|1", norm: "up", pos: "preposition"},
    %{id: "to|preposition|1", norm: "to", pos: "preposition"},
    %{id: "doing|verb|1", norm: "doing", pos: "verb"},
    %{id: "hey|interjection|1", norm: "hey", pos: "interjection"},
    %{id: "the|determiner|1", norm: "the", pos: "determiner"},
    %{id: "task|noun|1", norm: "task", pos: "noun"},
    %{id: "for|preposition|1", norm: "for", pos: "preposition"}
  ]

  defp si(sentence) do
    tokens =
      sentence
      |> String.split(" ", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {word, i} -> %{index: i, phrase: word, n: 1} end)

    %{sentence: sentence, source: :test, tokens: tokens, active_cells: @cells, trace: []}
  end

  defp frames(sentence),
    do: sentence |> si() |> Signatures.detect() |> Enum.filter(&(&1.id == :wh_be_activity_question))

  defp meaning(sentence) do
    base = si(sentence)
    base |> Map.put(:sentence_signatures, Signatures.detect(base)) |> SentenceMeaning.build()
  end

  describe "signature" do
    test "matches activity questions, with or without a vocative prefix" do
      for {sentence, roles} <- [
            {"what are you up to?", %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, predicate_end: 4}},
            {"hey symbrella what are you up to?",
             %{wh: 2, auxiliary: 3, subject: 4, predicate: 5, predicate_end: 6, vocative: 0}},
            {"symbrella, what are you doing?",
             %{wh: 1, auxiliary: 2, subject: 3, predicate: 4, predicate_end: 4, vocative: 0}},
            {"what is she doing?", %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, predicate_end: 3}}
          ] do
        assert [frame] = frames(sentence), "expected a frame for #{inspect(sentence)}"
        assert frame.roles == roles
        assert frame.source == :core_syntax
        assert (:vocative_prefix in frame.evidence) == Map.has_key?(roles, :vocative)
      end
    end

    test "does not match trailing material, missing predicate, or statements" do
      for sentence <- [
            "what are you up to the task for?",
            "what are you?",
            "what are you up to",
            "you are up to?",
            "hey there what are you up to?"
          ] do
        assert frames(sentence) == [], "unexpected frame for #{inspect(sentence)}"
      end
    end

    test "a non-verb -ing word is not a progressive predicate" do
      cells = Enum.reject(@cells, &(&1.norm == "doing")) ++ [%{id: "doing|noun|1", norm: "doing", pos: "noun"}]
      base = %{si("what are you doing?") | active_cells: cells}

      assert Signatures.detect(base) |> Enum.filter(&(&1.id == :wh_be_activity_question)) == []
    end
  end

  describe "sentence meaning" do
    test "the live sentence yields a provisional activity question" do
      m = meaning("hey symbrella what are you up to?")

      assert m.kind == :activity_question
      assert m.status == :provisional
      assert m.speech_act == :request_information
      assert m.roles.subject.surface == "you"
      assert m.roles.predicate.surface == "up to"
      assert m.roles.activity == %{role: :activity, status: :unbound}
      assert m.addressee_vocative?
      assert m.grounding.exact_lexical_meanings == :not_verified
      assert m.grounding.factual_authority? == false
    end

    test "no frame, no activity meaning" do
      refute meaning("what are you up to the task for?").kind == :activity_question
    end
  end
end
