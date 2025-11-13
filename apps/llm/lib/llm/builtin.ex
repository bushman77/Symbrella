defmodule Llm.Builtin do
  @moduledoc """
  Minimal builtin entries for a few stubborn words (expand as needed).
  Each entry uses **string keys** to match the rest of the Llm pipeline.
  """

  # NOTE: Typespec maps cannot use string-literal keys like "pos".
  # We keep runtime string keys but declare an approximate spec.
  @type entry :: %{String.t() => String.t() | [String.t()]}

  @spec entries_for(String.t()) :: [entry]
  def entries_for("hello") do
    [
      %{
        "pos" => "interjection",
        "lemma" => "hello",
        "short_gloss" => "greeting used on meeting",
        "example" => "Hello, everyone!",
        "synonyms" => ["hi", "hey"],
        "antonyms" => ["goodbye", "bye"]
      },
      %{
        "pos" => "noun",
        "lemma" => "hello",
        "short_gloss" => "an utterance of “hello”",
        "example" => "She gave a cheerful hello.",
        "synonyms" => ["greeting", "salutation"],
        "antonyms" => ["farewell"]
      }
    ]
  end

  def entries_for("fuck") do
    [
      %{
        "pos" => "verb",
        "lemma" => "fuck",
        "short_gloss" => "to have sex (vulgar)",
        "example" => "They fucked last night.",
        "synonyms" => ["copulate", "screw"],
        "antonyms" => ["abstain"]
      },
      %{
        "pos" => "verb",
        "lemma" => "fuck",
        "short_gloss" => "to spoil or ruin (slang)",
        "example" => "He fucked up the plan.",
        "synonyms" => ["ruin", "botch"],
        "antonyms" => ["fix", "repair"]
      },
      %{
        "pos" => "noun",
        "lemma" => "fuck",
        "short_gloss" => "an act of sex (vulgar)",
        "example" => "He doesn’t give a fuck.",
        "synonyms" => ["sex", "intercourse"],
        "antonyms" => ["abstinence"]
      },
      %{
        "pos" => "interjection",
        "lemma" => "fuck",
        "short_gloss" => "exclamation of anger or surprise",
        "example" => "Fuck! That hurt.",
        "synonyms" => ["damn", "blast"],
        "antonyms" => ["hooray"]
      }
    ]
  end

  def entries_for(_), do: []
end
