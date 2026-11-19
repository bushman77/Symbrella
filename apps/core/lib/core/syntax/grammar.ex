defmodule Core.Syntax.Grammar do
  @moduledoc """
  Closed, declarative grammar inventory for the bounded Colony syntax chart.

  The rule table is data. Open-class lexical categories must have an attested
  POS (and a past-participle morphology witness where applicable).
  No token surface is converted to an atom.
  """

  @copulas ~w(is are was were)
  @quantifiers ~w(most every each all both several some many few)
  @negative_words ~w(not never no n't isn't aren't wasn't weren't don't doesn't didn't can't couldn't won't wouldn't shouldn't)
  @modals ~w(can could may might must shall should will would)
  @subordinators ~w(if when unless because although while)
  @pronouns ~w(i you he she it we they)
  @plural_pronouns ~w(we they you)
  @singular_pronouns ~w(he she it i)
  @determiners ~w(the a an this that these those)
  @agreement %{"is" => :singular, "was" => :singular,
    "are" => :plural, "were" => :plural}

  @rules [
    %{lhs: :nominal, rhs: [:noun], op: :nominal_head},
    %{lhs: :nominal, rhs: [:adjective, :nominal], op: :adjective_modifier},
    %{lhs: :nominal, rhs: [:noun, :nominal], op: :noun_modifier},
    %{lhs: :subject, rhs: [:nominal], op: :bare_subject},
    %{lhs: :subject, rhs: [:pronoun], op: :pronoun_subject},
    %{lhs: :subject, rhs: [:proper_noun], op: :proper_subject},
    %{lhs: :subject, rhs: [:possessive_np], op: :possessive_subject},
    %{lhs: :subject, rhs: [:determiner, :nominal], op: :determined_subject},
    %{lhs: :subject, rhs: [:determiner, :subject], op: :determined_possessive_subject},
    %{lhs: :subject, rhs: [:quantifier, :nominal], op: :quantified_subject},
    %{lhs: :subject, rhs: [:subject, :pp], op: :subject_adjunct},
    %{lhs: :pp, rhs: [:preposition, :subject], op: :pp},
    %{lhs: :vp, rhs: [:copula, :complement], op: :copular_vp},
    %{lhs: :vp, rhs: [:vp, :pp], op: :predicate_pp},
    %{lhs: :vp, rhs: [:vp, :adverb], op: :predicate_adverb},
    %{lhs: :sentence, rhs: [:subject, :vp], op: :predication}
  ]

  @doc false
  def rules, do: @rules
  @doc false
  def unary_rules, do: Enum.filter(@rules, &(length(&1.rhs) == 1))
  @doc false
  def binary_rules, do: Enum.filter(@rules, &(length(&1.rhs) == 2))

  @doc false
  def copula?(word), do: word in @copulas
  @doc false
  def quantifier?(word), do: word in @quantifiers
  @doc false
  def determiner?(word), do: word in @determiners
  @doc false
  def pronoun?(word), do: word in @pronouns
  @doc false
  def negative?(word), do: word in @negative_words
  @doc false
  def modal?(word), do: word in @modals
  @doc false
  def subordinator?(word), do: word in @subordinators

  @doc false
  def agreement(word), do: Map.get(@agreement, word, :unknown)

  @doc false
  def pronoun_number(word) do
    cond do
      word in @plural_pronouns -> :plural
      word in @singular_pronouns -> :singular
      true -> :unknown
    end
  end
end
