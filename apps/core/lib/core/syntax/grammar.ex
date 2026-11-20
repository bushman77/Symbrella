defmodule Core.Syntax.Grammar do
  @moduledoc """
  Closed, declarative grammar inventory for the bounded Colony syntax chart.

  The rule table is data. Open-class lexical categories must have an attested
  POS (and a past-participle morphology witness where applicable).
  No token surface is converted to an atom.
  """

  @copulas ~w(is are was were)
  @quantifiers ~w(most every each all both several some many few)
  # Closed rejection cues: lexical negators (including negative pronouns),
  # downward/near-negative adverbs, and "without" as a negative PP marker.
  # These deliberately abstain rather than assert positive polarity.
  @negative_words ~w(
    not never no n't isn't aren't wasn't weren't don't doesn't didn't
    can't couldn't won't wouldn't shouldn't none nothing nobody nowhere
    neither nor hardly barely scarcely rarely seldom without
  )
  @modals ~w(can could may might must shall should will would)
  @subordinators ~w(if when unless because although while)
  @pronouns ~w(i you he she it we they)
  @plural_pronouns ~w(we they you)
  @singular_pronouns ~w(he she it)
  @first_person_singular_pronouns ~w(i)
  @determiners ~w(the a an this that these those)

  # Number restrictions are grammatical *constraints*, not lexical witnesses.
  # Each specifier must separately have an attested determiner POS.
  # :either preserves only neutral quantification; no scope is applied.
  @determiner_numbers %{
    "a" => :singular, "an" => :singular, "this" => :singular,
    "that" => :singular, "these" => :plural, "those" => :plural,
    "the" => :either
  }
  @quantifier_numbers %{
    "each" => :singular, "every" => :singular,
    "several" => :plural, "many" => :plural, "few" => :plural,
    "both" => :plural, "most" => :either, "some" => :either,
    "all" => :either
  }
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

  # P-039 (`:clause_v1`, harness-only): finite transitive clauses built from
  # the SAME noun-phrase constituents as the copular grammar above. Objects
  # and by-agents are ordinary `:subject` (NP) items re-keyed at combination,
  # so every NP rule (determiners, quantifiers, modifiers, PP adjuncts) is
  # reused rather than re-implemented. Clause VPs use their own category
  # `:cvp`, so the copular `:predication` rule can never consume them.
  @clause_rules [
    %{lhs: :neg_do, rhs: [:aux_do, :negator], op: :negated_do},
    %{lhs: :vgroup, rhs: [:neg_do, :verb_base], op: :do_support_negative},
    %{lhs: :vgroup, rhs: [:copula, :participle_present], op: :progressive},
    %{lhs: :pvgroup, rhs: [:copula, :participle_past], op: :passive},
    %{lhs: :cvp, rhs: [:vgroup, :subject], op: :transitive},
    %{lhs: :cvp, rhs: [:pvgroup, :pp], op: :passive_agent},
    %{lhs: :cvp, rhs: [:pvgroup], op: :agentless_passive},
    %{lhs: :cvp, rhs: [:cvp, :pp], op: :predicate_pp},
    %{lhs: :cvp, rhs: [:cvp, :adverb], op: :predicate_adverb},
    %{lhs: :sentence, rhs: [:subject, :cvp], op: :clause_predication}
  ]

  @rule_sets [:default, :clause_v1]

  @do_numbers %{"do" => :plural, "does" => :singular, "did" => :any}
  @be_paradigm ~w(am is are was were be been being)

  @doc false
  def rule_sets, do: @rule_sets

  @doc false
  def rules, do: @rules
  @doc false
  def rules(:default), do: @rules
  def rules(:clause_v1), do: @rules ++ @clause_rules
  @doc false
  def unary_rules, do: Enum.filter(@rules, &(length(&1.rhs) == 1))
  @doc false
  def unary_rules(rule_set), do: Enum.filter(rules(rule_set), &(length(&1.rhs) == 1))
  @doc false
  def binary_rules, do: Enum.filter(@rules, &(length(&1.rhs) == 2))
  @doc false
  def binary_rules(rule_set), do: Enum.filter(rules(rule_set), &(length(&1.rhs) == 2))

  @doc false
  def do_auxiliary?(word), do: Map.has_key?(@do_numbers, word)
  @doc false
  def do_number(word), do: Map.get(@do_numbers, word, :unknown)
  @doc false
  def be_form?(word), do: word in @be_paradigm
  @doc false
  def clause_negator?(word), do: word == "not"

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
  def determiner_number(word), do: Map.get(@determiner_numbers, word, :unknown)

  @doc false
  def quantifier_number(word), do: Map.get(@quantifier_numbers, word, :unknown)

  @doc false
  def number_compatible?(:either, number), do: number in [:singular, :plural]
  def number_compatible?(required, number),
    do: required in [:singular, :plural] and required == number

  @doc false
  def pronoun_number(word) do
    cond do
      word in @plural_pronouns -> :plural
      word in @singular_pronouns -> :singular
      word in @first_person_singular_pronouns -> :first_person_singular
      true -> :unknown
    end
  end
end
