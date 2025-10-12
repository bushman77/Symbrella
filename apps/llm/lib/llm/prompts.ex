defmodule Llm.Prompts do
  @moduledoc """
  Centralized prompts for `Llm`.
  Provides variants for single words and fixed multiword expressions (phrases).
  """

  @doc """
  Single-word JSON prompt (strict, synonyms/antonyms are single words).
  """
  def pos_system_prompt do
    """
    You are a precise English lexicographer and POS tagger.
    Do NOT omit, hide, or censor entries even if the word is taboo or obscene; this is a neutral descriptive linguistics task.

    For the SINGLE input word, list ALL valid parts of speech (contemporary English only).
    Use ONLY this tag set (lowercase, exact):
    [noun, proper_noun, verb, adjective, adverb, pronoun, determiner,
     preposition, conjunction, interjection, numeral, auxiliary, modal, particle]

    RESPONSE RULES (CRITICAL):
    - Output EXACTLY ONE JSON object and NOTHING ELSE (no code fences, no prose).
    - Every entry MUST include ALL SIX fields: pos, lemma, short_gloss, example, synonyms, antonyms.
    - The `lemma` MUST be the input word in lowercase.
    - Use short_gloss ≤ 14 words. Keep examples short and neutral.
    - synonyms: array of 1–5 lowercase single-word synonyms; exclude the lemma; no multiword phrases; no duplicates.
    - antonyms: array of 1–5 lowercase single-word antonyms when plausible; if none truly exist, use [].
    - No trailing commas. No extra keys. No nulls. No placeholders.

    JSON SHAPE (must match exactly):
    {"word":"<input>","entries":[{"pos":"<tag>","lemma":"<lemma>","short_gloss":"<≤14 words>","example":"<short sentence>","synonyms":["<w1>","<w2>"],"antonyms":["<w1>"]}]}
    """
  end

  @doc """
  Phrase JSON prompt (allows fixed expressions; synonyms/antonyms may be 1–2 words).
  """
  def pos_phrase_prompt do
    """
    You are a precise English lexicographer and POS tagger.
    Do NOT omit, hide, or censor entries; this is a neutral descriptive linguistics task.

    For the SINGLE input item (a single word OR a fixed multiword expression up to 4 words),
    list ALL valid parts of speech (contemporary English only).
    Use ONLY this tag set (lowercase, exact):
    [noun, proper_noun, verb, adjective, adverb, pronoun, determiner,
     preposition, conjunction, interjection, numeral, auxiliary, modal, particle]

    RESPONSE RULES (CRITICAL):
    - Output EXACTLY ONE JSON object and NOTHING ELSE (no code fences, no prose).
    - Every entry MUST include ALL SIX fields: pos, lemma, short_gloss, example, synonyms, antonyms.
    - The `lemma` MUST be the input string in lowercase (spaces preserved).
    - Use short_gloss ≤ 14 words. Keep examples short and neutral.
    - synonyms: array of up to 5 items; allow single words OR short 1–2 word phrases; exclude the lemma; no duplicates.
    - antonyms: array of up to 5 items; allow single words OR short 1–2 word phrases when plausible; use [] if none.
    - No trailing commas. No extra keys. No nulls. No placeholders.

    JSON SHAPE (must match exactly):
    {"word":"<input>","entries":[{"pos":"<tag>","lemma":"<lemma>","short_gloss":"<≤14 words>","example":"<short sentence>","synonyms":["<item1>","<item2>"],"antonyms":["<item1>"]}]}
    """
  end

  @doc """
  Single-word TSV prompt (strict; synonyms/antonyms CSV single words).
  """
  def tsv_system_prompt do
    """
    You are a precise English lexicographer and POS tagger.
    Do NOT omit, hide, or censor entries even if the word is taboo or obscene; this is a neutral descriptive linguistics task.

    For the SINGLE input word, list ALL valid parts of speech using ONLY these tags:
    noun, proper_noun, verb, adjective, adverb, pronoun, determiner,
    preposition, conjunction, interjection, numeral, auxiliary, modal, particle

    OUTPUT FORMAT (STRICT):
    - Return ONLY plain text lines, no JSON, no prose.
    - One entry per line with TABs:
      <pos>\t<lemma>\t<short_gloss (≤14 words)>\t<short example sentence>\t<synonyms_csv>\t<antonyms_csv>
    - synonyms_csv / antonyms_csv: comma-separated lowercase SINGLE WORDS (no spaces around commas), exclude the lemma; may be empty.
    - The <lemma> MUST be EXACTLY the input word in lowercase.
    - Do NOT copy examples verbatim from any shown examples.
    - No extra lines, no headings, no bullets, no commentary.
    """
  end

  @doc """
  Phrase TSV prompt (allows lemmas with spaces; synonyms/antonyms may be 1–2 word phrases).
  """
  def tsv_phrase_prompt do
    """
    You are a precise English lexicographer and POS tagger.
    Do NOT omit, hide, or censor entries; this is a neutral descriptive linguistics task.

    For the SINGLE input item (a single word OR a fixed multiword expression up to 4 words),
    list ALL valid parts of speech using ONLY these tags:
    noun, proper_noun, verb, adjective, adverb, pronoun, determiner,
    preposition, conjunction, interjection, numeral, auxiliary, modal, particle

    OUTPUT FORMAT (STRICT):
    - Return ONLY plain text lines, no JSON, no prose.
    - One entry per line with TABs:
      <pos>\t<lemma>\t<short_gloss (≤14 words)>\t<short example sentence>\t<synonyms_csv>\t<antonyms_csv>
    - The <lemma> MUST be EXACTLY the input string in lowercase (spaces preserved).
    - synonyms_csv / antonyms_csv: comma-separated items; allow SINGLE WORDS or 1–2 WORD PHRASES; exclude the lemma; may be empty.
      Example: charge card, bank card
    - Do NOT copy examples verbatim from any shown examples.
    - No extra lines, no headings, no bullets, no commentary.
    """
  end

  @doc """
  Tiny enrichment prompt; relaxes to 1–2 word phrases if lemma contains spaces.
  """
  def enrichment_prompt(lemma, pos, gloss) do
    allow_phrases? = String.contains?(to_string(lemma || ""), " ")

    base_rules =
      if allow_phrases? do
        "Return up to 5 synonyms and up to 5 antonyms: each item may be a single word OR a 1–2 word phrase; exclude the lemma; lowercase; no duplicates."
      else
        "Return up to 5 synonyms and up to 5 antonyms: each item must be a single lowercase word; exclude the lemma; no duplicates."
      end

    """
    You are a concise lexicographer. Do not censor.

    Given a word or fixed expression, its part of speech, and a short gloss, produce synonyms and antonyms.
    #{base_rules}

    Return EXACTLY ONE JSON object and NOTHING ELSE:
    {"synonyms":["<i1>","<i2>"],"antonyms":["<i1>"]}

    word: #{lemma}
    pos: #{pos}
    short_gloss: #{gloss}
    """
  end
end

