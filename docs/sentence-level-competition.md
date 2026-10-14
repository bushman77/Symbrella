# Sentence-level semantic competition (initial bounded implementation)

Sentence interpretation must be able to contest a locally plausible
dictionary MWE *before* a virtual MWE token shadows compositional words.
Existing LIFG sense IDs, lexical candidates and ATL diagnostics remain
unchanged; this is a higher-level compatibility decision, not a replacement
dictionary or a new learned model.

## Stage order

1. Core.MWE.Lexicon attaches real contiguous dictionary phrase senses.
2. Core.Syntax.Signatures recognizes complete WH–auxiliary–subject–verb
   questions with an explicit prepositional topic and noun/pronoun evidence.
   It also retains all existing low-confidence sentence signatures.
3. Core.Syntax.SentenceCompetition compares each *overlapping* dictionary
   MWE against that complete sentence frame using bounded compatibility
   scores: 0.90 composition; 0.25 + 0.60 * ContextFit for the lexical
   interpretation. A 0.16 margin and minimum 0.80 composition score are
   required to prefer compositional structure. These are deliberately
   conservative heuristics, not LIFG probabilities.
4. TokenResolution declines only those MWE spans defeated by the complete
   frame; otherwise dictionary-backed token-cover precedence is unchanged.
5. Core.MWE.Decisions checks the SAME sentence verdict, preventing a later
   stale MWE candidate from restoring an overruled phrase.
6. ATL consumes the canonical meaning, including the question-signature
   roles, and Explain displays the actual frame, winner, scores and margin.

**No recursion or arbitrary rerun is performed.** Sentence-level assessment
is bounded to 8 frames, 32 dictionary phrases and 8 senses per phrase.
Question without sufficient grammatical evidence remains ambiguous and
keeps the previous lexical MWE outcome rather than fabricating confidence.

Example strong frame: 'What do you know about Japan's railgun development?'
Example without a complete topic frame: 'Well, what do you know?'
Example other clause: 'What did she say about the experiment?'
Example unrelated MWE: 'turn off' later in the topic is left alone.

## Test and inspect

    mix test apps/core/test/core/syntax/sentence_competition_test.exs
    mix test apps/core/test/core/mwe/decisions_test.exs
    mix test apps/core/test/core/syntax/signatures_test.exs
    mix test apps/symbrella_web/test/symbrella_web/live/explain_evidence_test.exs
    mix run scripts/trace_question_mwe_promotion.exs

Experimental bounds: this first frame does not handle all syntactic
questions, idiom styles or language varieties. Missing lexical POS evidence
must not be replaced with an invented dictionary sense. LIFG can still
produce weak or incorrect senses on *individual* words; the sentence
competition does not claim full sentence understanding.
