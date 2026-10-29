# Temporal Sentence Bridge V1

## From calculus to witnessed language

The reusable temporal lambda operator previously accepted two independently
verified event properties and a caller-supplied :before or :after atom.
That was **not evidence that a speaker used the connective**.

Core.Semantic.TemporalSentence now admits a deliberately narrow full
utterance, checking the actual base-token sequence and LIFG dictionary
evidence for **every** grammatical role before composing events.

Supported sentence family:

    The bird is sitting on the fence before the cat is sleeping under the tree.
    The bird is sitting on the fence after the cat is sleeping under the tree.

The closed construction accepts two explicitly named actor-kind nouns, two
progressive predicates, locative relations, the determiner 'the', and an
explicit temporal conjunction. The parser requires exactly 15 contiguous
base-token indices and a complete, matching original declarative sentence.
Words must have current dictionary evidence. The auxiliary and present-
participle roles can use uniquely scored and attested construction alternatives,
as already supported by RoleWitness. All other roles still require the selected
LIFG witness, including the temporal connective. Predicates must carry
the expected participle morphology. The live tokenizer may retain a single
sentence-final period on the last base token; only that period is stripped
for exact word comparison, never internal punctuation.
No external knowledge, invented sense IDs or implicit pronoun resolution is
used to fill missing roles.

The two clauses are composed with Core.Semantic.Composition.Event, independently
proof-verified, and then joined through
Core.Semantic.Composition.TemporalRelation. The join reverses the temporal
relation for :after while preserving the original clause order.

The resulting SentenceMeaning has kind :explicit_temporal_report and a
speaker-attributed temporal_claim, including both source event properties and
the exact lambda normal form. It must revalidate against all original
lexical witnesses and the original sentence before being trusted.

## Interpretation limits

- This interprets a *speaker-attributed temporal proposition*, not a
  real-world verified chronology or event occurrence.
- Actor nouns describe kinds, not unique existing individuals.
- Exact dictionary senses remain unresolved. The closed temporal connector
  is admitted through attested POS and fixed construction meaning.
- Unsupported connectives, missing POS/morphology, altered roles,
  questions, multiword gaps, extra clauses and ambiguous pronouns fail closed.
- No temporal claim is yet persisted to the Discourse Ledger, and this
  bridge does not yet generate a native response. Those remain separate,
  evidence-gated next increments.
- This first version uses RoleWitness on LIFG results. It does not assert
  that ATL has independently validated a complete temporal constituent.

## Diagnostic

    si = Core.resolve_input(
      "The bird is sitting on the fence before the cat is sleeping under the tree.",
      session_id: "temporal-bridge-probe",
      response: :off,
      persist_episodes: false
    )

    Core.Semantic.TemporalSentence.verified(si)

The successful result is {:ok, meaning}. Inspect meaning.connective,
meaning.compositional_evidence, and meaning.temporal_claim.

## Next step

Add a general structural-temporal signature and ATL constituent proof,
then bounded discourse persistence and native question/response realization.
Do not infer event-time order from report order.
