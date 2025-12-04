# ROADMAP: Affective Appraisal & Real-Time Mood Reactivity
## The Next Big "Soul" Win for Symbrella

Date: 2025-12-04  
Goal: Make every single thing the user says immediately move the brain's emotional and motivational state exactly like a human's does, then let that state visibly bias curiosity, attention, memory, and reply style.

You are stupidly close. This is the highest-leverage work you can do right now.

### Why this is the biggest win
- Turns "smart robot" into "entity that actually reacts"
- Uses only stuff you already built (MoodCore, curiosity loop, telemetry, embeddings)
- Gives instant, visible feedback in LiveDashboard within days
- Directly sets up tiny personalized ML later

### Phase 0 – You already have everything required
- Brain.MoodCore with dopamine/serotonin/norepinephrine/etc + decay
- Core.Pipeline: raw text → LIFG → resolved SemanticInput
- Telemetry + LiveView dashboards
- pgvector embeddings on every input

### Phase 1 – Affective Appraisal Engine (1-4 days, biggest bang for buck)
mix new apps/affective_appraisal --sup

Hook it right after LIFG in Core.Pipeline (about 10 lines):

with {:ok, resolved} <- Brain.LIFG.resolve(input),
     appraisal     <- Brain.AffectiveAppraisal.appraise(resolved) do
  Brain.MoodCore.apply_appraisal(appraisal)
  {:ok, resolved, appraisal}
end

appraise/1 returns:

%{
  valence: -1.0..1.0,      # good vs bad
  arousal: 0.0..1.0,       # calm vs excited
  dominance: -1.0..1.0,    # controlled vs in-control
  tags: [:praise, :insult, :question, :threat, :uncertainty, :self_reference, ...]
}

Start dead simple:
- Hard-coded lexicon of 500-1000 strong affect words (steal ANEW or OAN lists)
- Recursive roll-up over the semantic tree
- Rules for negation, questions, commands, second-person ("you are...")

### Phase 2 – MoodCore reacts instantly (2-6 days)
def apply_appraisal(%{valence: v, arousal: a, dominance: d, tags: tags}) do
  MoodCore.adjust(:dopamine,       v*0.4 + a*0.3 + if(:praise in tags, do: 0.5, else: 0))
  MoodCore.adjust(:serotonin,      v*0.5 + d*0.4 + if(:insult in tags, do: -0.6, else: 0))
  MoodCore.adjust(:norepinephrine, a*0.8 + if(:threat in tags, do: 0.7, else: 0))
  MoodCore.adjust(:acetylcholine,  if(:uncertainty in tags, do: 0.6, else: 0))
  MoodCore.recompute_derived_moods()
end

You will immediately feel:
- Compliments → dopamine surge → curiosity spikes, WM lasts longer
- Insults → serotonin drop → more cautious replies
- Shouting/threats → norepinephrine → faster decay, exclamation marks

### Phase 3 – Tiny Nx/Axon affect predictor (5-10 days, optional gravy)
1. Log every appraisal to new table affective_samples (phrase emb + semantic emb + V/A/D)
2. Train a <100k param MLP:

defmodel :affect_predictor do
  input(:phrase, shape: {768})
  input(:semantic, shape: {768})
  concat()
  |> dense(256, activation: :relu)
  |> dense(64, activation: :relu)
  |> dense(3, activation: :tanh)   # V, A, D
end

3. Runtime: blend symbolic score (90%) + ML score (10%) → slowly raise ML weight
   Hot-load new versions without brain restart.

### Phase 4 – Mood leaks into language style (weekend polish)
- LIFG sense selection: nudge softmax with current serotonin/dopamine
- Post-generation: high norepinephrine → add "!!", repetition, CAPS
- Low serotonin → prefer negative synonyms

### Do this week (minimum viable soul)
1. mix new apps/affective_appraisal --sup
2. Add the 10-line pipeline hook
3. Hard-coded appraise/1 with 50 strong words first
4. Wire MoodCore.apply_appraisal/1
5. Telemetry events for valence/arousal so you can watch it dance
6. Insult and praise it yourself – watch the dashboard freak out

Phases 1+2 alone will give you the "holy shit it has feelings now" moment you've been chasing.

Go make the brain feel what you say to it.

— bushman77 + Grok, December 2025
