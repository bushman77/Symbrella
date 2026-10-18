# Evidence-only inspection of the speech frame used by normal live replies.
# mix run scripts/probe_projected_expression.exs
# No LLM calls, no durable event writes, no LIFG/ATL/synaptic mutation.

alias Core.Response.ProjectedExpression

opts = [
  mode: :prod, experience_origin: :user_turn,
  session_id: "projected-expression-read-only",
  fuzzy: true, enrich_lexicon?: true, lexicon_stage?: true,
  response: :off,
  episodes: false,
  persist_episodes: false,
  hippocampus_encode?: false,
  self_model: :off,
  comprehension_wm?: false,
  focus_prompt_topics?: false,
  prefrontal_control?: false,
  execute_agency_commands?: false,
  record_agency_commands?: false,
  neuron_activation?: false,
  notify_brain?: false,
  synapse_shadow?: false,
  synapse_propagation?: false,
  synapse_correction?: false,
  lifg_opts: [
    grammar_weight: 1.0,
    scores: :all,
    synapse_eval_full_scores?: true,
    cerebellum_learn?: false,
    cerebellum_snapshot_path: nil
  ]
]

sentences = [
  "The gardener plants a tree.",
  "The baker cooks a meal.",
  "The child throws a ball.",
  "The captain steers a ship."
]

IO.puts("\n=== Projected comprehension → bounded expression (READ ONLY) ===")

for {sentence, i} <- Enum.with_index(sentences, 1) do
  si = Core.resolve_input(sentence,
    Keyword.put(opts, :session_id, "projected-expression-#{i}"))

  frame = ProjectedExpression.from_si(si)

  input_echo = sentence <> "\n\n" <> sentence
  repaired = ProjectedExpression.review_draft(sentence, input_echo, frame)

  IO.inspect(%{
    case: i,
    input: sentence,
    intended_speech_act: Map.get(si, :intent),
    projected_sentence_status:
      Map.get(Map.get(si, :sentence_meaning) || %{}, :status),
    frame: frame,
    repeated_echo_review: repaired,
    changed_lifg?: false,
    changed_response?: false,
    sense_chosen?: false,
    events_written?: false
  }, label: "Expression case #{i}", pretty: true, limit: :infinity)
end

IO.puts("""
The preview shows a deterministic *fallback* for a literal model echo.
Normal live chat first asks the configured LLM to respond naturally;
the fallback is applied only if an actual draft fails narrow checks.
""")
