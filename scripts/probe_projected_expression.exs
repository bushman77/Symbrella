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
  echo_review = ProjectedExpression.review_draft(sentence, input_echo, frame)

  # The real Qwen drafts previously turned these finite-present reports
  # into present perfect, incorrectly suggesting a completed action.
  # This diagnostic only previews the response-review decision.
  completed_action_draft =
    case i do
      1 -> "The gardener has planted a tree."
      2 -> "The baker has cooked a meal."
      3 -> "The child has thrown a ball."
      4 -> "The captain has steered a ship."
    end

  temporal_review =
    ProjectedExpression.review_draft(sentence, completed_action_draft, frame)

  IO.inspect(%{
    case: i,
    input: sentence,
    intended_speech_act: Map.get(si, :intent),
    projected_sentence_status:
      Map.get(Map.get(si, :sentence_meaning) || %{}, :status),
    frame: frame,
    repeated_echo_review: echo_review,
    model_completed_action_draft: completed_action_draft,
    completed_action_review: temporal_review,
    changed_lifg?: false,
    changed_response?: false,
    sense_chosen?: false,
    events_written?: false
  }, label: "Expression case #{i}", pretty: true, limit: :infinity)
end

IO.puts("""
The preview shows deterministic fallbacks for an exact model echo
or a simple-present utterance incorrectly restated as a completed action.
Normal live chat first asks the configured LLM to respond naturally;
the fallback applies only if a real draft fails narrow checks.
""")
