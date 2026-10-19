# mix run scripts/probe_speaker_location_fidelity.exs
#
# Read-only response-review proof, using the two actual failure drafts
# captured in the 2026-10-18 LIFG / Blackboard diagnostic export.
# No LLM, network, new memories, working-memory updates or synapses.

alias Core.Response.{ReflectionLoop, SpeakerLocationFidelity}

wrong = "I'm currently located in Kamloops, British Columbia, Canada."
question = "where am i atm"
correction = "no you are not"

{:ok, answer, first} =
  ReflectionLoop.review(question, wrong,
    %{features: %{intent: :ask, conf: 0.8125}})

history = [
  %{"role" => "user", "content" => question},
  %{"role" => "assistant", "content" => wrong}
]

{:ok, denial_answer, second} =
  ReflectionLoop.review(correction, wrong,
    %{features: %{intent: :deny, conf: 0.82},
      review_history: history})

true_answer = "According to Wikipedia, Kamloops is in British Columbia."
{:ok, public_answer, public_review} =
  ReflectionLoop.review("where is kamloops bc", true_answer, %{})

IO.puts("\n=== Speaker / location fidelity (READ ONLY) ===")
IO.inspect(%{
  prior_user_location_question: question,
  original_unsupported_self_location: wrong,
  first_review: Map.take(first, [:status, :issues, :applied?]),
  safe_first_reply: answer,
  user_correction: correction,
  prior_assistant_claim_is_correction_target?: true,
  second_review: Map.take(second, [:status, :issues, :applied?]),
  safe_correction_reply: denial_answer,
  public_place_attribution_guard: SpeakerLocationFidelity.issue(
    "where is kamloops bc", true_answer, %{}),
  public_place_response_unchanged?: public_answer == true_answer and
    :speaker_location_fidelity not in public_review.issues,
  external_requests?: false,
  memory_or_synapse_writes?: false
}, pretty: true)

unless first.status == :repair and
    second.status == :repair and
    :speaker_location_fidelity in first.issues and
    :speaker_location_fidelity in second.issues and
    answer != wrong and denial_answer != wrong and
    public_answer == true_answer and
    :speaker_location_fidelity not in public_review.issues do
  raise "Speaker-location attribution review failed"
end

IO.puts("PASS: deictic user location and correction cannot emit assistant's invented city.")
