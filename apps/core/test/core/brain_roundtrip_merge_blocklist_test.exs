# apps/core/test/core/brain_roundtrip_merge_blocklist_test.exs
defmodule Core.BrainRoundtripMergeBlocklistTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput

  test "Core refuses to merge per-turn derived fields from Brain output (tokens/sense_candidates/etc.)" do
    si =
      %SemanticInput{
        sentence: "hello",
        source: :prod,
        tokens: [%{index: 0, phrase: "hello"}],
        sense_candidates: %{0 => [%{id: "hello|interjection|0"}]},
        trace: [%{stage: :core_seed}]
      }

    brain_out = %{
      # Non-atom keys are ignored by merge
      "junk" => 123,

      # These should be blocked
      :sentence => "POISON",
      :source => :brain,
      :tokens => [%{index: 999, phrase: "poison"}],
      :token_structs => [%{oops: true}],
      :pos_list => [:oops],
      :cells => [:oops],
      :pattern_roles => %{0 => :oops},
      :phrase_matches => [%{oops: true}],
      :sense_candidates => %{0 => [%{id: "poison|noun|0"}]},

      # These may be merged (not in blocklist)
      :lifg_choices => [%{id: "hello|interjection|0", token_index: 0, score: 1.0}],
      :evidence => %{episodes: [%{at: 123, score: 0.9}]},

      # Trace should merge (not clobber)
      :trace => [%{stage: :brain_stm}]
    }

    out = Core.__merge_brain_out__(si, brain_out)

    # Blocklist is enforced
    assert out.sentence == "hello"
    assert out.source == :prod
    assert out.tokens == [%{index: 0, phrase: "hello"}]
    assert out.sense_candidates == %{0 => [%{id: "hello|interjection|0"}]}

    # Allowed fields can still be merged
    assert out.lifg_choices == [%{id: "hello|interjection|0", token_index: 0, score: 1.0}]
    assert out.evidence == %{episodes: [%{at: 123, score: 0.9}]}

    # Trace merges rather than overwrites (Brain events come first)
    assert Enum.take(out.trace, 2) == [%{stage: :brain_stm}, %{stage: :core_seed}]
  end
end

