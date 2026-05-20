defmodule Core.Pipeline.PerceptionTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Perception

  test "attaches deterministic perception signals" do
    si = %{
      tokens: [
        %{index: 0, phrase: "bank", span: {0, 4}, n: 1, mw: false},
        %{index: 1, phrase: "river bank", span: {0, 10}, n: 2, mw: true}
      ],
      sense_candidates: %{
        0 => [
          %{id: "bank|noun|finance", score: 0.52},
          %{id: "bank|noun|river-edge", score: 0.5}
        ],
        1 => [
          %{id: "river bank|phrase|edge", score: 0.9}
        ]
      },
      evidence: %{source: :test},
      episode: nil,
      trace: []
    }

    out = Perception.run(si, [])

    assert %{
             backend: :deterministic,
             version: 1,
             token_vectors: token_vectors,
             salience: salience,
             ambiguity: ambiguity,
             candidate_bias: candidate_bias,
             phrase_coherence: phrase_coherence,
             meta: %{token_count: 2, candidate_bucket_count: 2}
           } = out.perception

    assert is_list(token_vectors[0])
    assert is_list(token_vectors[1])
    assert salience[1] > salience[0]
    assert ambiguity[0] > ambiguity[1]
    assert candidate_bias[0]["bank|noun|finance"] > candidate_bias[0]["bank|noun|river-edge"]
    assert phrase_coherence[1] > phrase_coherence[0]
    assert [%{stage: :perception, meta: %{backend: :deterministic}} | _] = out.trace
  end
end
