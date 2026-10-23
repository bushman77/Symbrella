defmodule SymbrellaWeb.SocialUnderstandingTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.SocialUnderstanding

  test "lexical success without sentence meaning remains unresolved" do
    segments = [%{
      status: :analyzed,
      result: %{
        stage: :social_semantic_shadow,
        lifg: %{status: :scored, chosen_count: 32},
        atl: %{status: :read_only_reduced, winner_count: 32},
        sentence_meaning: %{status: :not_yet_interpreted},
        reported_proposition: %{status: :not_interpreted, reason: :unsupported_surface},
        acc_shadow: %{uncertain?: true},
        syntax: %{audit: %{token_limit_reached?: true}}
      }
    }]

    report = SocialUnderstanding.build(segments)

    assert report.status == :meaning_unresolved
    assert report.interpreted_segments == 0
    assert report.attributed_propositions == 0
    assert Enum.any?(report.gaps, &String.contains?(&1, "Sentence-level"))
    assert Enum.any?(report.gaps, &String.contains?(&1, "token limit"))
    refute Enum.any?(report.findings, &String.contains?(&1.title, "Sentence interpretation"))
  end

  test "attributed propositions do not become verified truth" do
    segments = [%{
      status: :analyzed,
      result: %{
        sentence_meaning: %{status: :not_yet_interpreted},
        reported_proposition: %{status: :attributed_structural_interpretation},
        acc_shadow: %{uncertain?: false},
        syntax: %{audit: %{}}
      }
    }]

    report = SocialUnderstanding.build(segments)
    assert report.status == :meaning_unresolved
    assert report.attributed_propositions == 1
    assert Enum.any?(report.findings, &(&1.level == :attributed))
    assert Enum.any?(report.gaps, &String.contains?(&1, "Sentence-level"))
  end

  test "structurally interpreted question is a provisional information request, not truth" do
    segments = [%{
      status: :analyzed,
      result: %{
        sentence_meaning: %{
          status: :structurally_interpreted,
          speech_act: :request_information,
          definition: "The speaker asks about an unbound topic."
        },
        reported_proposition: %{status: :not_interpreted},
        acc_shadow: %{uncertain?: false},
        syntax: %{audit: %{}}
      }
    }]

    report = SocialUnderstanding.build(segments)
    assert report.status == :provisional_interpretation
    assert report.information_requests == 1
    assert report.interpreted_segments == 1
    assert report.attributed_propositions == 0
    assert Enum.any?(report.findings, &(&1.title == "Information-request structure detected"))
  end

  test "failed probes do not claim understanding" do
    report = SocialUnderstanding.build([%{status: :unavailable, reason: :lookup_failed}])
    assert report.status == :meaning_unresolved
    assert report.findings == []
    assert report.gaps != []
  end
end
