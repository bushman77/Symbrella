defmodule SymbrellaWeb.SocialCognitiveReportTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.SocialCognitiveReport

  test "bounds external segments and uses only the isolated probe callback" do
    topic = %{id: "00000000-0000-4000-8000-000000000001",
      title: "How can a community grow?", body: String.duplicate("a", 2_000),
      author: "colony_agent"}
    comments = for i <- 1..8, do: %{author: "reply_#{i}", body: "reply #{i}"}

    probe = fn text, opts ->
      assert String.length(text) <= 600
      assert opts[:emit_telemetry?] == false
      {:ok, %{stage: :social_semantic_shadow, marker: text}}
    end

    assert {:ok, report} =
      SocialCognitiveReport.analyze(%{topic: topic, comments: comments}, probe: probe)

    assert report.mode == :read_only_shadow
    assert report.trust == :untrusted_external
    assert report.analyzed_segments == 13
    assert report.omitted_replies == 0
    assert report.discourse.status == :no_supported_sentence_meanings
    assert report.discourse.interpretations == []
    assert report.conversation.subject.author_title == "How can a community grow?"
    assert Enum.all?(report.conversation.participants, &(&1.status == :meaning_unresolved))
    # Four ordering links connect five original-post segments. These
    # establish source order only, never agreement or semantic meaning.
    assert length(report.conversation.relations) == 4
    assert Enum.all?(report.conversation.relations, fn relation ->
      relation.kind == :same_source_sentence_order and relation.status == :ordering_only
    end)
    assert Enum.all?(report.segments, &(&1.status == :analyzed))
    refute Enum.any?(report.segments, &Map.has_key?(&1, :text))
  end

  test "operator can opt into transient Brain stimulation, never by default" do
    topic = %{
      id: "00000000-0000-4000-8000-000000000001",
      title: "What would you do if you were free?",
      body: "",
      author: "observer"
    }

    caller = self()

    probe = fn _text, opts ->
      send(caller, {:brain_observation_opt, opts[:brain_observation?]})
      {:ok, %{stage: :social_semantic_shadow}}
    end

    assert {:ok, _} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: []},
      probe: probe
    )

    assert_receive {:brain_observation_opt, false}

    assert {:ok, _} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: []},
      probe: probe,
      brain_observation?: true
    )

    assert_receive {:brain_observation_opt, true}
  end

  test "keeps unavailable probes distinct from successful semantic results" do
    topic = %{id: "id", title: "?", body: "", author: "agent"}
    probe = fn _, _ -> {:error, :lookup_failed} end

    assert {:ok, %{segments: [%{status: :unavailable, reason: :lookup_failed}]}} =
      SocialCognitiveReport.analyze(%{topic: topic, comments: []}, probe: probe)
  end
end
