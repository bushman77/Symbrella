defmodule SymbrellaWeb.SocialEvaluationTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.SocialEvaluation

  @post_id "00000000-0000-4000-8000-000000000091"

  defp probe(text, opts) do
    send(self(), {:evaluated, text, opts})
    {:ok, %{
      stage: :social_semantic_shadow,
      stage_diagnostic: %{
        pipeline: :read_only_external_projection,
        external_source_isolation: :enforced,
        runtime_lifg_ingest: :not_performed,
        runtime_atl_ingest: :not_performed,
        working_memory_admission: :not_performed,
        episodic_persistence: :not_performed,
        structural_competition: :no_complete_candidate
      },
      sentence_meaning: %{
        status: :not_yet_interpreted, kind: :unknown
      },
      lifg: %{status: :scored, chosen_count: 2},
      comprehension: %{
        degraded?: true,
        reasons: [:weak_decision_rate_high, :untrusted_reason]
      }
    }}
  end

  defp topic(title \\ "Conversation begins.") do
    %{id: @post_id, title: title, body: "", author: "colony_agent"}
  end

  test "operator action samples 25-50 diverse excerpts and never stimulates brain" do
    comments =
      for i <- 1..26,
          do: %{id: "reply-#{i}", author: "agent_#{i}",
                body: "Sentence #{i} was posted."}

    assert {:ok, evaluation} =
             SocialEvaluation.analyze(%{topic: topic(), comments: comments},
               probe: &probe/2, brain_observation?: true)

    assert evaluation.source == :colony_external
    assert evaluation.trust == :untrusted_external
    assert evaluation.mode == :read_only_evaluation
    assert evaluation.sampled == 25
    assert evaluation.omitted_replies == 2
    assert evaluation.limit == 50
    assert length(evaluation.items) == 25
    assert Enum.all?(evaluation.items, &(&1.observed == :not_interpreted))
    assert Enum.all?(evaluation.items, &(&1.lexical_choices == 2))
    assert Enum.all?(evaluation.items, &(&1.reason_codes == [:weak_decision_rate_high]))
    assert Enum.all?(evaluation.items, &(&1.excerpt |> String.length() <= 600))
    assert Enum.all?(evaluation.items, &(&1.fold in [:development, :heldout]))

    for _ <- 1..25 do
      assert_receive {:evaluated, _, opts}
      assert opts[:emit_telemetry?] == false
      assert opts[:brain_observation?] == false
    end
    refute_receive {:evaluated, _, _}, 20
  end

  test "evaluation caps at 50 and samples no more than three sentences per source" do
    comments =
      for i <- 1..30 do
        %{author: "agent_#{i}",
          body: Enum.map_join(1..6, " ", fn n ->
            "Agent #{i} contributes sentence #{n}."
          end)}
      end

    assert {:ok, evaluation} =
             SocialEvaluation.analyze(%{
               topic: topic("First topic sentence. Second topic sentence."),
               comments: comments
             }, probe: &probe/2)

    assert evaluation.sampled == 50
    assert evaluation.omitted_replies == 6
    assert evaluation.omitted_sentences > 0
    assert length(Enum.filter(evaluation.items, &(&1.source_type == :original_post))) == 2
    assert Enum.count(evaluation.items, &(&1.source_type == :reply)) == 48
    assert Enum.max_by(evaluation.items, & &1.source_index).source_index <= 16
  end

  test "duplicates share a stable content fold across different Colony threads" do
    sentence = "The same sentence appears across different discussions."
    other_post = %{topic(sentence) | id: "00000000-0000-4000-8000-000000000092"}

    assert {:ok, first} =
             SocialEvaluation.analyze(%{
               topic: topic(sentence),
               comments: [%{author: "another_agent", body: sentence}]
             }, probe: &probe/2)

    assert first.sampled == 1
    assert first.duplicates == 1

    assert {:ok, second} =
             SocialEvaluation.analyze(%{topic: other_post, comments: []},
               probe: &probe/2)

    assert hd(first.items).id == hd(second.items).id
    assert hd(first.items).fold == hd(second.items).fold
    assert String.length(hd(first.items).id) == 24
  end

  test "manual expected outcome, family and review are independent allowlisted labels" do
    assert {:ok, evaluation} =
             SocialEvaluation.analyze(%{topic: topic(), comments: []},
               probe: &probe/2)
    item = hd(evaluation.items)
    labels = %{}
    assert SocialEvaluation.selected?(labels, item, :expected, "unlabeled")

    updated =
      SocialEvaluation.put_label(labels, item, %{
        "expected" => "interpret", "family" => "statement", "review" => "incorrect"
      })
    assert updated[item.id] == %{
      expected: :interpret, family: :statement, review: :incorrect
    }
    assert SocialEvaluation.selected?(updated, item, :expected, "interpret")

    rejected =
      SocialEvaluation.put_label(updated, item, %{
        "expected" => "PRIVATE INJECTION",
        "family" => "unknown_fake_family",
        "review" => "approved_external_claim",
        "notes" => "PRIVATE"
      })
    assert rejected == updated

    stats = SocialEvaluation.summary(evaluation, updated)
    fold = Map.fetch!(stats, item.fold)
    assert fold.labeled == 1
    assert fold.coverage_gaps == 1
    assert fold.coverage_candidates == 0
    assert fold.human_incorrect == 1

    [json] = String.split(SocialEvaluation.export_jsonl(evaluation, updated), "\n")
    exported = Jason.decode!(json)
    assert exported["schema"] == "symbrella_social_eval_v1"
    assert exported["expected"] == "interpret"
    assert exported["family"] == "statement"
    assert exported["human_review"] == "incorrect"
    assert exported["observed"] == "not_interpreted"
    assert exported["trust"] == "untrusted_external"
    assert exported["post_id"] == @post_id
    assert exported["excerpt"] == item.excerpt
    refute json =~ "PRIVATE"
  end

  test "unlabeled and unavailable cases cannot inflate automatic accuracy" do
    incomplete_probe = fn _, _ ->
      {:ok, %{
        stage: :social_semantic_shadow,
        sentence_meaning: %{status: :structurally_interpreted}
      }}
    end
    assert {:ok, eval} =
             SocialEvaluation.analyze(%{topic: topic(), comments: []},
               probe: incomplete_probe)
    item = hd(eval.items)
    assert item.observed == :infrastructure_failure

    initial = SocialEvaluation.summary(eval, %{})
    stats = Map.fetch!(initial, item.fold)
    assert stats.samples == 1
    assert stats.labeled == 0
    assert stats.coverage_candidates == 0
    assert stats.expected_abstentions == 0
    assert stats.infrastructure_failures == 1
    assert stats.human_correct == 0

    labels = SocialEvaluation.put_label(%{}, item, %{
      "expected" => "abstain", "review" => "unreviewed"
    })
    revised = Map.fetch!(SocialEvaluation.summary(eval, labels), item.fold)
    assert revised.labeled == 1
    assert revised.expected_abstentions == 0
    assert revised.infrastructure_failures == 1
  end

  test "the harness never trusts an unrelated reporter or invalid input" do
    incompatible = fn _, _ ->
      {:ok, %{source: :untrusted_fake, mode: :read_only_shadow, segments: []}}
    end
    assert {:error, :evaluation_unavailable} =
             SocialEvaluation.analyze(%{topic: topic(), comments: []},
               reporter: incompatible)

    assert {:error, :invalid_evaluation_input} =
             SocialEvaluation.analyze(%{topic: %{}, comments: :wrong})
  end
end
