defmodule Core.Tools.PollingEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval.PollingEvidence

  @subject "Donald Trumps approval rating favorability polls trend"

  defp source(title, id, measurement \\ nil) do
    item = %{
      title: title,
      publisher: "Example Poll Wire",
      published_at: "2026-10-24T14:00:00Z",
      url: "https://news.google.com/rss/articles/poll-#{id}"
    }

    if measurement, do: Map.put(item, :poll_measurement, measurement), else: item
  end

  defp measurement(percentage, start_date, end_date, opts \\ []) do
    %{
      metric: Keyword.get(opts, :metric, :approval),
      value_pct: percentage,
      pollster: Keyword.get(opts, :pollster, "Independent Survey House"),
      population: Keyword.get(opts, :population, "US adults"),
      fieldwork_start: start_date,
      fieldwork_end: end_date,
      question: Keyword.get(opts, :question, "Do you approve of the job?"),
      methodology: Keyword.get(opts, :methodology, "Online probability panel"),
      sample_size: Keyword.get(opts, :sample_size, 1_000)
    }
  end

  test "extracts one headline-reported percentage without manufacturing survey details" do
    sources = [
      source("Trump's approval falls to record low of 31% as midterms near", "reuters"),
      source("How popular is Donald Trump?", "silver"),
      source("Approval rating of a different politician is 89%", "unrelated")
    ]

    data = PollingEvidence.from_sources(sources, subject: @subject)
    assert data.kind == :public_polling
    assert data.verified? == false
    assert data.evidence_level == :source_reported
    assert length(data.sources) == 2
    assert [%{metric: :approval, value_pct: 31.0, origin: :headline_reported} = observation] =
             data.observations
    assert observation.population == nil
    assert observation.pollster == nil
    assert observation.fieldwork_start == nil
    assert observation.fieldwork_end == nil
    assert observation.question == nil
    assert observation.methodology == nil
    assert data.comparisons == []

    reply = PollingEvidence.answer(data)
    assert reply =~ "31% approval"
    assert reply =~ "Example Poll Wire"
    assert reply =~ "https://news.google.com/rss/articles/poll-reuters"
    assert reply =~ "population and fieldwork not established"
    assert reply =~ "Headlines alone cannot establish"
    assert reply =~ "do not establish what caused"
    refute reply =~ "89%"
  end

  test "a reported headline number is never sufficient for a measured trend" do
    sources = [
      source("Trump approval at 42% according to survey", "one"),
      source("Trump approval at 35% after a poll", "two")
    ]

    data = PollingEvidence.from_sources(sources, subject: @subject)

    assert length(data.observations) == 2
    assert data.comparisons == []
    refute PollingEvidence.answer(data) =~ "a change of -7 percentage points"
  end

  test "matching documented fieldwork yields a signed percentage-point change" do
    sources = [
      source("Donald Trump approval survey in May", "may",
        measurement(42, "2026-05-01", "2026-05-03")),
      source("Donald Trump approval survey in October", "oct",
        measurement(36, "2026-10-20", "2026-10-22"))
    ]

    data = PollingEvidence.from_sources(sources, subject: @subject)
    assert length(data.observations) == 2

    assert [%{
      delta_percentage_points: -6.0,
      kind: :source_reported_comparison,
      causal_inference?: false
    }] = data.comparisons

    reply = PollingEvidence.answer(data)
    assert reply =~ "42%"
    assert reply =~ "36%"
    assert reply =~ "-6 percentage points"
    assert reply =~ "US adults"
    assert reply =~ "2026-05-01"
    assert reply =~ "2026-10-22"
    assert reply =~ "Online probability panel"
    assert reply =~ "Do you approve of the job?"
    assert reply =~ "https://news.google.com/rss/articles/poll-may"
    assert reply =~ "https://news.google.com/rss/articles/poll-oct"
    assert reply =~ "not independent verification"
    assert reply =~ "do not establish what caused"
  end

  test "different measured populations, questions, or methodologies are not comparable" do
    baseline = source("Donald Trump approval baseline", "base",
      measurement(44, "2026-02-01", "2026-02-03"))

    variations = [
      measurement(31, "2026-10-01", "2026-10-03", population: "US registered voters"),
      measurement(31, "2026-10-01", "2026-10-03", question: "Do you favor the policy?"),
      measurement(31, "2026-10-01", "2026-10-03", methodology: "Telephone random digit"),
      measurement(31, "2026-10-01", "2026-10-03", pollster: "Different Survey House"),
      measurement(31, "2026-10-01", "2026-10-03", metric: :disapproval),
      measurement(31, "2026-10-01", "2026-10-03", sample_size: nil),
      measurement(31, "2026-02-02", "2026-02-04")
    ]

    Enum.each(Enum.with_index(variations), fn {item, i} ->
      latest = source("Donald Trump approval recent", "#{i}", item)
      data = PollingEvidence.from_sources([baseline, latest], subject: @subject)
      assert data.comparisons == [], "invalid comparison variation #{i}"
    end)
  end

  test "rejects impossible numeric, date and sample-size records without inferring them" do
    invalid = [
      measurement(114, "2026-08-01", "2026-08-03"),
      measurement(-4, "2026-08-01", "2026-08-03"),
      measurement(30, "2026-10-04", "2026-10-01"),
      measurement(30, "2026-02-30", "2026-03-01"),
      measurement(30, "2026-08-01", "2026-08-03", sample_size: -2),
      measurement(30, "2026-08-01", "2026-08-03", metric: :unknown)
    ]

    Enum.each(Enum.with_index(invalid), fn {measurement, i} ->
      data =
        PollingEvidence.from_sources(
          [source("Donald Trump approval survey", "invalid-#{i}", measurement)],
          subject: @subject
        )

      assert data.observations == []
      assert data.comparisons == []
    end)
  end

  test "no relevant subject or no valid headline percentage yields no fictional result" do
    data = PollingEvidence.from_sources(
      [source("Another leader approval hits 79%", "other")], subject: @subject
    )

    assert data.sources == []
    assert data.observations == []
    assert PollingEvidence.answer(data) =~ "couldn't find relevant dated polling"

    candidate =
      PollingEvidence.from_sources(
        [source("Trump economy report estimates 35 billion in costs", "econ")],
        subject: @subject
      )

    assert candidate.observations == []
    refute PollingEvidence.answer(candidate) =~ "35%"
  end
end
