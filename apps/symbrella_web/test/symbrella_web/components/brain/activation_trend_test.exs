defmodule SymbrellaWeb.Components.Brain.ActivationTrendTest do
  use SymbrellaWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.ActivationTrend

  test "captures actual runtime activation reports without fabricated decay" do
    samples =
      []
      |> ActivationTrend.capture(%{"bat|noun|1" => 0.6, "fly|verb|1" => 0.2}, 1_000)
      |> ActivationTrend.capture(%{"bat|noun|1" => 0.8, "fly|verb|1" => 0.2}, 1_500)

    assert length(samples) == 2
    assert [%{count: 2, peak: 0.6}, %{count: 2, peak: 0.8}] = samples
    assert_in_delta hd(samples).mean, 0.4, 1.0e-9
    assert_in_delta List.last(samples).mean, 0.5, 1.0e-9
  end

  test "does not invent activity when no BrainCell has reported positive activation" do
    samples = ActivationTrend.capture([], %{"a" => 0.0, "b" => :not_a_number}, 1_000)
    assert [%{count: 0, mean: 0.0, peak: 0.0}] = samples

    html = render_component(&ActivationTrend.panel/1, samples: samples)
    assert html =~ "Neuron activation"
    assert html =~ "No neuron activation reports received yet"
    assert html =~ "neuron half-life:"
    assert html =~ "resting threshold:"
  end

  test "keeps at most one minute and 120 samples even during frequent refresh" do
    samples =
      Enum.reduce(0..150, [], fn i, acc ->
        ActivationTrend.capture(acc, %{"a" => 1.0}, i * 500)
      end)

    assert length(samples) == 120
    assert hd(samples).at_ms >= List.last(samples).at_ms - 60_000
    assert List.last(samples).at_ms == 75_000
  end

  test "renders mean and peak as two distinct live SVG line traces" do
    samples =
      []
      |> ActivationTrend.capture(%{"a" => 0.4, "b" => 0.8}, 1_000)
      |> ActivationTrend.capture(%{"a" => 0.6, "b" => 1.2}, 1_500)

    html = render_component(&ActivationTrend.panel/1, samples: samples)

    assert html =~ "brain-activation-trend"
    assert html =~ "Reporting neurons: 2"
    assert html =~ "Mean: 0.90"
    assert html =~ "Peak: 1.20"
    assert html =~ "polyline"
    assert html =~ "stroke=\"#0ea5e9\""
    assert html =~ "stroke=\"#f59e0b\""
  end

  test "bad snapshot is not interpreted as an empty observation" do
    samples = ActivationTrend.capture([], %{"a" => 1.0}, 100)
    assert ActivationTrend.capture(samples, nil, 200) == samples
  end
end
