defmodule Core.Response.MemoryRecallTelemetryTest do
  use ExUnit.Case, async: false

  alias Core.Response

  test "symbolic identity recall emits provenance without names or chat text" do
    id = "memory-recall-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:core, :response, :memory_recall],
        fn _event, measurements, metadata, pid ->
          send(pid, {:memory_recall, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    {_tone, text, meta} = Response.memory_reply(%{text: "what is your name?"})

    assert text == "My name is Symbrella."
    assert meta.retrieval_found? == true
    assert meta.neural_ranker_used? == false

    assert_receive {:memory_recall, %{count: 1, hit: 1}, recorded}, 1000
    assert recorded.identity_subject == :assistant
    assert recorded.memory_source == :configuration
    assert recorded.neural_ranker_used? == false
    refute Map.has_key?(recorded, :value)
    refute Map.has_key?(recorded, :name)
    refute Map.has_key?(recorded, :text)
  end
end
