defmodule Core.StateDumpTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.StateDump

  setup do
    dir =
      Path.join(
        System.tmp_dir!(),
        "symbrella_state_dump_test_#{System.unique_integer([:positive])}"
      )

    path = Path.join(dir, "raw.log")
    on_exit(fn -> File.rm_rf(dir) end)
    %{path: path}
  end

  test "off by default writes nothing and leaves the state unchanged", %{path: path} do
    state = %{sentence: "hello", nested: %{raw: [:a, :b]}}
    assert StateDump.capture(state, :final, config: [mode: :off, path: path]) == state
    refute File.exists?(path)
  end

  test "final mode appends untruncated nested SemanticInput and replies", %{path: path} do
    long_value = String.duplicate("complete_nested_state_", 500)

    state = %SemanticInput{
      sentence: "Good morning Symbrella",
      source: :prod,
      tokens: [%{index: 0, phrase: "good"}, %{index: 1, phrase: "morning"}],
      sentence_signatures: [
        %{id: :salutation, span: {0, 1}, roles: %{greeting: 0}, evidence: [:lexical]}
      ],
      semantic_choices: [
        %{id: "good morning|noun|1", definition: long_value, score: 0.143554}
      ],
      trace: [%{stage: :test, meta: %{deep: %{full: long_value}}}],
      response_meta: %{reflection: %{status: :accept, issues: [:too_generic]}}
    }

    config = [mode: :final, path: path]

    assert StateDump.capture(state, :stm, config: config) == state
    refute File.exists?(path)

    assert StateDump.capture(state, :pipeline_raw, config: config) == state
    reply = %{text: "Hello!", si: state, metadata: %{marker: :complete_reply}}
    assert StateDump.capture(reply, :chat_reply, config: config) == reply

    {:ok, raw} = File.read(path)
    assert raw =~ "stage: pipeline_raw"
    assert raw =~ "stage: chat_reply"
    assert raw =~ "__struct__: Core.SemanticInput"
    assert raw =~ "complete_reply"
    assert raw =~ "good morning|noun|1"
    assert raw =~ long_value
    assert length(String.split(raw, "SYMBRELLA RAW STATE BEGIN")) == 3
    assert Bitwise.band(File.stat!(path).mode, 0o777) == 0o600
  end

  test "all mode logs intermediate stages as independent complete snapshots", %{path: path} do
    config = [mode: :all, path: path]
    original = %{trace: [:initial], nested: %{value: 10}}
    changed = %{original | trace: [:initial, :lifg]}

    assert StateDump.capture(original, :initial, config: config) == original
    assert StateDump.capture(changed, :lifg, config: config) == changed

    raw = File.read!(path)
    assert raw =~ "stage: initial"
    assert raw =~ "stage: lifg"
    assert raw =~ "[:initial, :lifg]"
    assert raw =~ "nested: %{value: 10}"
  end
end
