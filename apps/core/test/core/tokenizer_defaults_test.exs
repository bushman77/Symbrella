defmodule Core.TokenizerDefaultsTest do
  use ExUnit.Case, async: true
  alias Core.LIFG.Input

  setup do
    prev = Application.get_env(:core, :tokenizer_defaults)

    on_exit(fn ->
      case prev do
        nil -> Application.delete_env(:core, :tokenizer_defaults)
        _ -> Application.put_env(:core, :tokenizer_defaults, prev)
      end
    end)

    :ok
  end

  test "falls back to base defaults when not configured" do
    Application.delete_env(:core, :tokenizer_defaults)
    assert Input.tokenizer_defaults()[:mode] == :words
    assert Input.tokenizer_defaults()[:emit_chargrams] == false
  end

  test "reads configured values" do
    Application.put_env(:core, :tokenizer_defaults, mode: :words, emit_chargrams: true)
    d = Input.tokenizer_defaults()
    assert d[:mode] == :words
    assert d[:emit_chargrams] == true
  end
end
