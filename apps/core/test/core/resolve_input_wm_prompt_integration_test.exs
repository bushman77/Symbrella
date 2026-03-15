defmodule Core.ResolveInputWMPromptIntegrationTest do
  use ExUnit.Case, async: false

  alias Core.Response.LlmPrompt

  defp clear_wm! do
    case Process.whereis(Brain) do
      pid when is_pid(pid) ->
        %{wm: wm} = Brain.snapshot_wm()

        Enum.each(wm, fn item ->
          _ = Brain.defocus(item.id)
        end)

      _ ->
        :ok
    end

    :ok
  end

  defp seed_filler_wm! do
    _ =
      Brain.focus(
        [
          %{id: "tell|verb|0", lemma: "tell", score: 1.0, source: :lifg},
          %{id: "have|verb|0", lemma: "have", score: 1.0, source: :lifg},
          %{id: "you|pronoun|0", lemma: "you", score: 1.0, source: :lifg}
        ],
        []
      )

    :ok
  end

  setup do
    clear_wm!()

    on_exit(fn ->
      clear_wm!()
    end)

    :ok
  end

  test "working-memory self query surfaces working and memory in prompt-facing active concepts" do
    seed_filler_wm!()

    _si =
      Core.resolve_input(
        "tell me about your working memory i have built you",
        max_wordgram_n: 3
      )

    %{wm: wm} = Brain.snapshot_wm()
    summary = LlmPrompt.summarize_wm(wm)

    prompt =
      LlmPrompt.build_system_prompt(
        %{intent: :command},
        %{tone: :warm, mode: :pair_programmer},
        %{tone_hint: :neutral},
        wm
      )

    assert "working memory" in summary or
             ("working" in summary and "memory" in summary)

    assert prompt =~ "Active concepts:"

    assert prompt =~ "working memory" or
             (prompt =~ "working" and prompt =~ "memory")
  end
end
