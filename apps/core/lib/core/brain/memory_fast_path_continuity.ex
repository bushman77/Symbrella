defmodule Core.Brain.MemoryFastPathContinuity do
  @moduledoc """
  Run comprehension for a user turn already answered by Hippocampal memory.

  A memory hit owns the *reply*. The additional pass exists only to advance
  current-turn STM/WM and evidence-gated neural feedback using the original
  user utterance. It MUST NOT synthesize a second answer, persist another
  episode, train Stage1, mutate self-state, or execute an agency command.

  This is intentionally synchronous and uses Core's real pipeline, so the
  resulting SI and trace can be inspected by HomeLive's Explain view.
  """

  @spec options(String.t()) :: keyword()
  def options(session_id) when is_binary(session_id) do
    [
      mode: :prod,
      session_id: session_id,
      response: :off,
      episodes: false,
      persist_episodes: false,
      hippocampus_encode?: false,
      self_model: :off,
      comprehension_wm?: true,
      focus_prompt_topics?: false,
      prefrontal_control?: false,
      execute_agency_commands?: false,
      record_agency_commands?: false,
      neuron_activation?: true,
      notify_brain?: true,
      semantic_propagation?: true,
      lifg_opts: [
        grammar_weight: 1.0,
        cerebellum_learn?: false,
        cerebellum_snapshot_path: nil
      ]
    ]
  end

  @spec run(String.t(), String.t()) :: {:ok, map()} | {:error, atom()}
  def run(user_text, session_id)
      when is_binary(user_text) and is_binary(session_id) and
             byte_size(session_id) > 0 and byte_size(session_id) <= 128 do
    case Core.resolve_input(user_text, options(session_id)) do
      %Core.SemanticInput{} = si ->
        {:ok, Map.put(si, :text, user_text)}

      _ ->
        {:error, :invalid_semantic_input}
    end
  rescue
    _ -> {:error, :comprehension_failed}
  catch
    :exit, _ -> {:error, :comprehension_unavailable}
  end

  def run(_, _), do: {:error, :invalid_input}
end
