defmodule Core.Curiosity.Enricher do
  @moduledoc """
  Boundary wrapper around optional LLM POS enrichment.
  """

  require Logger

  @compile {:no_warn_undefined, Llm.Pos}

  @spec enrich(String.t(), map()) :: term()
  def enrich(phrase, state) when is_binary(phrase) and is_map(state) do
    if Code.ensure_loaded?(Llm.Pos) and function_exported?(Llm.Pos, :run, 2) do
      try do
        Llm.Pos.run(
          phrase,
          model: Map.get(state, :llm_model),
          keep_alive: "5m",
          timeout: :timer.seconds(20),
          options: %{num_predict: 256, num_ctx: 1024, temperature: 0.0},
          allow_builtin_lexicon: true,
          require_nonempty_syn_ant?: false
        )
      rescue
        e ->
          Logger.warning("Curiosity: Llm.Pos.run/2 crashed: #{inspect(e)}")
          {:error, e}
      catch
        kind, reason ->
          Logger.warning("Curiosity: Llm.Pos.run/2 threw: #{inspect({kind, reason})}")
          {:error, reason}
      end
    else
      :undef
    end
  end
end
