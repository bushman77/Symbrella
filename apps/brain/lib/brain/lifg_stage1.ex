defmodule Brain.LIFGStage1 do
  @moduledoc """
  Orchestrates Stage-1 disambiguation:
  - converts SI to LIFG candidates
  - calls Brain.LIFG
  - applies control signals to running Brain.Cell processes
  - returns {:ok, lifg_result}
  """

  alias Brain.LIFG

  @type si :: map()

  @spec run(si, [number()], keyword()) ::
          {:ok,
           %{
             choices: [map()],
             boosts: [{binary(), number()}],
             inhibitions: [{binary(), number()}],
             audit: map()
           }}
  def run(si, context_vec, opts \\ []) when is_list(context_vec) do
    candidates = si_to_candidates!(si)

    {:ok, result} =
      LIFG.disambiguate_stage1(
        candidates,
        context_vec,
        Keyword.merge([scores: :top2], opts)
      )

    apply_control_signals(result.boosts, result.inhibitions, opts)

    {:ok, result}
  end

  # ----- SI → LIFG candidates -----
  # Adapt these clauses to your actual SI shape. The idea:
  # - produce a flat list of maps, each with:
  #   id, token_index, lemma, pos, embedding | embedding_id, lex_fit, rel_prior, intent_bias, activation
  defp si_to_candidates!(%{candidates_by_token: groups}) when is_map(groups) do
    groups
    |> Enum.flat_map(fn {tidx, senses} ->
      Enum.map(senses, fn s ->
        %{
          id: s.id || s[:sense_id] || raise("candidate missing id"),
          token_index: tidx,
          lemma: s.lemma || s[:word] || "",
          pos: s.pos || "x",
          embedding: s[:embedding],
          embedding_id: s[:embedding_id],
          lex_fit: s[:lex_fit] || 0.0,
          rel_prior: s[:rel_prior] || 0.0,
          intent_bias: s[:intent_bias] || 0.0,
          activation: s[:activation] || 0.0
        }
      end)
    end)
  end

  # If SI already holds a flat list:
  defp si_to_candidates!(%{candidates: list}) when is_list(list), do: list

  # If you’re feeding in the candidates directly:
  defp si_to_candidates!(list) when is_list(list), do: list

  defp si_to_candidates!(other),
    do: raise(ArgumentError, "Cannot extract LIFG candidates from: #{inspect(other)}")

  # ----- signal application -----

  # opts:
  #   :signal_concurrency - integer, default System.schedulers_online()
  #   :coalesce - if true, sums multiple deltas per id before casting (default true)
  @spec apply_control_signals([{binary(), number()}], [{binary(), number()}], keyword()) :: :ok
  def apply_control_signals(boosts, inhibitions, opts \\ []) do
    coalesce? = Keyword.get(opts, :coalesce, true)
    conc = Keyword.get(opts, :signal_concurrency, System.schedulers_online())

    signals =
      boosts
      |> Enum.concat(inhibitions)
      |> then(fn pairs -> if coalesce?, do: coalesce(pairs), else: pairs end)

    # Fan out as casts to cell processes
    signals
    |> Task.async_stream(&send_delta/1, max_concurrency: conc, timeout: :infinity)
    |> Stream.run()

    :ok
  end

  defp coalesce(pairs) do
    pairs
    |> Enum.group_by(fn {id, _} -> id end, fn {_, d} -> d end)
    |> Enum.map(fn {id, deltas} -> {id, Enum.sum(deltas)} end)
  end

  defp send_delta({id, delta}) do
    # Brain.Cell registers under Brain.via(id)
    GenServer.cast(Brain.via(id), {:activate, %{delta: delta}})
  end
end
