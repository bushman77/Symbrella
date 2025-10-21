defmodule Core.Commit do
  @moduledoc """
  Finalize/Output commit for the Core pipeline.

  • Packs LIFG winners (+ spans, n, alt_ids) into a client-friendly map.
  • Summarizes ATL slate and episodic evidence.
  • Surfaces `:acc_conflict` for UI gating/telemetry.
  • Appends a `:commit` trace event without changing Core’s return shape.
  • Telemetry: `[:brain, :core, :commit]` with winner count.
  """

  @type t :: map()

  @spec commit(map(), keyword()) :: %{output: map(), si: map()}
  def commit(si, opts \\ []) do
    include_trace?  = Keyword.get(opts, :include_trace, false)
    include_tokens? = Keyword.get(opts, :include_tokens, false)
    now_ms = System.system_time(:millisecond)

    lifg_choices = Map.get(si, :lifg_choices, [])
    tokens       = Map.get(si, :tokens, [])

    winners =
      lifg_choices
      |> Enum.map(fn ch ->
        idx  = Map.get(ch, :token_index, 0)
        tok  = Enum.at(tokens, idx, %{})
        span = Map.get(tok, :span, nil)
        n    = Map.get(tok, :n, 1)

        %{
          token_index: idx,
          id:        Map.get(ch, :id),
          lemma:     Map.get(ch, :lemma),
          score:     Map.get(ch, :score, 0.0),
          alt_ids:   Map.get(ch, :alt_ids, []),
          n:         n,
          span:      span
        }
      end)

    # Compact episodes summary (stable for UI/logs)
    episodes =
      case get_in(si, [:evidence, :episodes]) do
        list when is_list(list) ->
          Enum.map(list, fn rec ->
            at    = Map.get(rec, :at) || Map.get(rec, "at")
            score = Map.get(rec, :score) || Map.get(rec, "score") || 0.0

            meta =
              case Map.get(rec, :episode) do
                %{} = ep -> Map.get(ep, :meta) || Map.get(ep, "meta")
                _ -> nil
              end

            %{at: at, score: score, meta: meta}
          end)

        _ -> []
      end

    atl        = Map.get(si, :atl_slate, %{})
    atl_sum    = %{
      winner_count: Map.get(atl, :winner_count, 0),
      concepts:     atl |> Map.get(:by_norm, %{}) |> map_size()
    }

    acc_conflict = Map.get(si, :acc_conflict, 0.0)

    base_output = %{
      sentence:      Map.get(si, :sentence),
      winners:       winners,
      episodes:      episodes,
      acc_conflict:  acc_conflict,
      atl:           atl_sum,
      ts_ms:         now_ms
    }

    output =
      (if include_trace?, do: Map.put(base_output, :trace, Map.get(si, :trace, [])), else: base_output)
      |> then(fn m -> if include_tokens?, do: Map.put(m, :tokens, tokens), else: m end)

    commit_event = %{
      stage: :commit,
      ts_ms: now_ms,
      winners: length(winners),
      episodes: length(episodes),
      acc_conflict: acc_conflict
    }

    si2 = Map.update(si, :trace, [commit_event], fn tr -> [commit_event | tr] end)

    emit([:brain, :core, :commit], %{winners: length(winners)}, %{acc_conflict: acc_conflict})

    %{output: output, si: si2}
  end

  # Local telemetry shim
  defp emit(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end
end

