defmodule Core.RuntimeBind do
  @moduledoc """
  Stage: take SemanticInput tokens → start/lookup runtime Brain cells → fire them
  → annotate SemanticInput.active_cells.

  Keeps things sense-agnostic for now (id = "w:<word>"). Later we can switch
  to DB-backed sense ids like "hello|interj|0" once POS disambiguation lands.
  """

  alias Core.SemanticInput, as: SI
  alias Core.BrainAdapter

  @type opts :: [impulse: float(), snapshot_fun: (() -> any())]

  @spec bind(SI.t(), opts()) :: SI.t()
  def bind(%SI{} = si, opts \\ []) do
    impulse = opts[:impulse] || 0.1
    now_ms = System.monotonic_time(:millisecond)

    words =
      (si.tokens || [])
      |> Enum.map(&extract_phrase/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    active_refs =
      Enum.reduce(words, [], fn w, acc ->
        id = "w:" <> w

        attrs = %{
          id: id,
          word: w,
          pos: "unk",
          type: "runtime",
          activation: 0.0,
          dopamine: 0.0,
          serotonin: 0.0,
          modulated_activation: 0.0,
          connections: []
        }

        case BrainAdapter.start_or_lookup_cell(attrs) do
          {:ok, pid} ->
            _ = BrainAdapter.fire(pid, impulse)

            aref = %{
              id: id,
              matched_tokens: [],           # can fill with token refs later
              activation_snapshot: 0.0,     # we can read live status later if needed
              source: :runtime,
              reason: :matched_active_cell,
              score: nil,
              ts_ms: now_ms
            }

            [aref | acc]

          _ ->
            acc
        end
      end)
      |> Enum.reverse()

    si
    |> Map.put(:active_cells, active_refs)
    |> put_trace(:runtime_bind, %{count: length(active_refs)})
  end

  # ——— helpers ———

  defp extract_phrase(%{phrase: p}) when is_binary(p), do: String.downcase(p)
  defp extract_phrase(p) when is_binary(p), do: String.downcase(p)
  defp extract_phrase(_), do: nil

  defp put_trace(%SI{trace: tr} = si, stage, meta) do
    evt = %{stage: stage, ts_ms: System.monotonic_time(:millisecond), meta: Map.new(meta)}
    %{si | trace: [evt | (tr || [])]}
  end
end

