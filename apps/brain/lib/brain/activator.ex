defmodule Brain.Activator do
  @moduledoc """
  Core.SemanticInput tokens → start/fire **ALL** DB senses for each word.
  Brain-side only; Core stays Brain-agnostic.
  """

  alias Core.SemanticInput, as: SI
  alias Brain.Cell
  alias Db

  @type opts :: [impulse: float()]

  @spec run(SI.t(), opts()) :: SI.t()
  def run(%SI{} = si, opts \\ []) do
    impulse = opts[:impulse] || 0.1
    now_ms  = System.monotonic_time(:millisecond)

    words =
      (si.tokens || [])
      |> Enum.map(&extract_phrase/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    # 1) Pull ALL senses in one query
    senses_by_word = Db.senses_for_words(words)

    # 2) For words with no DB rows yet, provide a fallback runtime cell
    rows =
      words
      |> Enum.flat_map(fn w ->
        case Map.get(senses_by_word, w, []) do
          []    -> [%{id: "w:" <> w, word: w, pos: "unk", type: "runtime"}]
          senses -> senses
        end
      end)
      |> Enum.uniq_by(& &1.id) # defensive de-dup

    # 3) Ensure + fire every sense cell
    active_ids =
      for attrs <- rows do
        pid = ensure_cell(attrs)
        Cell.fire(pid, impulse)
        attrs.id
      end

    # 4) Reflect into SI.active_cells
    active_refs =
      Enum.map(active_ids, fn id ->
        %{
          id: id,
          matched_tokens: [],
          activation_snapshot: 0.0,
          source: :runtime,
          reason: :matched_active_cell,
          score: nil,
          ts_ms: now_ms
        }
      end)

    %{si | active_cells: active_refs}
  end

  # ——— helpers ———

  defp ensure_cell(%{id: id} = attrs) when is_binary(id) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] ->
        pid

      [] ->
        case DynamicSupervisor.start_child(Brain.CellSup, {Cell, attrs}) do
          {:ok, pid}                      -> pid
          {:error, {:already_started, p}} -> p
          {:error, reason}                -> raise "Cell start failed for #{id}: #{inspect(reason)}"
        end
    end
  end

  defp extract_phrase(%{phrase: p}) when is_binary(p), do: String.downcase(p)
  defp extract_phrase(p) when is_binary(p), do: String.downcase(p)
  defp extract_phrase(_), do: nil
end

