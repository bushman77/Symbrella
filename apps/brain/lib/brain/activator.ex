defmodule Brain.Activator do
  @moduledoc """
  Core.SemanticInput tokens → start/fire **ALL** DB senses per word.
  Runtime fallback is created only when a word has no senses.
  MWE fallback is configurable (default: :skip).
  """

  alias Core.SemanticInput, as: SI
  alias Brain.Cell
  alias Db
  alias Db.BrainCell, as: Schema

  @type opts :: [
          impulse: float(),
          mwe_fallback: :skip | :runtime
        ]

  @spec run(SI.t(), opts()) :: SI.t()
  def run(%SI{} = si, opts \\ []) do
    impulse      = opts[:impulse] || 0.1
    mwe_fallback = opts[:mwe_fallback] || :skip
    now_ms       = System.monotonic_time(:millisecond)

    {words, mwes} = partition_tokens(si.tokens || [])
    lookups       = words ++ mwes

    # one batched fetch for everything
    senses_by = Db.senses_for_words_full(lookups)

    # unigrams: use senses if present, else one runtime fallback
    rows_words =
      Enum.flat_map(words, fn w ->
        case Map.get(senses_by, w, []) do
          []       -> [%{id: "w:" <> w, word: w, pos: "unk", type: "runtime"}]
          schemas  -> schemas
        end
      end)

    # MWEs: usually no lexicon entries; fallback optional
    rows_mwes =
      Enum.flat_map(mwes, fn p ->
        case Map.get(senses_by, p, []) do
          [] ->
            case mwe_fallback do
              :runtime -> [%{id: "w:" <> p, word: p, pos: "unk", type: "runtime"}]
              :skip    -> []
            end

          schemas -> schemas
        end
      end)

    rows = (rows_words ++ rows_mwes) |> Enum.uniq_by(&id_of/1)

    # ensure/hydrate + fire
    active_ids =
      for attrs <- rows do
        pid = ensure_cell(attrs)
        Cell.fire(pid, impulse)
        id_of(attrs)
      end

    # reflect into SI
    refs =
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

    %{si | active_cells: refs}
  end

  # ——— helpers ———

  defp ensure_cell(%Schema{id: id} = schema) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] -> Brain.Cell.hydrate(pid, schema); pid
      [] ->
        case DynamicSupervisor.start_child(Brain.CellSup, {Brain.Cell, schema}) do
          {:ok, pid} -> pid
          {:error, {:already_started, p}} -> p
          {:error, reason} -> raise "Cell start failed for #{id}: #{inspect(reason)}"
        end
    end
  end

  defp ensure_cell(%{id: id} = attrs) when is_binary(id) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] -> pid
      [] ->
        case DynamicSupervisor.start_child(Brain.CellSup, {Brain.Cell, attrs}) do
          {:ok, pid} -> pid
          {:error, {:already_started, p}} -> p
          {:error, reason} -> raise "Cell start failed for #{id}: #{inspect(reason)}"
        end
    end
  end

  defp id_of(%Schema{id: id}), do: id
  defp id_of(%{id: id}),       do: id

  defp partition_tokens(tokens) do
    Enum.reduce(tokens, {[], []}, fn
      %{phrase: p, n: 1}, {ws, ms} when is_binary(p) -> {[String.downcase(p) | ws], ms}
      %{phrase: p, n: n}, {ws, ms} when is_binary(p) and is_integer(n) and n > 1 ->
        {ws, [String.downcase(p) | ms]}
      _, acc -> acc
    end)
  end
end

