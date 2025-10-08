defmodule Brain.Utils.Safe do
  @moduledoc """
  Safe, struct-aware access helpers.

  - `to_plain/1` : struct -> map (drops `__struct__/__meta__`), map -> map, other -> other
  - `get/3`      : read by atom **or** string key (tries both, struct-safe)
  - `get_in/2`   : nested read that never uses Access on structs
  - `ensure_list/1` : nil -> [], x -> [x] unless already list
  """

  @doc """
  Convert structs to plain maps for safe reading.

  Drops `:__struct__` and `:__meta__` (Ecto) keys to avoid leaking metadata.
  """
  @spec to_plain(term()) :: map() | term()
  def to_plain(%_{} = s) do
    s
    |> Map.from_struct()
    |> Map.drop([:__struct__, :__meta__])
  end

  def to_plain(%{} = m), do: m
  def to_plain(other),   do: other

  @doc """
  Get a value from a map or struct using an atom **or** string key.
  Falls back to the other key type and returns `default` on miss.
  """
  @spec get(map() | struct(), atom() | String.t(), any()) :: any()
  def get(data, key, default \\ nil) do
    m = to_plain(data)

    case key do
      k when is_atom(k) ->
        Map.get(m, k, Map.get(m, Atom.to_string(k), default))

      k when is_binary(k) ->
        case Map.get(m, k) do
          nil ->
            case safe_to_existing_atom(k) do
              {:ok, k2} -> Map.get(m, k2, default)
              :error    -> default
            end

          v -> v
        end
    end
  end

  @doc """
  Struct-safe nested get. Walks the path step-by-step using `to_plain/1` + `get/3`
  at each level, so no `Access` protocol is invoked on structs.
  """
  @spec get_in(map() | struct(), [atom() | String.t()]) :: any()
  def get_in(data, path) when is_list(path) do
    Enum.reduce_while(path, to_plain(data), fn key, acc ->
      next = get(acc, key, :__missing__)
      if next == :__missing__, do: {:halt, nil}, else: {:cont, to_plain(next)}
    end)
  end

  @doc "Normalize to list."
  @spec ensure_list(term()) :: list()
  def ensure_list(nil), do: []
  def ensure_list(list) when is_list(list), do: list
  def ensure_list(x), do: [x]

  # ── priv ─────────────────────────────────────────────────────────────

  defp safe_to_existing_atom(bin) when is_binary(bin) do
    try do
      {:ok, String.to_existing_atom(bin)}
    rescue
      ArgumentError -> :error
    end
  end
end

