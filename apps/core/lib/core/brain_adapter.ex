defmodule Core.BrainAdapter do
  @moduledoc """
  Core-side facade for calls into Brain.
  """

  alias Core.Brain.Runtime

  @type pos :: String.t() | atom() | nil
  @type key :: String.t() | {String.t(), pos()} | {:mwe, [String.t()]}

  @default_limit 12

  @spec synonyms_for_keys([key()], map() | keyword()) ::
          {:ok, %{optional(key()) => [String.t()]}} | {:error, term()}
  def synonyms_for_keys(keys, opts \\ %{})

  def synonyms_for_keys(keys, opts) when is_list(keys) and (is_map(opts) or is_list(opts)) do
    limit = positive_int(get_opt(opts, :limit, @default_limit), @default_limit)
    pos_filter = get_opt(opts, :pos_filter, nil)

    if Code.ensure_loaded?(Brain.Recall.Synonyms) do
      result =
        keys
        |> Enum.map(fn key -> {key, lookup_synonyms(key, pos_filter, limit)} end)
        |> Map.new()

      {:ok, result}
    else
      {:error, :brain_synonyms_unavailable}
    end
  end

  def synonyms_for_keys(_keys, _opts), do: {:error, :invalid_args}

  defp lookup_synonyms({word, pos}, _pos_filter, limit) when is_binary(word) do
    lookup_by_pos(word, pos, limit)
  end

  defp lookup_synonyms({:mwe, tokens}, pos_filter, limit) when is_list(tokens) do
    tokens
    |> Enum.map(&to_string/1)
    |> Enum.join(" ")
    |> lookup_by_pos(pos_filter, limit)
  end

  defp lookup_synonyms(word, nil, limit) when is_binary(word) do
    Runtime.apply_if_exported(Brain.Recall.Synonyms, :lookup, [word, limit], [])
  end

  defp lookup_synonyms(word, pos_filter, limit) when is_binary(word) do
    lookup_by_pos(word, pos_filter, limit)
  end

  defp lookup_synonyms(_key, _pos_filter, _limit), do: []

  defp lookup_by_pos(word, nil, limit) do
    Runtime.apply_if_exported(Brain.Recall.Synonyms, :lookup, [word, limit], [])
  end

  defp lookup_by_pos(word, pos, limit) do
    Runtime.apply_if_exported(Brain.Recall.Synonyms, :lookup_by_pos, [word, pos, limit], [])
  end

  defp get_opt(opts, key, default) when is_map(opts), do: Map.get(opts, key, default)
  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)

  defp positive_int(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_int(_value, default), do: default
end
