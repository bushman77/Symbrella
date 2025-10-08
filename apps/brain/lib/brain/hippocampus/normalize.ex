defmodule Brain.Hippocampus.Normalize do
  @moduledoc """
  Cue/episode normalization helpers: extract norms from slates/tokens/winners,
  build sets, and light string sanitation.
  """

  @spec cue_set(nil | [any()] | map()) :: MapSet.t()
  def cue_set(nil), do: MapSet.new()
  def cue_set(%{winners: winners}) when is_list(winners), do: cue_set(winners)
  def cue_set(%{"winners" => winners}) when is_list(winners), do: cue_set(winners)
  def cue_set(%{tokens: tokens}) when is_list(tokens), do: cue_set(tokens)
  def cue_set(%{"tokens" => tokens}) when is_list(tokens), do: cue_set(tokens)

  def cue_set(list) when is_list(list) do
    list
    |> Enum.map(fn
      %{} = m ->
        val =
          Map.get(m, :norm)  || Map.get(m, "norm")  ||
          Map.get(m, :lemma) || Map.get(m, "lemma") ||
          Map.get(m, :word)  || Map.get(m, "word")  ||
          parse_id_word(Map.get(m, :id) || Map.get(m, "id"))

        norm_str(val)

      s when is_binary(s) ->
        if String.contains?(s, "|"), do: norm_str(parse_id_word(s)), else: norm_str(s)

      _ -> nil
    end)
    |> Enum.reject(&empty?/1)
    |> MapSet.new()
  end

  @spec episode_token_set(%{norms: MapSet.t()} | %{slate: map()}) :: MapSet.t()
  def episode_token_set(%{norms: %MapSet{} = ms}), do: ms
  def episode_token_set(%{slate: slate}),
    do: slate |> extract_norms_from_any() |> Enum.reject(&empty?/1) |> MapSet.new()

  # Accept winners/tokens under atom or string keys
  @spec extract_norms_from_any(map()) :: [binary()]
  def extract_norms_from_any(%{winners: winners}) when is_list(winners),
    do: Enum.map(winners, &winner_norm/1)

  def extract_norms_from_any(%{"winners" => winners}) when is_list(winners),
    do: Enum.map(winners, &winner_norm/1)

  def extract_norms_from_any(%{tokens: tokens}) when is_list(tokens),
    do: Enum.map(tokens, &token_norm/1)

  def extract_norms_from_any(%{"tokens" => tokens}) when is_list(tokens),
    do: Enum.map(tokens, &token_norm/1)

  def extract_norms_from_any(_), do: []

  defp winner_norm(map) when is_map(map) do
    val =
      Map.get(map, :lemma)  || Map.get(map, "lemma") ||
      parse_id_word(Map.get(map, :id) || Map.get(map, "id")) ||
      Map.get(map, :word)   || Map.get(map, "word")

    norm_str(val)
  end

  # Tokens can be ID-only; fall back to parsing from :id
  defp token_norm(map) when is_map(map) do
    val =
      Map.get(map, :norm)   || Map.get(map, "norm")   ||
      Map.get(map, :lemma)  || Map.get(map, "lemma")  ||
      Map.get(map, :word)   || Map.get(map, "word")   ||
      parse_id_word(Map.get(map, :id) || Map.get(map, "id"))

    norm_str(val)
  end

  def norm_str(nil), do: nil
  def norm_str(s) when is_binary(s), do: s |> String.trim() |> String.downcase()

  def parse_id_word(nil), do: nil
  def parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  def empty?(nil), do: true
  def empty?(""),  do: true
  def empty?(_),   do: false
end

