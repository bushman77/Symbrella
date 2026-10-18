defmodule Brain.SemanticPulse do
  @moduledoc """
  Bounded, session-scoped, ephemeral *lemma-level* routing evidence.

  Entries are NOT BrainCells, dictionary senses, synapses or verified meanings.
  An attested inflected action (e.g. draws -> draw) may briefly prime existing
  verb candidates for the lemma on a later turn, without selecting a base sense.

  Storage lives only in Brain's process state: no DB, no ETS, no extra processes.
  Half-life matches Brain.Cell and stale/session-overflow entries are evicted.
  """

  alias Brain.Cell.Activation

  @max_entries 128
  @max_per_session 8
  @max_age_ms 30_000
  @max_signal 0.03

  @type key :: {String.t(), String.t()}
  @type entries :: %{optional(key()) => %{value: float(), at_ms: integer(), source_id: String.t()}}

  @spec put(entries(), term(), term(), term(), number(), integer()) :: entries()
  def put(entries, session, lemma, source_id, delta, now_ms)
      when is_map(entries) and is_integer(now_ms) do
    retained = prune(entries, now_ms)

    if valid_session?(session) and valid_lemma?(lemma) and
         attested_verb_id?(source_id) and source_surface(source_id) != lemma and
         is_number(delta) and delta > 0 do
      key = {session, lemma}
      before = case Map.get(retained, key) do
        %{value: value} when is_number(value) -> value
        _ -> 0.0
      end

      updated = Map.put(retained, key, %{
        value: min(before + min(delta * 1.0, @max_signal), @max_signal),
        at_ms: now_ms,
        source_id: source_id
      })

      updated
      |> trim_session(session)
      |> trim_global()
    else
      retained
    end
  end

  def put(entries, _, _, _, _, _) when is_map(entries), do: entries

  @doc "Read one conversation's decayed lemma priors; also return pruned storage."
  @spec snapshot(entries(), term(), integer()) :: {map(), entries()}
  def snapshot(entries, session, now_ms) when is_map(entries) and is_integer(now_ms) do
    alive = prune(entries, now_ms)

    signals =
      if valid_session?(session) do
        alive
        |> Enum.flat_map(fn
          {{^session, lemma}, %{value: signal}} -> [{lemma, signal}]
          _ -> []
        end)
        |> Map.new()
      else
        %{}
      end

    {signals, alive}
  end

  def snapshot(entries, _, _) when is_map(entries), do: {%{}, entries}

  @doc "Prune after absolute TTL as well as decaying below activation threshold."
  @spec prune(entries(), integer()) :: entries()
  def prune(entries, now_ms) when is_map(entries) and is_integer(now_ms) do
    Enum.reduce(entries, %{}, fn
      {{session, lemma} = key, %{value: value, at_ms: at_ms, source_id: id}}, acc
      when is_integer(at_ms) and is_number(value) ->
        age = max(0, now_ms - at_ms)
        decayed = if age > @max_age_ms, do: 0.0,
          else: Activation.decay(value, at_ms, now_ms)

        if valid_session?(session) and valid_lemma?(lemma) and
             attested_verb_id?(id) and decayed > 0 do
          Map.put(acc, key, %{value: decayed, at_ms: now_ms, source_id: id})
        else
          acc
        end

      _, acc -> acc
    end)
  end

  defp trim_session(entries, session) do
    own =
      entries
      |> Enum.filter(fn {{id, _}, _} -> id == session end)
      |> Enum.sort_by(fn {key, item} -> {-item.value, key} end)

    drops = Enum.drop(own, @max_per_session) |> Enum.map(&elem(&1, 0))
    Map.drop(entries, drops)
  end

  defp trim_global(entries) do
    if map_size(entries) <= @max_entries do
      entries
    else
      keep =
        entries
        |> Enum.sort_by(fn {key, item} -> {-item.value, key} end)
        |> Enum.take(@max_entries)
      Map.new(keep)
    end
  end

  defp valid_session?(id), do: is_binary(id) and byte_size(id) > 0 and byte_size(id) <= 128

  defp valid_lemma?(lemma) when is_binary(lemma),
    do: byte_size(lemma) <= 48 and Regex.match?(~r/\A[a-z][a-z0-9_]*\z/, lemma)
  defp valid_lemma?(_), do: false

  defp attested_verb_id?(id) when is_binary(id) do
    case String.split(id, "|") do
      [surface, "verb", sense] ->
        surface != "" and sense != "" and sense != "fallback"
      _ -> false
    end
  end

  defp attested_verb_id?(_), do: false

  defp source_surface(id), do: id |> String.split("|", parts: 2) |> hd()
end
