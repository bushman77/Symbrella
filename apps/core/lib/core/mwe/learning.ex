defmodule Core.MWE.Learning do
  @moduledoc """
  Bounded, in-memory contextual evidence for unverified MWE hypotheses.

  One input turn is one observation per phrase/category, even if the phrase
  occurs twice in that turn. A context is the immediately preceding and
  following word. Candidates reaching the conservative thresholds become
  :candidate_for_review, never verified dictionary senses.

  This observer NEVER writes Db.BrainCell, and its counters reset when the
  Core application stops. Persistent storage and lexical promotion require
  separate, explicit mechanisms.
  """

  use GenServer

  @default_keep 512
  @context_limit 24

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    gen_opts = if is_nil(name), do: [], else: [name: name]
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  @impl true
  def init(opts) do
    keep =
      case Keyword.get(opts, :keep, @default_keep) do
        n when is_integer(n) and n > 0 -> min(n, 4096)
        _ -> @default_keep
      end

    {:ok, %{records: %{}, order: [], keep: keep}}
  end

  @doc """
  Attach accumulated contextual evidence to the current SI, without changing
  its words, lexical senses or candidate IDs.

  Pass :mwe_learning_server for an isolated GenServer in tests.
  Pass :mwe_learning => :off to leave the SI completely unchanged.
  """
  @spec observe(map(), keyword()) :: map()
  def observe(si, opts \\ [])

  def observe(%{mwe_hypotheses: hypotheses} = si, opts)
      when is_list(hypotheses) and is_list(opts) do
    if Keyword.get(opts, :mwe_learning, :on) in [:off, false] or hypotheses == [] do
      si
    else
      server = Keyword.get(opts, :mwe_learning_server, __MODULE__)

      if server_alive?(server) do
        try do
          {enriched, reviewed} = GenServer.call(server, {:observe, si}, 2_000)

          event = %{
            stage: :mwe_learning,
            decision: :observed,
            meta: %{
              hypotheses: length(enriched),
              review_candidates: reviewed
            },
            ts_ms: System.system_time(:millisecond)
          }

          si
          |> Map.put(:mwe_hypotheses, enriched)
          |> Map.update(:trace, [event], fn trace -> [event | List.wrap(trace)] end)
        catch
          :exit, _ -> si
        end
      else
        si
      end
    end
  end

  def observe(other, _opts), do: other

  @doc "Inspect non-sensitive aggregate counts; no source sentences retained."
  @spec snapshot(GenServer.server()) :: map()
  def snapshot(server \\ __MODULE__), do: GenServer.call(server, :snapshot)

  @doc "Clear contextual learning evidence (also useful for deterministic tests)."
  @spec reset(GenServer.server()) :: :ok
  def reset(server \\ __MODULE__), do: GenServer.call(server, :reset)

  @impl true
  def handle_call(:reset, _from, state) do
    {:reply, :ok, %{state | records: %{}, order: []}}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    aggregates =
      Enum.into(state.records, %{}, fn {key, record} ->
        {key,
         %{
           observation_count: record.observation_count,
           distinct_contexts: MapSet.size(record.contexts),
           review_status: review_status(elem(key, 1), record)
         }}
      end)

    {:reply, %{phrase_count: map_size(aggregates), records: aggregates}, state}
  end

  @impl true
  def handle_call({:observe, si}, _from, state) do
    words = indexed_words(Map.get(si, :tokens, []))
    hypotheses = List.wrap(Map.get(si, :mwe_hypotheses))
    verified = verified_phrases(Map.get(si, :mwe_lexicon, []))

    # Every distinct phrase/category gets at most one observation in this
    # turn. Multiple occurrences may contribute distinct contexts.
    grouped =
      hypotheses
      |> Enum.filter(&candidate?/1)
      |> Enum.reject(&(MapSet.member?(verified, &1.phrase)))
      |> Enum.group_by(&{&1.phrase, &1.category})

    next_state =
      Enum.reduce(grouped, state, fn {key, candidates}, acc ->
        contexts =
          candidates
          |> Enum.map(&context_for(&1, words))
          |> MapSet.new()

        update_record(acc, key, contexts)
      end)

    enriched =
      Enum.map(hypotheses, fn
        %{phrase: phrase, category: category} = item ->
          case Map.get(next_state.records, {phrase, category}) do
            nil ->
              item

            record ->
              Map.merge(item, %{
                observation_count: record.observation_count,
                distinct_contexts: MapSet.size(record.contexts),
                review_status: review_status(category, record)
              })
          end

        other ->
          other
      end)

    ready =
      Enum.count(enriched, fn
        %{review_status: :candidate_for_review} -> true
        _ -> false
      end)

    {:reply, {enriched, ready}, next_state}
  end

  defp update_record(state, key, contexts) do
    {records, order} =
      if Map.has_key?(state.records, key) do
        {state.records, state.order}
      else
        case length(state.order) >= state.keep do
          true ->
            [oldest | rest] = state.order
            {Map.delete(state.records, oldest), rest ++ [key]}

          false ->
            {state.records, state.order ++ [key]}
        end
      end

    prior = Map.get(records, key, %{observation_count: 0, contexts: MapSet.new()})

    observed_contexts =
      contexts
      |> Enum.reduce(prior.contexts, fn context, accumulated ->
        if MapSet.size(accumulated) < @context_limit do
          MapSet.put(accumulated, context)
        else
          accumulated
        end
      end)

    updated = %{
      observation_count: prior.observation_count + 1,
      contexts: observed_contexts
    }

    %{state | records: Map.put(records, key, updated), order: order}
  end

  defp candidate?(%{phrase: phrase, category: category, status: :unverified, verified?: false})
       when is_binary(phrase) and is_atom(category) and byte_size(phrase) > 0 do
    true
  end

  defp candidate?(_), do: false

  defp verified_phrases(units) do
    units
    |> List.wrap()
    |> Enum.filter(fn
      %{phrase: phrase, senses: [_ | _]} when is_binary(phrase) -> true
      _ -> false
    end)
    |> Enum.map(& &1.phrase)
    |> MapSet.new()
  end

  defp indexed_words(tokens) do
    tokens
    |> List.wrap()
    |> Enum.filter(fn token -> field(token, :n) in [nil, 1] end)
    |> Enum.with_index()
    |> Enum.into(%{}, fn {token, fallback_idx} ->
      idx = field(token, :index)
      index = if is_integer(idx), do: idx, else: fallback_idx
      {index, normalize(field(token, :phrase))}
    end)
  end

  defp context_for(%{span: {first, last}}, words)
       when is_integer(first) and is_integer(last) do
    {Map.get(words, first - 1, :begin), Map.get(words, last + 1, :end)}
  end

  defp context_for(_, _), do: {:unknown, :unknown}

  defp review_status(category, record) do
    required = if category == :compound_noun, do: 5, else: 3

    if record.observation_count >= required and MapSet.size(record.contexts) >= 2 do
      :candidate_for_review
    else
      :collecting
    end
  end

  defp server_alive?(server) when is_pid(server), do: Process.alive?(server)
  defp server_alive?(server) when is_atom(server), do: is_pid(Process.whereis(server))
  defp server_alive?(_), do: false

  defp field(%{} = map, key), do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
  defp field(_, _), do: nil

  defp normalize(text) when is_binary(text) do
    text
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize(_), do: ""
end
