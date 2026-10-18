defmodule Brain.Synapses.Shadow do
  @moduledoc """
  Experience-driven *shadow* synaptic plasticity.

  Holds bounded, ephemeral, typed connection weights in this GenServer only.
  NO brain_cells writes, learned LIFG priors, Axon updates or activation spread.

  An admitted observation is a revalidated, attributed semantic experience,
  not proof of external truth or of a particular dictionary sense. A repeated
  identical claim (even in another session) has no extra learning effect.
  Distinct contextual experiences can contribute to the same provisional edge.

  Never stores raw utterances or event participant strings. HMAC evidence keys
  are scoped to this BEAM lifetime; a restart intentionally resets all weights.
  """

  use GenServer

  @delta 0.01
  @max_weight 0.08
  @max_experiences 512
  @max_edges 1024

  @type edge_key :: {String.t(), String.t(), :actor | :patient}

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Process one bounded Core-validated observation synchronously."
  @spec observe(map()) :: {:ok, :recorded | :duplicate} | {:error, atom()}
  def observe(%{evidence: evidence, edges: edges, provenance: :user_reported} = _observation)
      when is_binary(evidence) and is_list(edges) do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, {:observe, evidence, edges}, 5_000)
    else
      {:error, :not_running}
    end
  catch
    :exit, _ -> {:error, :not_running}
  end

  def observe(_), do: {:error, :invalid_observation}

  @doc "Read-only bounded ledger statistics, no original sentences."
  @spec snapshot() :: map()
  def snapshot do
    GenServer.call(__MODULE__, :snapshot)
  end

  @doc "Read an individual candidate weight; a return value is NOT a verified fact."
  @spec edge(String.t(), String.t(), atom()) :: map() | nil
  def edge(from_id, to_id, relation) do
    GenServer.call(__MODULE__, {:edge, {from_id, to_id, relation}})
  end

  @impl true
  def init(_opts) do
    {:ok, %{
      secret: :crypto.strong_rand_bytes(32),
      seen: MapSet.new(),
      edges: %{},
      accepted: 0,
      duplicates: 0
    }}
  end

  @impl true
  def handle_call({:observe, evidence, edges}, _from, state) do
    # Edges have already been verified from Core's current SemanticInput.
    # Validate again at this service boundary; callers cannot arbitrarily
    # supply negative weights or use a nonlexical source.
    unique_edges = Enum.uniq(edges)

    cond do
      byte_size(evidence) == 0 or byte_size(evidence) > 512 or
          length(unique_edges) != 2 or not Enum.all?(unique_edges, &valid_edge?/1) ->
        {:reply, {:error, :invalid_observation}, state}

      true ->
        receipt = :crypto.mac(:hmac, :sha256, state.secret, evidence)

        cond do
          MapSet.member?(state.seen, receipt) ->
            {:reply, {:ok, :duplicate}, %{state | duplicates: state.duplicates + 1}}

          MapSet.size(state.seen) >= @max_experiences ->
            {:reply, {:error, :experience_capacity}, state}

          map_size(state.edges) +
              Enum.count(unique_edges, &(not Map.has_key?(state.edges, &1))) >
              @max_edges ->
            {:reply, {:error, :edge_capacity}, state}

          true ->
            updated =
              Enum.reduce(unique_edges, state.edges, fn key, all ->
                previous = Map.get(all, key, %{weight: 0.0, experiences: 0})

                Map.put(all, key, %{
                  weight: min(previous.weight + @delta, @max_weight),
                  experiences: previous.experiences + 1,
                  evidence_tier: :user_reported_structural,
                  lexical_sense_verified?: false,
                  deployment: :shadow_only
                })
              end)

            {:reply, {:ok, :recorded}, %{state |
              edges: updated,
              seen: MapSet.put(state.seen, receipt),
              accepted: state.accepted + 1
            }}
        end
    end
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    {:reply, %{
      mode: :shadow_only,
      experiences: state.accepted,
      duplicates: state.duplicates,
      connections: map_size(state.edges),
      max_experiences: @max_experiences,
      max_connections: @max_edges,
      weight_cap: @max_weight,
      live_priming?: false,
      persisted?: false
    }, state}
  end

  @impl true
  def handle_call({:edge, key}, _from, state) do
    {:reply, Map.get(state.edges, key), state}
  end

  defp valid_edge?({from_id, to_id, relation})
       when is_binary(from_id) and is_binary(to_id) and
              relation in [:actor, :patient] do
    byte_size(from_id) in 3..128 and byte_size(to_id) in 3..128 and
      lex_id?(from_id, "verb") and
      lex_id?(to_id, "noun") and from_id != to_id
  end

  defp valid_edge?(_), do: false

  defp lex_id?(id, pos) do
    case String.split(id, "|", parts: 3) do
      [word, ^pos, sense] ->
        word != "" and sense != "" and sense != "fallback" and
          not String.contains?(word, "|")
      _ -> false
    end
  end
end
