defmodule Brain.Synapses.Shadow do
  @moduledoc """
  Experience-driven, *shadow-only* synaptic plasticity.

  Distinct structurally supported user reports create small provisional
  connection weights. Human review may confirm or reject an existing
  experience; rejecting reverses precisely its own contribution, leaving
  independent experiences intact. An identical claim is not independent
  corroboration, regardless of session or repetition count.

  Retains no utterances, raw event participants, reviewer names or DB writes.
  Evidence is keyed using an in-process HMAC; all records vanish on restart.

  This subsystem never stimulates cells, influences LIFG, trains Axon, or
  asserts that a selected dictionary sense or user claim is objectively true.
  """

  use GenServer

  @unreviewed_delta 0.01
  @reviewed_delta 0.03
  @max_weight 0.08
  @max_experiences 512
  @max_edges 1024
  @max_preview 64

  @type edge_key :: {String.t(), String.t(), :actor | :patient}

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Record a Core-revalidated, attributed experience with typed edges."
  @spec observe(map()) :: {:ok, :recorded | :duplicate} | {:error, atom()}
  def observe(%{evidence: evidence, edges: edges, provenance: :user_reported})
      when is_binary(evidence) and is_list(edges) do
    call({:observe, evidence, edges})
  end

  def observe(_), do: {:error, :invalid_observation}

  @doc """
  Review an already recorded experience. The caller must be a trusted,
  authenticated human-review interface, NOT an unparsed chat instruction.

  :confirm increases its contribution from 0.01 to 0.03.
  :reject removes its contribution but retains its receipt for deduplication.
  Later authenticated review can reverse either decision.
  """
  @spec review(map()) :: {:ok, :confirmed | :rejected | :unchanged} | {:error, atom()}
  def review(%{evidence: evidence, provenance: :human_review,
               reviewer: reviewer, decision: decision})
      when is_binary(evidence) and is_binary(reviewer) and
           decision in [:confirm, :reject] do
    if byte_size(reviewer) in 1..128 and String.trim(reviewer) != "" do
      call({:review, evidence, decision})
    else
      {:error, :invalid_review}
    end
  end

  def review(_), do: {:error, :invalid_review}

  @doc "Read-only aggregate counters with no plaintext experiences."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(__MODULE__, :snapshot)

  @doc "A provisional edge is not evidence that its dictionary sense is right."
  @spec edge(String.t(), String.t(), atom()) :: map() | nil
  def edge(from_id, to_id, relation) do
    GenServer.call(__MODULE__, {:edge, {from_id, to_id, relation}})
  end

  @doc """
  Side-by-side shadow support for a caller-provided, *existing* verb slate
  against an independently chosen noun context. Never a lexical decision.
  """
  @spec preview([String.t()], String.t(), :actor | :patient) :: map()
  def preview(candidates, noun_id, relation)
      when is_list(candidates) and is_binary(noun_id) and
           relation in [:actor, :patient] do
    ids = Enum.uniq(candidates)

    if length(ids) in 1..@max_preview and lex_id?(noun_id, "noun") and
         Enum.all?(ids, &lex_id?(&1, "verb")) do
      GenServer.call(__MODULE__, {:preview, ids, noun_id, relation})
    else
      %{status: :invalid_candidates, candidates: []}
    end
  end

  def preview(_, _, _), do: %{status: :invalid_candidates, candidates: []}

  @impl true
  def init(_opts) do
    {:ok, %{
      secret: :crypto.strong_rand_bytes(32),
      receipts: %{},
      edges: %{},
      duplicates: 0,
      reviews: 0,
      rejected_reviews: 0
    }}
  end

  @impl true
  def handle_call({:observe, evidence, edges}, _from, state) do
    unique_edges = Enum.uniq(edges)

    cond do
      not valid_evidence?(evidence) or
          length(unique_edges) != 2 or
          not Enum.all?(unique_edges, &valid_edge?/1) or
          Enum.sort(Enum.map(unique_edges, &elem(&1, 2))) != [:actor, :patient] or
          length(Enum.uniq(Enum.map(unique_edges, &elem(&1, 0)))) != 1 ->
        {:reply, {:error, :invalid_observation}, state}

      true ->
        receipt = receipt(state, evidence)

        cond do
          Map.has_key?(state.receipts, receipt) ->
            {:reply, {:ok, :duplicate}, %{state | duplicates: state.duplicates + 1}}

          map_size(state.receipts) >= @max_experiences ->
            {:reply, {:error, :experience_capacity}, state}

          map_size(state.edges) +
              Enum.count(unique_edges, &(not Map.has_key?(state.edges, &1))) >
              @max_edges ->
            {:reply, {:error, :edge_capacity}, state}

          true ->
            record = %{edges: unique_edges, verdict: :unreviewed}
            receipts = Map.put(state.receipts, receipt, record)
            {:reply, {:ok, :recorded},
             %{state | receipts: receipts, edges: weights(receipts)}}
        end
    end
  end

  @impl true
  def handle_call({:review, evidence, decision}, _from, state) do
    if not valid_evidence?(evidence) do
      {:reply, {:error, :invalid_review}, state}
    else
      key = receipt(state, evidence)

      case Map.fetch(state.receipts, key) do
        :error ->
          {:reply, {:error, :unknown_experience}, state}

        {:ok, record} ->
          verdict = if decision == :confirm, do: :confirmed, else: :rejected

          if record.verdict == verdict do
            {:reply, {:ok, :unchanged}, state}
          else
            receipts = Map.put(state.receipts, key, %{record | verdict: verdict})
            {:reply, {:ok, if(decision == :confirm, do: :confirmed, else: :rejected)},
             %{state | receipts: receipts, edges: weights(receipts),
               reviews: state.reviews + 1,
               rejected_reviews: state.rejected_reviews +
                 if(decision == :reject, do: 1, else: 0)}}
          end
      end
    end
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    entries = Map.values(state.receipts)

    {:reply, %{
      mode: :shadow_only,
      experiences: Enum.count(entries, &(&1.verdict != :rejected)),
      rejected_experiences: Enum.count(entries, &(&1.verdict == :rejected)),
      confirmed_experiences: Enum.count(entries, &(&1.verdict == :confirmed)),
      duplicates: state.duplicates,
      reviews: state.reviews,
      rejected_reviews: state.rejected_reviews,
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

  @impl true
  def handle_call({:preview, ids, noun, relation}, _from, state) do
    candidates =
      Enum.map(ids, fn id ->
        edge = Map.get(state.edges, {id, noun, relation}, %{weight: 0.0, experiences: 0})
        %{id: id, shadow_support: edge.weight,
          independent_contexts: edge.experiences}
      end)

    {:reply, %{
      status: :shadow_only,
      relation: relation,
      candidates: candidates,
      affects_lifg?: false,
      senses_verified?: false
    }, state}
  end

  defp call(message) do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, message, 5_000)
    else
      {:error, :not_running}
    end
  catch
    :exit, _ -> {:error, :not_running}
  end

  defp weights(receipts) do
    receipts
    |> Map.values()
    |> Enum.reject(&(&1.verdict == :rejected))
    |> Enum.reduce(%{}, fn %{edges: edges, verdict: verdict}, all ->
      delta = if verdict == :confirmed, do: @reviewed_delta, else: @unreviewed_delta

      Enum.reduce(edges, all, fn key, acc ->
        prev = Map.get(acc, key, %{weight: 0.0, experiences: 0,
          reviewed_experiences: 0})

        Map.put(acc, key, %{
          weight: min(prev.weight + delta, @max_weight),
          experiences: prev.experiences + 1,
          reviewed_experiences: prev.reviewed_experiences +
            if(verdict == :confirmed, do: 1, else: 0)
        })
      end)
    end)
    |> Map.new(fn {key, entry} ->
      {key, Map.merge(entry, %{
        evidence_tier: if(entry.reviewed_experiences > 0,
          do: :human_reviewed_structural, else: :user_reported_structural),
        lexical_sense_verified?: false,
        deployment: :shadow_only
      })}
    end)
  end

  defp receipt(state, evidence), do:
    :crypto.mac(:hmac, :sha256, state.secret, evidence)

  defp valid_evidence?(value),
    do: is_binary(value) and byte_size(value) in 1..512

  defp valid_edge?({from_id, to_id, relation})
       when is_binary(from_id) and is_binary(to_id) and
              relation in [:actor, :patient] do
    byte_size(from_id) in 3..128 and byte_size(to_id) in 3..128 and
      lex_id?(from_id, "verb") and lex_id?(to_id, "noun") and from_id != to_id
  end

  defp valid_edge?(_), do: false

  defp lex_id?(id, pos) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [word, ^pos, sense] ->
        word != "" and sense != "" and sense != "fallback" and
          not String.contains?(word, "|")
      _ -> false
    end
  end

  defp lex_id?(_, _), do: false
end
