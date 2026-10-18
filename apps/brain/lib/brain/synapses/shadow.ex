defmodule Brain.Synapses.Shadow do
  @moduledoc """
  Experience-driven, *shadow-only* synaptic plasticity.

  Distinct structurally supported user reports create small provisional
  connection weights. Human review may confirm or reject an existing
  experience; rejecting reverses precisely its own contribution, leaving
  independent experiences intact. An identical claim is not independent
  corroboration, regardless of session or repetition count.

  Retains no utterances, raw event participants, reviewer names or DB writes.
  In development, an authenticated receipt snapshot and its stable local HMAC
  secret can survive restarts; test instances remain ephemeral by default.
  Only validated receipts are loaded, and weights are rebuilt from scratch.

  This subsystem never stimulates cells, influences LIFG, trains Axon, or
  asserts that a selected dictionary sense or user claim is objectively true.
  Review calls require a trusted application caller; a reviewer string is
  attribution, NOT an authentication mechanism. No chat review route exists.
  """

  use GenServer

  alias Brain.Synapses.ReceiptStore

  @unreviewed_delta 0.01
  @reviewed_delta 0.03
  @max_weight 0.08
  @max_experiences 512
  @max_edges 1024
  @max_preview 256

  @type edge_key :: {String.t(), String.t(), :actor | :patient}

  def start_link(opts \\ []) do
    configured = Application.get_env(:brain, __MODULE__, [])
    opts = Keyword.merge(configured, opts)

    case Keyword.get(opts, :name, __MODULE__) do
      nil -> GenServer.start_link(__MODULE__, opts)
      name -> GenServer.start_link(__MODULE__, opts, name: name)
    end
  end

  @doc "Record a Core-revalidated, attributed experience with typed edges."
  @spec observe(map(), GenServer.server()) :: {:ok, :recorded | :duplicate} | {:error, atom()}
  def observe(observation, server \\ __MODULE__)
  def observe(%{evidence: evidence, edges: edges, provenance: :user_reported} = entry, server)
      when is_binary(evidence) and is_list(edges) do
    session = Map.get(entry, :session_id)
    call({:observe, evidence, edges, session}, server)
  end

  def observe(_, _), do: {:error, :invalid_observation}

  @doc """
  Review an already recorded experience. The caller must be a trusted,
  authenticated human-review interface, NOT an unparsed chat instruction.

  :confirm increases its contribution from 0.01 to 0.03.
  :reject removes its contribution but retains its receipt for deduplication.
  Later authenticated review can reverse either decision.
  """
  @spec review(map(), GenServer.server()) :: {:ok, :confirmed | :rejected | :unchanged} | {:error, atom()}
  def review(review, server \\ __MODULE__)
  def review(%{evidence: evidence, provenance: :human_review,
               reviewer: reviewer, decision: decision}, server)
      when is_binary(evidence) and is_binary(reviewer) and
           decision in [:confirm, :reject] do
    if byte_size(reviewer) in 1..128 and String.trim(reviewer) != "" do
      call({:review, evidence, decision, reviewer}, server)
    else
      {:error, :invalid_review}
    end
  end

  def review(_, _), do: {:error, :invalid_review}

  @doc """
  Read-only correction proposal across two independently revalidated
  observations. A matching previous receipt must already exist and remain
  active. The subject and predicate cell identities must agree; the patient
  must differ. No receipt or weight is ever changed by this call.
  """
  @spec propose_correction(map(), map(), GenServer.server()) :: map()
  def propose_correction(old, new, server \\ __MODULE__)
  def propose_correction(
        %{evidence: old_evidence, provenance: :user_reported, edges: old_edges},
        %{evidence: new_evidence, provenance: :user_reported, edges: new_edges},
        server
      )
      when is_binary(old_evidence) and is_binary(new_evidence) and
             is_list(old_edges) and is_list(new_edges) do
    call({:propose_correction, old_evidence, old_edges, new_evidence, new_edges}, server)
  end

  def propose_correction(_, _, _), do: %{status: :invalid_evidence}

  @doc "Read-only test of whether an exact evidence report has been recorded (including rejection)."
  @spec recorded?(binary(), GenServer.server()) :: boolean()
  def recorded?(evidence, server \\ __MODULE__)

  def recorded?(evidence, server) when is_binary(evidence) do
    call({:recorded?, evidence}, server)
  end

  def recorded?(_, _), do: false

  @doc "Read-only aggregate counters with no plaintext experiences."
  @spec snapshot(GenServer.server()) :: map()
  def snapshot(server \\ __MODULE__), do: GenServer.call(server, :snapshot)

  @doc "A provisional edge is not evidence that its dictionary sense is right."
  @spec edge(String.t(), String.t(), atom(), GenServer.server()) :: map() | nil
  def edge(from_id, to_id, relation, server \\ __MODULE__) do
    GenServer.call(server, {:edge, {from_id, to_id, relation}})
  end

  @doc """
  Side-by-side shadow support for a caller-provided, *existing* verb slate
  against an independently chosen noun context. Never a lexical decision.
  """
  @spec preview([String.t()], String.t(), :actor | :patient, GenServer.server()) :: map()
  def preview(candidates, noun_id, relation, server \\ __MODULE__)

  def preview(candidates, noun_id, relation, server)
      when is_list(candidates) and is_binary(noun_id) and
           relation in [:actor, :patient] do
    ids = Enum.uniq(candidates)

    if length(ids) in 1..@max_preview and lex_id?(noun_id, "noun") and
         Enum.all?(ids, &lex_id?(&1, "verb")) do
      GenServer.call(server, {:preview, ids, noun_id, relation})
    else
      %{status: :invalid_candidates, candidates: []}
    end
  end

  def preview(_, _, _, _), do: %{status: :invalid_candidates, candidates: []}

  @impl true
  def init(opts) do
    durable? = Keyword.get(opts, :durable?, false) == true
    dir = Keyword.get(opts, :dir, "tmp/synapses")

    loaded =
      if durable? do
        case ReceiptStore.open(dir) do
          {:ok, %{secret: secret, receipts: receipts}} ->
            {secret, receipts, :ready}

          {:error, _} ->
            {:crypto.strong_rand_bytes(32), %{}, :unavailable}
        end
      else
        {:crypto.strong_rand_bytes(32), %{}, :disabled}
      end

    {secret, receipts, storage_status} = loaded

    {:ok, %{
      secret: secret,
      receipts: receipts,
      edges: weights(receipts),
      durable?: durable?,
      storage_dir: dir,
      storage_status: storage_status,
      duplicates: 0,
      reviews: 0,
      rejected_reviews: 0
    }}
  end

  @impl true
  def handle_call({:observe, evidence, edges, session}, _from, state) do
    unique_edges = Enum.uniq(edges)

    cond do
      state.storage_status == :unavailable ->
        {:reply, {:error, :storage_unavailable}, state}

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
            record = %{
              edges: canonical_edges(unique_edges),
              verdict: :unreviewed,
              reviews: [],
              first_seen_ms: System.system_time(:millisecond),
              first_session_digest: session_digest(state.secret, session)
            }
            receipts = Map.put(state.receipts, receipt, record)

            case commit_receipts(state, receipts) do
              {:ok, updated} -> {:reply, {:ok, :recorded}, updated}
              {:error, reason} -> {:reply, {:error, reason}, state}
            end
        end
    end
  end

  @impl true
  def handle_call({:review, evidence, decision, reviewer}, _from, state) do
    if state.storage_status == :unavailable do
      {:reply, {:error, :storage_unavailable}, state}
    else
      if not valid_evidence?(evidence) do
        {:reply, {:error, :invalid_review}, state}
      else
        key = receipt(state, evidence)

      case Map.fetch(state.receipts, key) do
        :error ->
          {:reply, {:error, :unknown_experience}, state}

        {:ok, record} ->
          verdict = if decision == :confirm, do: :confirmed, else: :rejected

          cond do
            record.verdict == verdict ->
              {:reply, {:ok, :unchanged}, state}

            length(Map.get(record, :reviews, [])) >= 8 ->
              {:reply, {:error, :review_capacity}, state}

            true ->
              audit = %{
                decision: verdict,
                reviewer: ReceiptStore.reviewer_digest(state.secret, reviewer),
                at_ms: System.system_time(:millisecond)
              }

              changed = %{record | verdict: verdict,
                reviews: Map.get(record, :reviews, []) ++ [audit]}
              receipts = Map.put(state.receipts, key, changed)

              case commit_receipts(state, receipts) do
                {:ok, updated} ->
                  {:reply, {:ok, verdict},
                   %{updated | reviews: state.reviews + 1,
                     rejected_reviews: state.rejected_reviews +
                       if(decision == :reject, do: 1, else: 0)}}

                {:error, reason} ->
                  {:reply, {:error, reason}, state}
              end
          end
      end
      end
    end
  end

  @impl true
  def handle_call({:propose_correction, prior, old_edges, replacement, new_edges}, _from, state) do
    old = canonical_correction_edges(old_edges)
    new = canonical_correction_edges(new_edges)

    result =
      with true <- valid_evidence?(prior) and valid_evidence?(replacement),
           true <- prior != replacement,
           {:ok, {old_actor, old_patient}} <- old,
           {:ok, {new_actor, new_patient}} <- new,
           true <- old_actor == new_actor,
           true <- elem(old_patient, 0) == elem(new_patient, 0),
           true <- elem(old_patient, 1) != elem(new_patient, 1),
           {:ok, record} <- Map.fetch(state.receipts, receipt(state, prior)) do
        cond do
          MapSet.new(record.edges) != MapSet.new(old_edges) ->
            %{status: :prior_receipt_mismatch}

          record.verdict == :rejected ->
            %{status: :prior_already_rejected}

          Map.has_key?(state.receipts, receipt(state, replacement)) ->
            %{status: :replacement_already_reported}

          true ->
            %{status: :review_proposed,
              source: :explicit_not_patient_correction,
              actor_id: elem(old_actor, 1),
              predicate_id: elem(old_actor, 0),
              previous_patient_id: elem(old_patient, 1),
              proposed_patient_id: elem(new_patient, 1),
              previous_review: record.verdict,
              old_patient_support:
                Map.get(state.edges, old_patient, %{weight: 0.0}).weight,
              applied?: false, affects_lifg?: false,
              externally_verified?: false}
        end
      else
        :error -> %{status: :unknown_prior_experience}
        _ -> %{status: :incompatible_or_ambiguous}
      end

    {:reply, result, state}
  end

  @impl true
  def handle_call({:recorded?, evidence}, _from, state) do
    {:reply, valid_evidence?(evidence) and
       Map.has_key?(state.receipts, receipt(state, evidence)), state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    entries = Map.values(state.receipts)

    {:reply, %{
      mode: :shadow_only,
      persistence_status: state.storage_status,
      experiences: Enum.count(entries, &(&1.verdict != :rejected)),
      rejected_experiences: Enum.count(entries, &(&1.verdict == :rejected)),
      confirmed_experiences: Enum.count(entries, &(&1.verdict == :confirmed)),
      durable_review_events: Enum.reduce(entries, 0,
        fn entry, total -> total + length(Map.get(entry, :reviews, [])) end),
      duplicates: state.duplicates,
      reviews: state.reviews,
      rejected_reviews: state.rejected_reviews,
      connections: map_size(state.edges),
      max_experiences: @max_experiences,
      max_connections: @max_edges,
      weight_cap: @max_weight,
      live_priming?: false,
      persisted?: state.storage_status == :ready
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
        edge = Map.get(state.edges, {id, noun, relation}, %{
          weight: 0.0, experiences: 0,
          provisional_weight: 0.0, reviewed_weight: 0.0,
          provisional_experiences: 0, reviewed_experiences: 0
        })

        %{id: id, shadow_support: edge.weight,
          provisional_support: edge.provisional_weight,
          reviewed_support: edge.reviewed_weight,
          distinct_report_contexts: edge.experiences,
          provisional_contexts: edge.provisional_experiences,
          reviewed_contexts: edge.reviewed_experiences}
      end)

    {:reply, %{
      status: :shadow_only,
      relation: relation,
      candidates: candidates,
      affects_lifg?: false,
      senses_verified?: false
    }, state}
  end

  defp call(message, server) do
    present? =
      cond do
        is_pid(server) -> Process.alive?(server)
        is_atom(server) -> is_pid(Process.whereis(server))
        true -> false
      end

    if present?, do: GenServer.call(server, message, 5_000),
      else: {:error, :not_running}
  catch
    :exit, _ -> {:error, :not_running}
  end

  defp commit_receipts(state, receipts) do
    outcome =
      if state.durable? do
        ReceiptStore.persist(state.storage_dir, state.secret, receipts)
      else
        :ok
      end

    case outcome do
      :ok -> {:ok, %{state | receipts: receipts, edges: weights(receipts)}}
      _ -> {:error, :persistence_failed}
    end
  end

  defp canonical_edges(edges) do
    Enum.sort_by(edges, fn
      {_, _, :actor} -> 0
      _ -> 1
    end)
  end

  defp weights(receipts) do
    receipts
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
    |> Enum.reject(&(&1.verdict == :rejected))
    |> Enum.reduce(%{}, fn %{edges: edges, verdict: verdict}, all ->
      delta = if verdict == :confirmed, do: @reviewed_delta, else: @unreviewed_delta

      Enum.reduce(edges, all, fn key, acc ->
        prev = Map.get(acc, key, %{
          provisional_weight: 0.0, reviewed_weight: 0.0,
          experiences: 0, reviewed_experiences: 0, provisional_experiences: 0
        })

        provisional = prev.provisional_weight +
          if(verdict == :unreviewed, do: delta, else: 0.0)
        reviewed = prev.reviewed_weight +
          if(verdict == :confirmed, do: delta, else: 0.0)

        Map.put(acc, key, %{
          weight: min(provisional + reviewed, @max_weight),
          provisional_weight: min(provisional, @max_weight),
          reviewed_weight: min(reviewed, @max_weight),
          experiences: prev.experiences + 1,
          provisional_experiences: prev.provisional_experiences +
            if(verdict == :unreviewed, do: 1, else: 0),
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

  defp session_digest(secret, session)
       when is_binary(session) and byte_size(session) in 1..128 do
    ReceiptStore.session_digest(secret, session)
  end

  defp session_digest(_, _), do: nil

  defp receipt(state, evidence) do
    :crypto.mac(:hmac, :sha256, state.secret, evidence)
    |> Base.encode16(case: :lower)
  end

  defp canonical_correction_edges(edges) when is_list(edges) do
    if length(edges) == 2 do
      actor = Enum.find(edges, fn
        {_, _, :actor} -> true
        _ -> false
      end)

      patient = Enum.find(edges, fn
        {_, _, :patient} -> true
        _ -> false
      end)

      if valid_edge?(actor) and valid_edge?(patient) and
           elem(actor, 0) == elem(patient, 0) do
        {:ok, {actor, patient}}
      else
        :error
      end
    else
      :error
    end
  end

  defp canonical_correction_edges(_), do: :error

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
