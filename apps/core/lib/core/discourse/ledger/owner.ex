defmodule Core.Discourse.Ledger.Owner do
  @moduledoc """
  Serialized owner of conversation-scoped semantic ledger records.

  ETS is protected: readers can inspect committed snapshots, but only this
  process can append, trim, or reset a session. No raw transcripts, LIFG
  slates, system prompts, or conversation-wide cognition traces are stored.
  This first version is intentionally in-memory, not restart-persistent.
  """
  use GenServer

  alias Core.Semantic.FinitePastDiscourse
  alias Core.Semantic.MetalinguisticClaim

  @table :core_discourse_ledger
  @default_keep 4096
  @maximum_keep 16_384

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    :ets.new(@table, [:named_table, :protected, :set, {:read_concurrency, true}])
    {:ok, %{}}
  end

  @spec append(String.t(), map()) :: {:ok, pos_integer()} | {:error, atom()}
  def append(session, record) when is_binary(session) and is_map(record) do
    GenServer.call(__MODULE__, {:append, session, record})
  end

  @doc """
  The sole finite-past admission boundary. Checks the original SI and the
  exact projected record again inside the serialized owner before persistence.
  A digest or a caller-made record alone cannot authorize admission.
  """
  @spec append_verified_finite_past(String.t(), map(), map(), function()) ::
          {:ok, pos_integer()} | {:error, atom()}
  def append_verified_finite_past(session, si, candidate, lookup)
      when is_binary(session) and is_map(si) and is_map(candidate) and
             is_function(lookup, 1) do
    GenServer.call(__MODULE__,
      {:append_verified_finite_past, session, si, candidate, lookup})
  end

  def append_verified_finite_past(_, _, _, _),
    do: {:error, :invalid_finite_past_evidence}

  @spec append_verified_metalinguistic(String.t(), map(), map(), integer()) ::
          {:ok, pos_integer()} | {:error, atom()}
  def append_verified_metalinguistic(session, si, candidate, now_ms)
      when is_binary(session) and is_map(si) and is_map(candidate) and is_integer(now_ms) do
    GenServer.call(__MODULE__,
      {:append_verified_metalinguistic, session, si, candidate, now_ms})
  end
  def append_verified_metalinguistic(_, _, _, _),
    do: {:error, :invalid_metalinguistic_evidence}

  @spec entries(String.t()) :: [map()]
  def entries(session) when is_binary(session) do
    case :ets.lookup(@table, session) do
      [{^session, _last_seq, entries}] when is_list(entries) -> entries
      _ -> []
    end
  rescue
    ArgumentError -> []
  end

  @doc "Clear only the specified session, for explicit reset and isolated tests."
  @spec clear(String.t()) :: :ok
  def clear(session) when is_binary(session) do
    GenServer.call(__MODULE__, {:clear, session})
  end

  @impl true
  def handle_call({:append, _session,
                   %{kind: :reported_finite_past_locative_candidate}},
                  _from, state),
    do: {:reply, {:error, :verified_finite_past_required}, state}

  def handle_call({:append, _session, %{kind: :metalinguistic_claim}},
                  _from, state),
    do: {:reply, {:error, :verified_metalinguistic_required}, state}

  def handle_call({:append, _session, %{kind: :uninterpreted, source_term: _}},
                  _from, state),
    do: {:reply, {:error, :verified_metalinguistic_required}, state}

  def handle_call({:append, session, record}, _from, state)
      when is_binary(session) and is_map(record) do
    persist(session, record, state)
  end

  def handle_call({:append_verified_finite_past, session, si, candidate, lookup},
                  _from, state) do
    valid_session? = is_binary(session) and byte_size(session) in 1..128

    verified =
      if valid_session? and is_map(si) and is_map(candidate) and
           is_function(lookup, 1) and Map.get(si, :session_id) == session do
        FinitePastDiscourse.project(si, lookup: lookup)
      else
        :not_applicable
      end

    case verified do
      {:ok, ^candidate} -> persist(session, candidate, state)
      _ -> {:reply, {:error, :invalid_finite_past_evidence}, state}
    end
  end

  def handle_call({:append_verified_metalinguistic, session, si, candidate, now_ms},
                  _from, state) do
    same_session? =
      is_binary(session) and byte_size(session) in 1..128 and
        Map.get(si, :session_id) == session and
        Map.get(si, :source) in [:user, :prod, :test]

    valid? =
      if same_session? do
        case candidate do
          %{kind: :uninterpreted, source_term: _, observed_at_ms: observed_at_ms}
          when is_integer(observed_at_ms) ->
            # Replay the original event timestamp, not a newly sampled clock.
            # The caller's admission time is independently bounded to prevent
            # accepting future-dated or stale reconstructed source markers.
            now_ms >= observed_at_ms and now_ms - observed_at_ms <= 1_000 and
              MetalinguisticClaim.isolated_marker(si, observed_at_ms) == candidate

          %{kind: :metalinguistic_claim, source_seq: seq} ->
            source = Map.get(si, :metalinguistic_source)
            latest =
              case :ets.lookup(@table, session) do
                [{^session, _last_seq, [head | _]}] -> head
                _ -> nil
              end

            is_map(latest) and is_map(source) and
              Map.take(latest, MetalinguisticClaim.source_fields()) == source and
              MetalinguisticClaim.valid_source?(source, now_ms) and
              seq == latest.seq and
              MetalinguisticClaim.verified(si) == {:ok, candidate}

          _ -> false
        end
      else
        false
      end

    if valid?, do: persist(session, candidate, state),
      else: {:reply, {:error, :invalid_metalinguistic_evidence}, state}
  end

  def handle_call({:clear, session}, _from, state) do
    :ets.delete(@table, session)
    {:reply, :ok, state}
  end

  defp persist(session, record, state) do
    {sequence, previous} =
      case :ets.lookup(@table, session) do
        [{^session, last, existing}] -> {last + 1, existing}
        [] -> {1, []}
      end

    # Newest-first. Keep the per-session sequence monotonic even after trim.
    entry = Map.put(record, :seq, sequence)
    keep = retention()
    entries = [entry | Enum.take(previous, keep - 1)]
    true = :ets.insert(@table, {session, sequence, entries})
    {:reply, {:ok, sequence}, state}
  end


  defp retention do
    case Application.get_env(:core, Core.Discourse.Ledger, []) do
      config when is_list(config) ->
        case Keyword.get(config, :max_entries_per_session, @default_keep) do
          count when is_integer(count) and count > 0 ->
            min(count, @maximum_keep)
          _ -> @default_keep
        end

      _ -> @default_keep
    end
  end
end
