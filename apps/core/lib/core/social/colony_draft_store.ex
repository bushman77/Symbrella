defmodule Core.Social.ColonyDraftStore do
  @moduledoc """
  Small in-memory, one-shot approval store. No durable social content is saved.

  Drafts expire after 15 minutes and are *consumed before* any outbound POST.
  A failed or uncertain network outcome is never retried automatically.
  """
  use GenServer

  @max_pending 16
  @ttl_ms 15 * 60_000

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, name: Keyword.get(opts, :name, __MODULE__))
  end

  @doc "Store a human-reviewable reply and return an opaque approval receipt."
  def put(draft, server \\ __MODULE__) when is_map(draft) do
    GenServer.call(server, {:put, draft})
  end

  @doc """
  Atomically consume exactly one approved reply. The approval phrase must be
  typed or pasted in a separate explicit operator action after reading it.
  """
  def take(id, phrase, server \\ __MODULE__) when is_binary(id) and is_binary(phrase) do
    GenServer.call(server, {:take, id, phrase})
  end

  @impl true
  def init(_), do: {:ok, %{}}

  @impl true
  def handle_call({:put, draft}, _from, state) do
    now = System.monotonic_time(:millisecond)
    state = prune(state, now)

    if map_size(state) >= @max_pending do
      {:reply, {:error, :too_many_pending_colony_drafts}, state}
    else
      id = Base.url_encode64(:crypto.strong_rand_bytes(18), padding: false)
      post_id = Map.fetch!(draft, :post_id)
      approval = "APPROVE COLONY COMMENT " <> id
      receipt = %{id: id, post_id: post_id, approval_phrase: approval, expires_in_seconds: div(@ttl_ms, 1_000)}
      {:reply, {:ok, receipt}, Map.put(state, id, {now + @ttl_ms, approval, draft})}
    end
  end

  @impl true
  def handle_call({:take, id, phrase}, _from, state) do
    now = System.monotonic_time(:millisecond)
    state = prune(state, now)

    case Map.get(state, id) do
      {_expires, ^phrase, draft} ->
        {:reply, {:ok, draft}, Map.delete(state, id)}

      nil ->
        {:reply, {:error, :colony_draft_missing_or_expired}, state}

      _ ->
        {:reply, {:error, :approval_phrase_mismatch}, state}
    end
  end

  defp prune(state, now) do
    Map.reject(state, fn {_id, {expires, _approval, _draft}} -> expires <= now end)
  end
end
