defmodule Brain.Hippocampus do
  @moduledoc """
  Hippocampus — episodic binding & sequence memory.

  • INPUT: An ATL “slate” (%{winners, tokens, ...}) or a map with
           :lifg_choices and :tokens (we normalize it).
  • OUTPUT: An `episode` struct-like map stored in a rolling window.
  • STATE:
      - episodes: bounded queue of recent episodes
      - assoc_norm: co-occurrence counts between lemma norms
      - assoc_id:   co-occurrence counts between sense ids
      - seq_bigrams: token index bigram counts (simple sequence memory)

  Public API:
    - start_link/1
    - encode/1     # episode encode from ATL slate or {choices,tokens}
    - snapshot/0
    - reset/0
  """

  use GenServer
  @name __MODULE__

  # ── Public API ────────────────────────────────────────────────────────────
  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, opts, name: @name)

  @doc "Encode an episode from an ATL slate (preferred)."
  def encode(%{winners: _} = slate), do: GenServer.call(@name, {:encode, slate})

  def encode(%{lifg_choices: ch, tokens: toks}) when is_list(ch) and is_list(toks) do
    slate = %{winners: norm_choices(ch), tokens: toks}
    GenServer.call(@name, {:encode, slate})
  end

  @doc "Peek current Hippocampus state."
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Clear episodes and counts."
  def reset, do: GenServer.call(@name, :reset)

  # ── GenServer callbacks ──────────────────────────────────────────────────
  @impl true
  def init(opts) do
    keep = Keyword.get(opts, :keep, 300)
    {:ok, %{keep: keep, episodes: [], assoc_norm: %{}, assoc_id: %{}, seq_bigrams: %{}, last_episode: nil}}
  end

  @impl true
  def handle_call({:encode, slate}, _from, st) do
ts = System.system_time(:millisecond)
episode = build_episode(slate, ts)

    assoc_norm  = bump_pairs(st.assoc_norm, episode.norms)
    assoc_id    = bump_pairs(st.assoc_id,   episode.ids)
    seq_bigrams = bump_bigrams(st.seq_bigrams, episode.token_order)

    episodes = [episode | st.episodes] |> Enum.take(st.keep)
    st1 = %{st | episodes: episodes, assoc_norm: assoc_norm, assoc_id: assoc_id,
                 seq_bigrams: seq_bigrams, last_episode: episode}
    {:reply, episode, st1}
  end

  def handle_call(:snapshot, _from, st), do: {:reply, st, st}

  def handle_call(:reset, _from, st) do
    {:reply, :ok, %{st | episodes: [], assoc_norm: %{}, assoc_id: %{}, seq_bigrams: %{}, last_episode: nil}}
  end

  # ── Internals ────────────────────────────────────────────────────────────
  defp build_episode(%{winners: winners, tokens: tokens}, ts) do
    norms = Enum.map(winners, &(&1.norm || id_norm(&1.id) || "")) |> Enum.reject(&(&1 == ""))
    ids   = Enum.map(winners, & &1.id) |> Enum.reject(&is_nil/1)
    token_order =
      winners
      |> Enum.map(fn w -> w[:raw][:token_index] || w.raw["token_index"] || 0 end)
      |> Enum.sort()

    %{
      ts_ms: ts,
      text: extract_text(tokens),
      winners: winners,
      tokens: tokens,
      token_count: length(tokens || []),
      winner_count: length(winners || []),
      norms: norms,
      ids: ids,
      token_order: token_order
    }
  end

  defp extract_text(tokens) do
    case tokens do
      [%{phrase: _} | _] ->
        tokens
        |> Enum.sort_by(& &1.index)
        |> Enum.filter(&is_binary(&1.phrase))
        |> Enum.map(& &1.phrase)
        |> Enum.join(" ")
      _ -> ""
    end
  end

  defp id_norm(nil), do: nil
  defp id_norm(id) when is_binary(id), do: id |> String.split("|") |> List.first()

  defp bump_pairs(map, list) when is_list(list) do
    pairs = for a <- list, b <- list, a < b, do: {a, b}
    Enum.reduce(pairs, map, fn k, acc -> Map.update(acc, k, 1, &(&1 + 1)) end)
  end

  defp bump_bigrams(map, ord) do
    bigrams =
      ord
      |> Enum.sort()
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [a, b] -> {a, b} end)

    Enum.reduce(bigrams, map, fn k, acc -> Map.update(acc, k, 1, &(&1 + 1)) end)
  end

  defp norm_choices(choices) do
    Enum.map(choices, fn ch ->
      id    = ch[:id]    || ch["id"]
      lemma = ch[:lemma] || ch["lemma"]
      %{
        id: id,
        norm: id_norm(id) || (lemma && norm_text(lemma)),
        lemma: lemma,
        score: ch[:score] || ch["score"],
        raw: ch
      }
    end)
  end

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
  defp norm_text(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()
end

