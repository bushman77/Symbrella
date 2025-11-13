defmodule Core.Recall.Synonyms.Providers.Fallback do
  @moduledoc """
  Safe, dependency-free provider.

  Returns the identity lemma as a single entry and optionally merges any
  extras passed via `opts[:extras]` (already-normalized entries).
  """

  @behaviour Core.Recall.Synonyms.Provider
  alias Core.Recall.Synonyms.Normalize

  @impl true
  def lookup(word, pos, opts) do
    extras =
      opts
      |> Keyword.get(:extras, [])
      |> Enum.map(&normalize_entry/1)

    base = %{
      lemma: Normalize.word(word),
      pos: Normalize.pos(pos),
      prior: 1.0,
      source: "fallback",
      meta: %{}
    }

    {:ok, [base | extras], %{provider: :fallback}}
  end

  defp normalize_entry(%{lemma: l} = e) do
    e
    |> Map.put(:lemma, Normalize.word(l))
    |> Map.update(:pos, nil, &Normalize.pos/1)
    |> Map.put_new(:prior, 0.5)
    |> Map.put_new(:source, "extras")
    |> Map.put_new(:meta, %{})
  end

  defp normalize_entry(other) when is_binary(other) do
    %{lemma: Normalize.word(other), pos: nil, prior: 0.5, source: "extras", meta: %{}}
  end
end
