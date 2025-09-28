defmodule Core.TokenFilters do
  alias Core.{SemanticInput, Token}

  @doc """
  Prunes tokens with a safety guard: if pruning would yield an empty list,
  keep the original tokens. This prevents downstream nil/[] explosions while
  we refine span semantics and MW handling.
  """
  @spec prune(SemanticInput.t(), keyword()) :: SemanticInput.t()
  def prune(%SemanticInput{tokens: toks} = si, opts) do
    min_n = Keyword.get(opts, :min_n, 1)

    kept =
      toks
      |> Enum.filter(fn
        %Token{span: {s, k}, n: n} when is_integer(s) and is_integer(k) and n >= min_n ->
          # Accept either {start, stop} (k > s) or {start, len} (k > 0)
          k > s or k > 0

        _ ->
          false
      end)

    final = if kept == [], do: toks, else: kept

    put_trace(%{si | tokens: final}, :token_prune, %{
      min_n: min_n,
      kept: length(final),
      removed: max(length(toks) - length(final), 0)
    })
  end

  defp put_trace(%SemanticInput{trace: tr} = si, stage, meta) do
    ts_ms = System.os_time(:millisecond)
    trace = [%{stage: stage, meta: meta, ts_ms: ts_ms} | tr || []]
    %{si | trace: trace}
  end
end
