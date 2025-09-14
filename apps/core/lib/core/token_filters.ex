defmodule Core.TokenFilters do
  @moduledoc """
  Pure token filters for early pipeline hygiene.
  """

  alias Core.SemanticInput, as: SI

  @spec prune(SI.t(), keyword()) :: SI.t()
  def prune(%SI{} = si, opts \\ []) do
    min_n = Keyword.get(opts, :min_n, 1)

    {kept, _removed} =
      Enum.split_with(si.tokens, fn
        %Core.Token{n: n} when is_integer(n) -> n >= min_n
        %Core.Token{} -> true
        _ -> true
      end)

    ev = %{
      stage: :token_prune,
      ts_ms: now_ms(),
      meta: %{min_n: min_n, kept: length(kept), removed: length(si.tokens) - length(kept)}
    }

    %SI{si | tokens: kept, trace: si.trace ++ [ev]}
  end

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end

