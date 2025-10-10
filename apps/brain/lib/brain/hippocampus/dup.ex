defmodule Brain.Hippocampus.Dup do
  @moduledoc """
  De-duplication knobs for Hippocampus episodes.
  """

  @mix_env (Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env()) || :prod
  @test_env @mix_env == :test

  @doc """
  If `:hippo_meta_dup_count` is true (and not test), increment `:dup_count` in meta.
  Otherwise, return the episode unchanged.

  NOTE: on the *first* duplicate, we want `dup_count=2` (two occurrences).
  """
  @spec bump_dup_count(%{meta: map()}) :: map()
  def bump_dup_count(%{meta: meta} = ep) do
    # Use compile-time branching for test env to avoid an unreachable `cond` clause warning.
    if @test_env do
      ep
    else
      if Application.get_env(:brain, :hippo_meta_dup_count, false) do
        # merge existing & new meta shape (in case caller passed a fresh meta)
        meta0 = meta || %{}

        # If already present, +1; otherwise seed at 2 (first duplicate → two copies total)
        dup2 =
          case fetch_dup_count(meta0) do
            nil -> 2
            n when is_integer(n) -> n + 1
            _ -> 2
          end

        %{ep | meta: Map.put(meta0, :dup_count, dup2)}
      else
        ep
      end
    end
  end

  def bump_dup_count(ep), do: ep

  @spec dup_count(map()) :: pos_integer()
  def dup_count(%{meta: m}) when is_map(m) do
    n =
      fetch_dup_count(m)
      |> case do
        nil ->
          1

        v when is_integer(v) ->
          v

        v when is_binary(v) ->
          case Integer.parse(v) do
            {i, _} -> i
            _ -> 1
          end

        _ ->
          1
      end

    n |> max(1) |> min(5)
  end

  def dup_count(_), do: 1

  # -------- internals --------

  defp fetch_dup_count(m) do
    Map.get(m, :dup_count) || Map.get(m, "dup_count") ||
      Map.get(m, :dup) || Map.get(m, "dup")
  end
end

