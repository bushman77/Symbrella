defmodule Brain.Hippocampus.Dup do
  @moduledoc """
  De-duplication knobs for Hippocampus episodes.
  """

  @mix_env (Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env()) || :prod
  @test_env @mix_env == :test

  @doc """
  If `:hippo_meta_dup_count` is true (and not test), increment `:dup_count` in meta.
  Otherwise, return the episode unchanged.
  """
  @spec bump_dup_count(%{meta: map()}) :: map()
  def bump_dup_count(%{meta: meta} = ep) do
    if @test_env do
      ep
    else
      if Application.get_env(:brain, :hippo_meta_dup_count, false) do
        meta2 = (meta || %{}) |> Map.update(:dup_count, 1, &(&1 + 1))
        %{ep | meta: meta2}
      else
        ep
      end
    end
  end

  def bump_dup_count(ep), do: ep

  @spec dup_count(map()) :: pos_integer()
  def dup_count(%{meta: m}) when is_map(m), do: Map.get(m, :dup_count, 1)
  def dup_count(_), do: 1
end

