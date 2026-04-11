defmodule Core.Brain.Episodes do
  @moduledoc """
  Episodic recall attachment stage for `Core.SemanticInput`.
  """

  alias Core.Brain.Runtime

  @spec attach(map(), keyword()) :: map()
  def attach(%{} = si, opts) when is_list(opts) do
    enabled? =
      case Keyword.get(opts, :episodes, nil) do
        true -> true
        false -> false
        _ -> Application.get_env(:brain, :episodes_mode, :on) != :off
      end

    cond do
      not enabled? ->
        si

      not Runtime.pid_alive?(Brain.Hippocampus) ->
        si

      true ->
        pass =
          []
          |> Runtime.put_if_present(:source, Keyword.get(opts, :recall_source))
          |> Runtime.put_if_present(:embedding, Keyword.get(opts, :episode_embedding))

        si2 =
          case Runtime.apply_if_exported(Brain.Hippocampus, :attach_episodes, [si, pass], si) do
            %{} = out -> out
            _ -> si
          end

        eps = get_in(si2, [:evidence, :episodes]) || []

        Runtime.emit([:brain, :core, :episodes_attached], %{count: length(eps)}, %{
          mode: :pre_lifg
        })

        si2
    end
  end

  def attach(si, _opts), do: si
end
