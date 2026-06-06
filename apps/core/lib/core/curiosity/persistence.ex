defmodule Core.Curiosity.Persistence do
  @moduledoc """
  Boundary persistence for successful curiosity enrichments.
  """

  require Logger

  @spec persist(list()) :: :ok
  def persist(entries) when is_list(entries) and entries != [] do
    write_as_episodes(entries)
    maybe_upsert_lexicon(entries)
    :ok
  end

  def persist(_), do: :ok

  defp write_as_episodes(entries) do
    ep_at = System.system_time(:millisecond)
    hippo = Module.concat([Brain, Hippocampus])

    for entry <- entries do
      episode = %{
        source: :curiosity,
        kind: :lexicon_seed,
        payload: entry,
        at: ep_at
      }

      safe_apply(hippo, :write, [episode])
    end

    :ok
  end

  defp maybe_upsert_lexicon(entries) do
    if Application.get_env(:core, :curiosity_allow_lexicon_writes, false) do
      core_lex = Module.concat([Core, Lexicon])
      db_lex = Module.concat([Db, Lexicon])

      cond do
        export?(core_lex, :upsert_fallbacks, 1) ->
          safe_apply(core_lex, :upsert_fallbacks, [entries])

        export?(db_lex, :bulk_upsert_senses, 1) ->
          safe_apply(db_lex, :bulk_upsert_senses, [entries])

        true ->
          Logger.warning("Curiosity: no Lexicon upsert function found - skipping persist")
          :ok
      end
    else
      :ok
    end
  end

  defp export?(mod, fun, arity),
    do: Code.ensure_loaded?(mod) and function_exported?(mod, fun, arity)

  defp safe_apply(mod, fun, args) do
    if export?(mod, fun, length(args)) do
      try do
        apply(mod, fun, args)
      rescue
        e ->
          Logger.warning(
            "Curiosity: #{inspect(mod)}.#{fun}/#{length(args)} failed: #{inspect(e)}"
          )

          :error
      catch
        _, _ -> :error
      end
    else
      :undef
    end
  end
end
