defmodule Db.Lexicon do
  @moduledoc "Bulk upserts of lexicon senses into brain_cells."
  alias Db, as: Repo
  alias Db.BrainCell

  @doc """
  Upsert a list of sense rows. Expected keys:
    :id, :word, :pos, :type, :definition, :example, :synonyms, :antonyms
  Missing arrays default to [].
  """
  @spec bulk_upsert_senses([map()]) :: :ok
  def bulk_upsert_senses(rows) when is_list(rows) do
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    rows1 =
      Enum.map(rows, fn r ->
        r
        |> Map.put_new(:status, "inactive")
        |> Map.put_new(:activation, 0.0)
        |> Map.put_new(:dopamine, 0.0)
        |> Map.put_new(:serotonin, 0.0)
        |> Map.put_new(:modulated_activation, 0.0)
        |> Map.put_new(:connections, [])
        |> Map.put_new(:semantic_atoms, [])
        |> Map.put_new(:synonyms, r[:synonyms] || [])
        |> Map.put_new(:antonyms, r[:antonyms] || [])
        |> Map.put_new(:inserted_at, now)
        |> Map.put_new(:updated_at, now)
      end)

    Repo.insert_all(
      BrainCell,
      rows1,
      conflict_target: :id,
      on_conflict:
        {:replace, [:definition, :example, :pos, :synonyms, :antonyms, :updated_at]}
    )

    :ok
  end
end

