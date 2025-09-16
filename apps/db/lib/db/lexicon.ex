defmodule Db.Lexicon do
  @moduledoc "Idempotent upserts for lexical items."
  import Ecto.Query, warn: false
  alias Db.{Repo, BrainCell}

  # Normalize exactly like the migration’s UPDATE
  defp normize(phrase) do
    phrase
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  # Policy for your string PK: stable + readable
  # Feel free to switch to Base.encode16(:crypto.hash(:sha256, key)) if preferred.
  defp make_id(%{norm: norm, pos: pos, type: type, gram_function: gf}) do
    [norm, pos || "", type || "", gf || ""]
    |> Enum.join("|")
  end

  @doc "Ensure a single lexical item exists; returns %Db.BrainCell{}."
  def ensure_one(phrase, opts \\ []) do
    attrs = %{
      word: phrase,
      norm: normize(phrase),
      pos: opts[:pos],
      type: opts[:type],
      gram_function: opts[:gram_function],
      status: opts[:status] || "inactive",
      token_id: opts[:token_id]
    }

    id = make_id(attrs)

    _ =
      %BrainCell{}
      |> BrainCell.changeset(Map.put(attrs, :id, id))
      |> Repo.insert(
        on_conflict: :nothing,
        conflict_target: {:unsafe_fragment, " (norm, COALESCE(pos,''), COALESCE(type,''), COALESCE(gram_function,'')) "}
      )

    Repo.get(BrainCell, id) ||
      Repo.get_by(BrainCell,
        norm: attrs.norm,
        pos: attrs.pos,
        type: attrs.type,
        gram_function: attrs.gram_function
      )
  end

  @doc "Ensure many at once (strings or %{phrase: ...} tokens)."
  def ensure_many(items, opts \\ []) do
    now = DateTime.utc_now()

    rows =
      items
      |> Enum.map(fn
        %{phrase: p} -> p
        p when is_binary(p) -> p
      end)
      |> Enum.map(fn p ->
        attrs = %{
          word: p,
          norm: normize(p),
          pos: opts[:pos],
          type: opts[:type],
          gram_function: opts[:gram_function],
          status: opts[:status] || "inactive",
          token_id: opts[:token_id]
        }

        Map.merge(attrs, %{
          id: make_id(attrs),
          inserted_at: now,
          updated_at: now
        })
      end)
      |> Enum.uniq_by(& &1.id)

    _ =
      Repo.insert_all(BrainCell, rows,
        on_conflict: :nothing,
        conflict_target: {:unsafe_fragment, " (norm, COALESCE(pos,''), COALESCE(type,''), COALESCE(gram_function,'')) "}
      )

    ids = Enum.map(rows, & &1.id)
    Repo.all(from b in BrainCell, where: b.id in ^ids)
  end
end

