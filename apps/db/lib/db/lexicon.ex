defmodule Db.Lexicon do
  @moduledoc """
  Idempotent upserts for lexical items. Provides POS-variant explosion for tokens.
  """
  import Ecto.Query, warn: false
  alias Db
  alias Db.BrainCell

  @pos_inventory ~w(noun verb adj adv pron det adp num cconj sconj part propn punct intj sym x)

  @doc """
  Ensure every token's phrase is present for **all POS variants**, returning the
  %Db.BrainCell{} rows for those variants. If `only_mw: true`, restrict to multiword tokens.

  Options:
    - only_mw: boolean
    - type: "word" | "phrase" (auto if not set)
    - gram_function: default ""
    - pos_inventory: override default POS list
  """
  def ensure_pos_variants_from_tokens(tokens, opts \\ []) do
    only_mw?      = !!opts[:only_mw]
    pos_list      = opts[:pos_inventory] || @pos_inventory
    gram_function = opts[:gram_function] || ""

    # Match migration's timestamps() = :naive_datetime
    now_naive = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    selected =
      if only_mw?, do: Enum.filter(tokens, &match?(%{mw: true}, &1)), else: tokens

    rows =
      for %{phrase: phrase, mw: mw?} <- selected,
          pos <- pos_list do
        norm = normize(phrase)
        type = opts[:type] || if(mw?, do: "phrase", else: "word")
        id   = make_id(norm, pos, type, gram_function)

        %{
          id: id,
          word: phrase,
          norm: norm,
          pos: pos,
          type: type,
          gram_function: gram_function,
          status: "active",
          synonyms: [],
          antonyms: [],
          semantic_atoms: [],
          activation: 0.0,
          modulated_activation: 0.0,
          dopamine: 0.0,
          serotonin: 0.0,
          connections: [],
          inserted_at: now_naive,
          updated_at: now_naive
        }
      end
      |> Enum.uniq_by(& &1.id)

    _ =
      Db.insert_all(BrainCell, rows,
        on_conflict: :nothing,
        conflict_target:
          {:unsafe_fragment,
           " (norm, COALESCE(pos,''), COALESCE(type,''), COALESCE(gram_function,'')) "}
      )

    ids = Enum.map(rows, & &1.id)

    _ =
      from(b in BrainCell, where: b.id in ^ids)
      |> Db.update_all(set: [status: "active", updated_at: now_naive])

    Db.all(from b in BrainCell, where: b.id in ^ids)
  end

  defp normize(p),
    do: p |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  defp make_id(norm, pos, type, gf),
    do: [norm, pos || "", type || "", gf || ""] |> Enum.join("|")
end
