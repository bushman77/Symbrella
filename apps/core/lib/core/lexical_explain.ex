defmodule Core.LexicalExplain do
  @moduledoc """
  Build small, human-friendly glosses from the lexical cells already attached
  to a SemanticInput (`si.active_cells`).

  Goals:
    * Never hit the DB directly (Core stays above Brain/Db).
    * Work with plain maps or Ecto structs.
    * Stay short: 1–2 key words, each with a compact definition and synonyms.

  This is intentionally "debug-friendly": good enough for chat sidebars and
  for understanding how the brain is reading a sentence, without trying to be
  a full dictionary.
  """

  @default_max_items 2
  @default_max_syns 3
  @default_def_len 120

  @type si_like :: %{optional(:active_cells) => list()}

  @doc """
  Build a lexical explanation snippet from an SI-like map.

      iex> Core.LexicalExplain.from_si(si)
      "By the way, here’s how I’m reading a couple of words:\\n• picking (verb) — ..."

  Options:
    * `:max_items`   — how many distinct words to mention (default 2)
    * `:max_synonyms` — how many synonyms per word (default 3)
  """
  @spec from_si(si_like, keyword()) :: String.t()
  def from_si(%{} = si, opts \\ []) do
    cells = Map.get(si, :active_cells, [])
    from_cells(cells, opts)
  end

  @doc """
  Build a lexical snippet directly from a list of cells (Ecto structs or maps).
  """
  @spec from_cells(list(), keyword()) :: String.t()
  def from_cells(cells, opts \\ []) when is_list(cells) do
    max_items = Keyword.get(opts, :max_items, @default_max_items)

    cells
    |> Enum.group_by(&cell_norm/1)
    |> Enum.reject(fn {norm, _} ->
      norm in [nil, ""]
    end)
    |> Enum.map(fn {norm, group} -> pick_head(norm, group, opts) end)
    |> Enum.reject(&is_nil/1)
    |> Enum.take(max_items)
    |> Enum.map(&format_head/1)
    |> Enum.join("\n")
    |> case do
      "" ->
        ""

      lines ->
        "By the way, here’s how I’m reading a couple of words:\n" <> lines
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Head selection
  # ────────────────────────────────────────────────────────────────────────────

  # Decide which cell should represent a given `norm`.
  # Simple POS priority: verb > adj > adv > noun > everything else.
  defp pick_head(norm, group, _opts) do
    pos_priority = ["verb", "adjective", "adj", "adverb", "adv", "noun"]

    best_cell =
      Enum.reduce(group, nil, fn c, acc ->
        acc_rank = pos_rank(acc, pos_priority)
        cur_rank = pos_rank(c, pos_priority)

        if cur_rank < acc_rank, do: c, else: acc
      end)

    case best_cell do
      nil ->
        nil

      c ->
        %{
          norm: norm,
          word: cell_word(c) || norm,
          pos: cell_pos(c),
          definition: cell_def(c),
          synonyms: cell_synonyms(c)
        }
    end
  end

  defp pos_rank(nil, _prio), do: 999

  defp pos_rank(cell, prio) do
    pos =
      cell_pos(cell)
      |> to_string()
      |> String.downcase()

    case Enum.find_index(prio, &(&1 == pos)) do
      nil -> 500
      idx -> idx
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Formatting
  # ────────────────────────────────────────────────────────────────────────────

  defp format_head(%{word: word, pos: pos, definition: defn, synonyms: syns}) do
    label_pos =
      case pos do
        nil -> ""
        "" -> ""
        p -> " (" <> to_string(p) <> ")"
      end

    gloss = gloss(defn || "")

    syn_str =
      syns
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.take(@default_max_syns)
      |> case do
        [] -> ""
        list -> " — similar to " <> Enum.join(list, ", ")
      end

    "• #{word}#{label_pos} — #{gloss}#{syn_str}"
  end

  defp gloss(nil), do: ""
  defp gloss(""), do: ""

  defp gloss(str) when is_binary(str) do
    s =
      str
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    if String.length(s) <= @default_def_len do
      s
    else
      String.slice(s, 0, @default_def_len) <> "…"
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Cell accessors (work with both Ecto structs and plain maps)
  # ────────────────────────────────────────────────────────────────────────────

  defp cell_word(c) when is_map(c),
    do: Map.get(c, :word) || Map.get(c, "word")

  defp cell_word(_), do: nil

  defp cell_norm(c) when is_map(c) do
    cond do
      is_binary(Map.get(c, :norm)) -> Map.get(c, :norm)
      is_binary(Map.get(c, "norm")) -> Map.get(c, "norm")
      is_binary(Map.get(c, :id)) -> id_norm(Map.get(c, :id))
      is_binary(Map.get(c, "id")) -> id_norm(Map.get(c, "id"))
      true -> nil
    end
  end

  defp cell_norm(_), do: nil

  defp id_norm(nil), do: nil

  defp id_norm(id) when is_binary(id) do
    id
    |> String.split("|")
    |> List.first()
  end

  defp cell_pos(c) when is_map(c),
    do: Map.get(c, :pos) || Map.get(c, "pos")

  defp cell_pos(_), do: nil

  defp cell_def(c) when is_map(c),
    do: Map.get(c, :definition) || Map.get(c, "definition")

  defp cell_def(_), do: nil

  defp cell_synonyms(c) when is_map(c) do
    case Map.get(c, :synonyms) || Map.get(c, "synonyms") do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp cell_synonyms(_), do: []
end

