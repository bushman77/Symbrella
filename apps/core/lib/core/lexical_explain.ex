defmodule Core.LexicalExplain do
  @moduledoc """
  Build small, human-friendly glosses from the lexical cells already attached
  to a SemanticInput (`si.active_cells`).

  Goals:
    * Never hit the DB directly (Core stays above Brain/Db).
    * Work with plain maps or Ecto structs.
    * Stay short: 1–2 key words, each with a compact definition and (optional) synonyms.

  This is intentionally "debug-friendly": good enough for chat sidebars and
  for understanding how the brain is reading a sentence, without trying to be
  a full dictionary.
  """

  @default_max_items 2
  @default_max_syns 3
  @default_def_len 120
  @default_min_syn_len 4

  # Suppress function-words & chat glue from explain tail.
  @stopwords MapSet.new(~w[
    a an the and or but if then else so
    i me my mine you your yours u we us our ours they them their theirs he him his she her hers it its
    is am are was were be been being
    do does did doing done
    can could may might must should would will wont
    to of in on at for from with into onto over under up down out off
    what who whom whose which when where why how
    this that these those
    here there
    please ok okay yeah yep nah no yes
    symbrella
  ])

  @type si_like :: %{
          optional(:active_cells) => list(),
          optional(:tokens) => list(),
          optional(:sentence) => String.t(),
          optional(:si_sentence) => String.t(),
          optional(:text) => String.t(),
          optional(:keyword) => any
        }

  @doc """
  Build a lexical explanation snippet from an SI-like map.

      iex> Core.LexicalExplain.from_si(si)
      "By the way, here’s how I’m reading a couple of words:\\n• picking (verb) — ..."

  Options:
    * `:max_items`     — how many distinct words to mention (default 2)
    * `:max_synonyms`  — how many synonyms per word (default 3)
  """
  @spec from_si(si_like, keyword()) :: String.t()
  def from_si(%{} = si, opts \\ []) do
    cells = Map.get(si, :active_cells, [])

    sentence =
      Map.get(si, :sentence) ||
        Map.get(si, :si_sentence) ||
        Map.get(si, :text) ||
        to_string(Map.get(si, :keyword) || "")

    allowed_norms =
      si
      |> Map.get(:tokens, [])
      |> allowed_norms_from_tokens()

    opts =
      opts
      |> Keyword.put_new(:sentence, sentence)
      |> Keyword.put_new(:allowed_norms, allowed_norms)

    from_cells(cells, opts)
  end

  @doc """
  Build a lexical snippet directly from a list of cells (Ecto structs or maps).

  Extra opts supported:
    * `:sentence`      — if present, only keep heads that match whole words in the sentence
    * `:allowed_norms` — MapSet/list of norms allowed (typically derived from si.tokens)
  """
  @spec from_cells(list(), keyword()) :: String.t()
  def from_cells(cells, opts \\ []) when is_list(cells) do
    max_items = Keyword.get(opts, :max_items, @default_max_items)
    max_syns = Keyword.get(opts, :max_synonyms, @default_max_syns)
    sentence = Keyword.get(opts, :sentence, nil)
    allowed_norms = Keyword.get(opts, :allowed_norms, MapSet.new()) |> to_norm_set()

    cells
    |> Enum.filter(&keep_cell?(&1, sentence, allowed_norms))
    |> Enum.group_by(&cell_norm_key/1)
    |> Enum.reject(fn {norm, _} -> norm in [nil, ""] end)
    |> Enum.map(fn {norm, group} -> pick_head(norm, group) end)
    |> Enum.reject(&is_nil/1)
    |> Enum.sort_by(&head_sort_key(&1, sentence))
    |> Enum.take(max_items)
    |> Enum.map(&format_head(&1, max_syns, sentence))
    |> Enum.join("\n")
    |> case do
      "" -> ""
      lines -> "By the way, here’s how I’m reading a couple of words:\n" <> lines
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Filtering
  # ────────────────────────────────────────────────────────────────────────────

  defp keep_cell?(c, sentence, allowed_norms) do
    norm = cell_norm_key(c)
    word = norm_text(cell_word(c) || "")

    ok? =
      cond do
        norm in [nil, ""] -> false
        not single_word?(norm) -> false
        MapSet.member?(@stopwords, norm) -> false
        String.length(norm) < 2 -> false
        norm =~ ~r/^\d+$/u -> false
        norm =~ ~r/^[[:punct:]]+$/u -> false
        word != "" and MapSet.member?(@stopwords, word) -> false
        true -> true
      end

    ok? =
      ok? and
        if MapSet.size(allowed_norms) > 0 do
          MapSet.member?(allowed_norms, norm) or
            (word != "" and MapSet.member?(allowed_norms, word))
        else
          true
        end

    ok? and
      if is_binary(sentence) and sentence != "" do
        whole_word_in_sentence?(sentence, norm) or
          (word != "" and whole_word_in_sentence?(sentence, word))
      else
        true
      end
  end

  defp single_word?(s) when is_binary(s), do: not Regex.match?(~r/\s/u, s)
  defp single_word?(_), do: false

  defp whole_word_in_sentence?(sentence, word) when is_binary(sentence) and is_binary(word) do
    w = String.trim(word)

    if w == "" do
      false
    else
      # Treat "word boundaries" as transitions around letters/digits/underscore.
      # This blocks substring matches like "eve" inside "evening".
      rx = ~r/(^|[^[:alnum:]_])#{Regex.escape(w)}([^[:alnum:]_]|$)/iu
      Regex.match?(rx, sentence)
    end
  end

  defp whole_word_in_sentence?(_, _), do: false

  defp head_sort_key(%{word: w, norm: n}, sentence) do
    idx = find_first_index(sentence, w || n)
    {idx, String.length(to_string(w || n))}
  end

  defp find_first_index(sentence, word) when is_binary(sentence) and is_binary(word) do
    w = String.trim(word)

    if w == "" do
      9_999_999
    else
      rx = ~r/(^|[^[:alnum:]_])#{Regex.escape(w)}([^[:alnum:]_]|$)/iu

      case Regex.run(rx, sentence, return: :index) do
        [match_idx | _] -> elem(match_idx, 0)
        _ -> 9_999_999
      end
    end
  end

  defp find_first_index(_, _), do: 9_999_999

  # ────────────────────────────────────────────────────────────────────────────
  # Head selection (POS normalization)
  # ────────────────────────────────────────────────────────────────────────────

  # Prefer common human readings: adjective/noun > verb > adverb.
  # Also normalize WordNet-ish tags like "a"/"s".
  defp pick_head(norm, group) do
    best_cell =
      Enum.reduce(group, nil, fn c, acc ->
        if pos_rank(c) < pos_rank(acc), do: c, else: acc
      end)

    case best_cell do
      nil ->
        nil

      c ->
        %{
          norm: norm,
          word: cell_word(c) || norm,
          pos: pos_label(pos_class(c)),
          definition: cell_def(c),
          synonyms: cell_synonyms(c)
        }
    end
  end

  defp pos_rank(nil), do: 999

  defp pos_rank(cell) do
    case pos_class(cell) do
      :adjective -> 0
      :noun -> 1
      :verb -> 2
      :adverb -> 3
      _ -> 10
    end
  end

  defp pos_label(:adjective), do: "adj"
  defp pos_label(:adverb), do: "adv"
  defp pos_label(:noun), do: "noun"
  defp pos_label(:verb), do: "verb"
  defp pos_label(_), do: nil

  defp pos_class(cell) do
    raw = pos_raw(cell)

    cond do
      raw in ["adj", "adjective", "a", "s", "adj_sat", "adjective satellite"] ->
        :adjective

      String.starts_with?(raw, "adj") ->
        :adjective

      raw in ["n", "noun"] or String.starts_with?(raw, "noun") ->
        :noun

      raw in ["v", "verb"] or String.starts_with?(raw, "verb") ->
        :verb

      raw in ["r", "adv", "adverb"] or String.starts_with?(raw, "adv") or
          String.starts_with?(raw, "adverb") ->
        :adverb

      true ->
        :other
    end
  end

  defp pos_raw(cell) do
    (cell_pos(cell) || id_pos(cell) || "")
    |> to_string()
    |> String.downcase()
    |> String.trim()
  end

  defp id_pos(c) when is_map(c) do
    id = Map.get(c, :id) || Map.get(c, "id")

    case id do
      id when is_binary(id) ->
        case String.split(id, "|") do
          [_norm, pos | _] -> pos
          _ -> nil
        end

      _ ->
        nil
    end
  end

  defp id_pos(_), do: nil

  # ────────────────────────────────────────────────────────────────────────────
  # Formatting
  # ────────────────────────────────────────────────────────────────────────────

  defp format_head(%{word: word, pos: pos, definition: defn, synonyms: syns}, max_syns, sentence) do
    label_pos =
      case pos do
        nil -> ""
        "" -> ""
        p -> " (" <> to_string(p) <> ")"
      end

    gloss = gloss(defn || "")

    syn_str =
      syns
      |> normalize_synonyms(sentence)
      |> Enum.take(max_syns)
      |> case do
        [] -> ""
        list -> " — similar to " <> Enum.join(list, ", ")
      end

    "• #{word}#{label_pos} — #{gloss}#{syn_str}"
  end

  defp normalize_synonyms(syns, sentence) do
    syns
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&norm_text/1)
    |> Enum.uniq()
    |> Enum.reject(&MapSet.member?(@stopwords, &1))
    |> Enum.reject(&Regex.match?(~r/\s/u, &1))
    |> Enum.reject(&(String.length(&1) < @default_min_syn_len))
    |> Enum.reject(fn s ->
      # If the synonym is literally present as a whole word in the sentence, it’s probably not a “synonym” here.
      is_binary(sentence) and sentence != "" and whole_word_in_sentence?(sentence, s)
    end)
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
  # Token whitelist (only explain what the user actually wrote)
  # ────────────────────────────────────────────────────────────────────────────

  defp allowed_norms_from_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.flat_map(fn
      %{phrase: p} = tok ->
        n = Map.get(tok, :n, 1)
        mw = Map.get(tok, :mw, false)

        if n == 1 and mw in [false, nil] do
          [norm_text(p)]
        else
          []
        end

      p when is_binary(p) ->
        [norm_text(p)]

      _ ->
        []
    end)
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.reject(&MapSet.member?(@stopwords, &1))
    |> Enum.reject(&Regex.match?(~r/\s/u, &1))
    |> MapSet.new()
  end

  defp allowed_norms_from_tokens(_), do: MapSet.new()

  defp to_norm_set(%MapSet{} = s), do: s
  defp to_norm_set(list) when is_list(list), do: MapSet.new(Enum.map(list, &norm_text/1))
  defp to_norm_set(_), do: MapSet.new()

  # ────────────────────────────────────────────────────────────────────────────
  # Cell accessors (work with both Ecto structs and plain maps)
  # ────────────────────────────────────────────────────────────────────────────

  defp cell_word(c) when is_map(c),
    do: Map.get(c, :word) || Map.get(c, "word")

  defp cell_word(_), do: nil

  # Group/compare on a normalized key to avoid EVE/eve weirdness.
  defp cell_norm_key(c) do
    c
    |> cell_norm()
    |> norm_text()
    |> case do
      "" -> nil
      v -> v
    end
  end

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

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm_text(v) do
    v
    |> Kernel.to_string()
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end
end
