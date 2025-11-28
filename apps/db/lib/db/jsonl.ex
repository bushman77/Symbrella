# apps/db/lib/db/jsonl.ex
defmodule Db.JSONL do
  @moduledoc """
  Streaming reader for JSONL (one JSON object per line), tuned for very large
  Kaikki/Wiktextract dumps.

  Key points:
  - Streams by :line (NOT fixed-size chunks) so each item is one JSON line.
  - Uses a cheap substring prefilter before JSON-decoding.
  - Never creates atoms from file content (avoids atom leaks).
  - Can return entries, or just senses (flattened).

  This is for *inspecting* the file before building an importer/index.
  """

  @default_filename "english.jsonl"

  @type row :: map()

  @type fetch_opt ::
          {:path, String.t()}
          | {:field, String.t() | atom()}
          | {:limit, non_neg_integer() | :infinity}
          | {:decode?, boolean()}
          | {:on_decode_error, :skip | :raise | :count}

  @type populate_opt ::
          fetch_opt()
          | {:type, String.t() | nil}
          | {:status, String.t() | nil}
          | {:pos_fallback, String.t()} # default "phrase"
          | {:include_semantic_atoms?, boolean()}
          | {:id_start, non_neg_integer()} # default 1 (set 0 if you want good|noun|0 style)

  @spec default_paths() :: [String.t()]
  def default_paths do
    build_priv =
      case :code.priv_dir(:db) do
        dir when is_list(dir) or is_binary(dir) -> dir |> to_string() |> Path.join(@default_filename)
        _ -> nil
      end

    source_priv = Path.expand("apps/db/priv/#{@default_filename}")

    [build_priv, source_priv]
    |> Enum.reject(&is_nil/1)
  end

  @spec default_path() :: String.t()
  def default_path do
    Enum.find(default_paths(), &File.exists?/1) || List.first(default_paths())
  end

  @doc """
  Read the first N JSONL lines (raw strings), useful to inspect shape quickly.

  Tip: if you want to decode, do:
      {:ok, [l|_]} = Db.JSONL.head(1)
      Jason.decode!(l)
  """
  @spec head(non_neg_integer(), keyword()) :: {:ok, [binary()]} | {:error, any()}
  def head(n \\ 3, opts \\ []) when is_integer(n) and n >= 0 do
    path = resolve_path(opts)

    if File.exists?(path) do
      lines =
        path
        |> stream_lines()
        |> Enum.take(n)

      {:ok, lines}
    else
      {:error, {:no_such_file, path}}
    end
  end

  @doc """
  Stream (lazy) all decoded entries where `field == word`.

  Options:
    * :path  (default: auto build priv or apps/db/priv)
    * :field (default: "word")
    * :decode? (default true) — if false, yields raw JSON lines
    * :on_decode_error (default :skip) — :skip | :raise | :count
      - :count yields `{:bad_json, line}` items instead of skipping, for investigation
  """
  @spec fetch_word_stream(String.t(), [fetch_opt()]) :: {:ok, Enumerable.t()} | {:error, any()}
  def fetch_word_stream(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)
    if word == "", do: {:error, :empty_word}, else: do_fetch_stream(word, opts)
  end

  @doc """
  Fetch all entries where `field == word` into a list.

  Options:
    * :limit (default :infinity)
    * all options from fetch_word_stream/2
  """
  @spec fetch_word(String.t(), [fetch_opt()]) :: {:ok, list(row() | binary())} | {:error, any()}
  def fetch_word(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)

    if word == "" do
      {:error, :empty_word}
    else
      limit = Keyword.get(opts, :limit, :infinity)

      with {:ok, stream} <- do_fetch_stream(word, opts) do
        rows =
          stream
          |> take_limit(limit)
          |> Enum.to_list()

        {:ok, rows}
      end
    end
  end

  @doc """
  Fetch *just* the senses for a given lemma, flattened across all matching entries.

  Each returned sense is the original sense map (string keys) plus `_meta` with
  entry context:

      %{
        ...sense fields...,
        "_meta" => %{"word" => "good", "pos" => "adj", "etymology_number" => 1}
      }

  Options:
    * :path, :limit, :on_decode_error (as above)
  """
  @spec fetch_senses(String.t(), keyword()) :: {:ok, list(map())} | {:error, any()}
  def fetch_senses(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)

    if word == "" do
      {:error, :empty_word}
    else
      limit = Keyword.get(opts, :limit, :infinity)
      opts = Keyword.put(opts, :decode?, true)

      with {:ok, stream} <- do_fetch_stream(word, opts) do
        senses =
          stream
          |> Stream.flat_map(fn
            {:bad_json, _line} ->
              []

            %{} = entry ->
              word0 = Map.get(entry, "word")
              pos = Map.get(entry, "pos")
              ety = Map.get(entry, "etymology_number")

              meta = %{
                "word" => word0,
                "pos" => pos,
                "etymology_number" => ety
              }

              entry
              |> Map.get("senses", [])
              |> List.wrap()
              |> Enum.map(fn s ->
                s
                |> ensure_map()
                |> Map.put("_meta", meta)
              end)

            _other ->
              []
          end)
          |> take_limit(limit)
          |> Enum.to_list()

        {:ok, senses}
      end
    end
  end

  @doc """
  Turn every sense for `word` into a `%Db.BrainCell{}`.

  This does **not** insert into Postgres — it only returns structs so you can inspect
  and then decide how/when to persist (via changeset + Db.insert_all/2 etc.).

  Options:
    * all options from fetch_senses/2 (path/limit/on_decode_error)
    * :type  (default "wiktextract")
    * :status (default "inactive")
    * :pos_fallback (default "phrase")
    * :include_semantic_atoms? (default true)
    * :id_start (default 1) — set 0 if you want good|noun|0 style
  """
  @spec populate_structs(String.t(), [populate_opt()]) :: {:ok, list(Db.BrainCell.t())} | {:error, any()}
  def populate_structs(word, opts \\ []) when is_binary(word) do
    with {:ok, senses} <- fetch_senses(word, opts) do
      {:ok, populate_structs_from_senses(senses, opts)}
    end
  end

  @doc """
  Map an already-loaded senses list (from fetch_senses/2) into BrainCell structs.

  IDs use numeric indexing: "word|pos|N" where N increments per {word,pos}.
  """
  @spec populate_structs_from_senses([map()], [populate_opt()]) :: list(Db.BrainCell.t())
  def populate_structs_from_senses(senses, opts \\ []) when is_list(senses) do
    default_type = Keyword.get(opts, :type, "wiktextract")
    default_status = Keyword.get(opts, :status, "inactive")
    pos_fallback = Keyword.get(opts, :pos_fallback, "phrase")
    include_atoms? = Keyword.get(opts, :include_semantic_atoms?, true)
    id_start = Keyword.get(opts, :id_start, 1)

    {cells, _counters} =
      Enum.map_reduce(senses, %{}, fn s, counters ->
        meta = Map.get(s, "_meta", %{})
        raw_word = Map.get(meta, "word") || Map.get(s, "word") || ""
        word = normalize_word(raw_word)

        raw_pos = Map.get(meta, "pos") || Map.get(s, "pos") || ""
        pos = canonical_pos(raw_pos, pos_fallback)

        key = {word, pos}
        idx = Map.get(counters, key, id_start - 1) + 1
        counters = Map.put(counters, key, idx)

        definition = pick_definition(s)
        example = pick_example(s)

        synonyms = flatten_term_list(Map.get(s, "synonyms"))
        antonyms = flatten_term_list(Map.get(s, "antonyms"))

        gf = gram_function_from_tags(pos, Map.get(s, "tags"), Map.get(s, "raw_tags"))

        semantic_atoms =
          if include_atoms? do
            atoms = semantic_atoms_from_sense(s, raw_pos)

            atoms =
              case Map.get(meta, "etymology_number") do
                n when is_integer(n) -> Enum.uniq(["ety:#{n}" | atoms])
                n when is_binary(n) and n != "" -> Enum.uniq(["ety:#{n}" | atoms])
                _ -> atoms
              end

            atoms
          else
            []
          end

        cell =
          %Db.BrainCell{
            id: "#{word}|#{pos}|#{idx}",
            word: word,
            norm: word,
            pos: pos,
            type: default_type,
            status: default_status,
            definition: definition,
            example: example,
            synonyms: synonyms,
            antonyms: antonyms,
            gram_function: gf,
            semantic_atoms: semantic_atoms,
            activation: 0.0,
            modulated_activation: 0.0,
            dopamine: 0.0,
            serotonin: 0.0,
            connections: []
          }

        {cell, counters}
      end)

    cells
  end

  @doc """
  Summarize decoded entries: top keys + pos distribution + senses count totals.
  """
  @spec summarize_entries([row()]) :: map()
  def summarize_entries(entries) when is_list(entries) do
    keys =
      entries
      |> Enum.flat_map(fn e -> Map.keys(e) end)
      |> Enum.map(&to_string/1)
      |> Enum.frequencies()
      |> Enum.sort_by(fn {_k, v} -> -v end)
      |> Enum.take(30)

    pos =
      entries
      |> Enum.map(&Map.get(&1, "pos"))
      |> Enum.reject(&is_nil/1)
      |> Enum.frequencies()

    senses_total =
      entries
      |> Enum.map(fn e -> e |> Map.get("senses", []) |> List.wrap() |> length() end)
      |> Enum.sum()

    %{
      entries: length(entries),
      pos: pos,
      senses_total: senses_total,
      key_frequencies_top30: keys
    }
  end

  @doc """
  Summarize flattened senses: key frequency + sample tags/gloss presence.
  """
  @spec summarize_senses([map()]) :: map()
  def summarize_senses(senses) when is_list(senses) do
    keys =
      senses
      |> Enum.flat_map(&Map.keys/1)
      |> Enum.map(&to_string/1)
      |> Enum.frequencies()
      |> Enum.sort_by(fn {_k, v} -> -v end)
      |> Enum.take(30)

    has_gloss? =
      Enum.count(senses, fn s ->
        (s["glosses"] |> List.wrap() |> length() > 0) or
          (s["raw_glosses"] |> List.wrap() |> length() > 0)
      end)

    has_examples? =
      Enum.count(senses, fn s ->
        s["examples"] |> List.wrap() |> length() > 0
      end)

    %{
      senses: length(senses),
      with_gloss: has_gloss?,
      with_examples: has_examples?,
      key_frequencies_top30: keys
    }
  end

  # ───────────────────────── internals ─────────────────────────

  defp do_fetch_stream(word, opts) do
    path = resolve_path(opts)
    field = opts |> Keyword.get(:field, "word") |> to_string()
    decode? = Keyword.get(opts, :decode?, true)
    on_err = Keyword.get(opts, :on_decode_error, :skip)

    unless File.exists?(path) do
      {:error, {:no_such_file, path}}
    else
      if decode? and not jason_available?() do
        {:error, :jason_not_available}
      else
        {needle1, needle2} = needles(field, word)

        stream =
          path
          |> stream_lines()
          |> Stream.filter(fn line ->
            String.contains?(line, needle1) or String.contains?(line, needle2)
          end)
          |> Stream.flat_map(fn line ->
            if decode? do
              case Jason.decode(line) do
                {:ok, %{} = m} ->
                  if Map.get(m, field) == word, do: [m], else: []

                {:error, _} ->
                  case on_err do
                    :raise -> raise "JSON decode failed for line beginning: #{String.slice(line, 0, 200)}"
                    :count -> [{:bad_json, line}]
                    :skip -> []
                  end

                _ ->
                  []
              end
            else
              [line]
            end
          end)

        {:ok, stream}
      end
    end
  end

  defp stream_lines(path) do
    path
    |> File.stream!([], :line)
    |> Stream.map(&trim_newline/1)
    |> Stream.reject(&(&1 == ""))
  end

  defp trim_newline(line) do
    line
    |> String.trim_trailing("\n")
    |> String.trim_trailing("\r")
  end

  defp resolve_path(opts) do
    case Keyword.get(opts, :path) do
      nil -> default_path()
      p -> Path.expand(p)
    end
  end

  defp take_limit(stream, :infinity), do: stream
  defp take_limit(stream, n) when is_integer(n) and n > 0, do: Stream.take(stream, n)
  defp take_limit(_stream, 0), do: Stream.take([], 0)

  defp jason_available? do
    Code.ensure_loaded?(Jason) and function_exported?(Jason, :decode, 1) and function_exported?(Jason, :encode, 1)
  end

  defp needles(field, word) do
    encoded = encode_json_string(word)
    {~s("#{field}":#{encoded}), ~s("#{field}": #{encoded})}
  end

  defp encode_json_string(word) do
    case Jason.encode(word) do
      {:ok, s} -> s
      _ -> ~s("#{escape_json(word)}")
    end
  end

  defp escape_json(s) do
    s
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\r")
    |> String.replace("\t", "\\t")
  end

  defp ensure_map(%{} = m), do: m
  defp ensure_map(_), do: %{}

  # ───────────────────────── sense → braincell helpers ─────────────────────────

  defp normalize_word(nil), do: ""
  defp normalize_word(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")

  defp normalize_word(other), do: other |> to_string() |> normalize_word()

  defp canonical_pos(raw, fallback) do
    raw =
      raw
      |> to_string()
      |> String.trim()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")

    case raw do
      "noun" -> "noun"
      "n" -> "noun"
      "verb" -> "verb"
      "v" -> "verb"
      "adjective" -> "adjective"
      "adj" -> "adjective"
      "a" -> "adjective"
      "adjective satellite" -> "adjective"
      "adj_sat" -> "adjective"
      "adverb" -> "adverb"
      "adv" -> "adverb"
      "r" -> "adverb"
      "interjection" -> "interjection"
      "interj" -> "interjection"
      "intj" -> "interjection"
      "proper noun" -> "proper_noun"
      "proper_noun" -> "proper_noun"
      "propn" -> "proper_noun"
      "pronoun" -> "pronoun"
      "pron" -> "pronoun"
      "determiner" -> "determiner"
      "det" -> "determiner"
      "article" -> "determiner"
      "preposition" -> "preposition"
      "prep" -> "preposition"
      "adposition" -> "preposition"
      "adp" -> "preposition"
      "conjunction" -> "conjunction"
      "conj" -> "conjunction"
      "numeral" -> "numeral"
      "num" -> "numeral"
      "particle" -> "particle"
      "part" -> "particle"
      "phrase" -> "phrase"
      _ -> fallback
    end
  end

  defp pick_definition(sense) do
    glosses = sense |> Map.get("glosses") |> List.wrap()
    raw = sense |> Map.get("raw_glosses") |> List.wrap()

    (glosses ++ raw)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> case do
      [] -> nil
      [one] -> one
      many -> Enum.join(many, " / ")
    end
  end

  defp pick_example(sense) do
    sense
    |> Map.get("examples")
    |> List.wrap()
    |> Enum.find_value(fn
      %{"text" => t} when is_binary(t) -> t
      %{"example" => t} when is_binary(t) -> t
      t when is_binary(t) -> t
      _ -> nil
    end)
    |> case do
      nil -> nil
      s -> String.trim(s)
    end
  end

  defp flatten_term_list(list) do
    list
    |> List.wrap()
    |> Enum.map(fn
      %{"word" => w} -> w
      %{"term" => w} -> w
      %{"value" => w} -> w
      w when is_binary(w) -> w
      _ -> nil
    end)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp gram_function_from_tags(pos, tags, raw_tags) do
    allow =
      case pos do
        "noun" -> MapSet.new(~w(countable uncountable plural-only usually plural))
        "verb" -> MapSet.new(~w(transitive intransitive ditransitive ambitransitive copular auxiliary modal ergative impersonal))
        "adjective" -> MapSet.new(~w(attributive-only predicative-only postpositive comparative-only))
        _ -> MapSet.new()
      end

    if MapSet.size(allow) == 0 do
      []
    else
      normalized =
        (List.wrap(tags) ++ List.wrap(raw_tags))
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&normalize_tag/1)
        |> Enum.map(&gf_canonicalize/1)
        |> Enum.uniq()

      normalized =
        if pos == "noun" and Enum.member?(normalized, "usually") and Enum.member?(normalized, "plural") do
          Enum.uniq(normalized ++ ["usually plural"])
        else
          normalized
        end

      normalized
      |> Enum.filter(&MapSet.member?(allow, &1))
      |> Enum.uniq()
    end
  end

  defp normalize_tag(t) do
    t
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp gf_canonicalize("plural only"), do: "plural-only"
  defp gf_canonicalize("predicative only"), do: "predicative-only"
  defp gf_canonicalize("attributive only"), do: "attributive-only"
  defp gf_canonicalize("comparative only"), do: "comparative-only"
  defp gf_canonicalize(other), do: other

  defp semantic_atoms_from_sense(sense, raw_pos) do
    cats =
      sense
      |> Map.get("categories")
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&("cat:" <> normalize_atom(&1)))

    tags =
      sense
      |> Map.get("tags")
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&("tag:" <> normalize_atom(&1)))

    rawt =
      sense
      |> Map.get("raw_tags")
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&("rtag:" <> normalize_atom(&1)))

    qual =
      case Map.get(sense, "qualifier") do
        q when is_binary(q) and q != "" -> ["qual:" <> normalize_atom(q)]
        _ -> []
      end

    pos_atom =
      case to_string(raw_pos) do
        "" -> []
        p -> ["pos_raw:" <> normalize_atom(p)]
      end

    (cats ++ tags ++ rawt ++ qual ++ pos_atom)
    |> Enum.uniq()
  end

  defp normalize_atom(s) do
    s
    |> to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end
end

