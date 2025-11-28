# apps/db/lib/db/jsonl.ex
defmodule Db.JSONL do
  @moduledoc false

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
          | {:pos_fallback, String.t()}
          | {:include_semantic_atoms?, boolean()}
          | {:fill_missing_relations?, boolean()}

  @spec default_paths() :: [String.t()]
  def default_paths do
    build_priv =
      case :code.priv_dir(:db) do
        dir when is_list(dir) or is_binary(dir) ->
          dir |> to_string() |> Path.join(@default_filename)

        _ ->
          nil
      end

    source_priv = Path.expand("apps/db/priv/#{@default_filename}")

    [build_priv, source_priv]
    |> Enum.reject(&is_nil/1)
  end

  @spec default_path() :: String.t()
  def default_path do
    Enum.find(default_paths(), &File.exists?/1) || List.first(default_paths())
  end

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

  @spec fetch_word_stream(String.t(), [fetch_opt()]) :: {:ok, Enumerable.t()} | {:error, any()}
  def fetch_word_stream(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)
    if word == "", do: {:error, :empty_word}, else: do_fetch_stream(word, opts)
  end

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
              meta =
                %{
                  "word" => Map.get(entry, "word"),
                  "pos" => Map.get(entry, "pos"),
                  "etymology_number" => Map.get(entry, "etymology_number"),
                  "entry_synonyms" => flatten_term_list(Map.get(entry, "synonyms")),
                  "entry_antonyms" => flatten_term_list(Map.get(entry, "antonyms"))
                }
                |> Enum.reject(fn {_k, v} -> is_nil(v) end)
                |> Map.new()

              entry
              |> Map.get("senses", [])
              |> List.wrap()
              |> Enum.map(fn s -> s |> ensure_map() |> Map.put("_meta", meta) end)

            _other ->
              []
          end)
          |> take_limit(limit)
          |> Enum.to_list()

        {:ok, senses}
      end
    end
  end

  @spec populate_structs(String.t(), [populate_opt()]) ::
          {:ok, list(Db.BrainCell.t())} | {:error, any()}
  def populate_structs(word, opts \\ []) when is_binary(word) do
    with {:ok, senses} <- fetch_senses(word, opts) do
      {:ok, populate_structs_from_senses(senses, opts)}
    end
  end

  @spec populate_structs_from_senses([map()], [populate_opt()]) :: list(Db.BrainCell.t())
  def populate_structs_from_senses(senses, opts \\ []) when is_list(senses) do
    default_type = Keyword.get(opts, :type, "wiktextract")
    default_status = Keyword.get(opts, :status, "inactive")
    pos_fallback = Keyword.get(opts, :pos_fallback, "phrase")
    include_atoms? = Keyword.get(opts, :include_semantic_atoms?, true)
    fill_missing? = Keyword.get(opts, :fill_missing_relations?, true)

    # Build a {word,pos} relations pool so we can optionally backfill missing rels.
    rels_by_key = build_relations_index(senses, pos_fallback)

    {cells, _counters} =
      Enum.map_reduce(senses, %{}, fn s, counters ->
        meta = Map.get(s, "_meta", %{})
        raw_word = Map.get(meta, "word") || Map.get(s, "word") || ""
        word = normalize_word(raw_word)

        raw_pos = Map.get(meta, "pos") || Map.get(s, "pos") || ""
        pos = canonical_pos(raw_pos, pos_fallback)

        key = {word, pos}
        idx = Map.get(counters, key, 0) + 1
        counters = Map.put(counters, key, idx)

        definition = pick_definition(s)
        example = pick_example(s)

        sense_syn = flatten_term_list(Map.get(s, "synonyms"))
        sense_ant = flatten_term_list(Map.get(s, "antonyms"))

        entry_syn = flatten_term_list(Map.get(meta, "entry_synonyms"))
        entry_ant = flatten_term_list(Map.get(meta, "entry_antonyms"))

        syn0 = uniq_terms((sense_syn ++ entry_syn) |> drop_self(word))
        ant0 = uniq_terms((sense_ant ++ entry_ant) |> drop_self(word))

        {synonyms, antonyms, rel_marks} =
          if fill_missing? do
            rel = Map.get(rels_by_key, key, %{syn: MapSet.new(), ant: MapSet.new()})

            {syn1, marks1} =
              if syn0 == [] and MapSet.size(rel.syn) > 0 do
                {MapSet.to_list(rel.syn) |> drop_self(word) |> uniq_terms(), ["rel:syn_pool"]}
              else
                {syn0, []}
              end

            {ant1, marks2} =
              if ant0 == [] and MapSet.size(rel.ant) > 0 do
                {MapSet.to_list(rel.ant) |> drop_self(word) |> uniq_terms(), ["rel:ant_pool"]}
              else
                {ant0, []}
              end

            {syn1, ant1, marks1 ++ marks2}
          else
            {syn0, ant0, []}
          end

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

            Enum.uniq(atoms ++ rel_marks)
          else
            rel_marks
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

  @spec summarize_entries([row()]) :: map()
  def summarize_entries(entries) when is_list(entries) do
    keys =
      entries
      |> Enum.flat_map(&Map.keys/1)
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

  @spec summarize_senses([map()]) :: map()
  def summarize_senses(senses) when is_list(senses) do
    keys =
      senses
      |> Enum.flat_map(&Map.keys/1)
      |> Enum.map(&to_string/1)
      |> Enum.frequencies()
      |> Enum.sort_by(fn {_k, v} -> -v end)
      |> Enum.take(30)

    has_gloss =
      Enum.count(senses, fn s ->
        (s["glosses"] |> List.wrap() |> length() > 0) or (s["raw_glosses"] |> List.wrap() |> length() > 0)
      end)

    has_examples =
      Enum.count(senses, fn s ->
        s["examples"] |> List.wrap() |> length() > 0
      end)

    %{
      senses: length(senses),
      with_gloss: has_gloss,
      with_examples: has_examples,
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
          |> Stream.filter(fn line -> String.contains?(line, needle1) or String.contains?(line, needle2) end)
          |> Stream.flat_map(fn line ->
            if decode? do
              case Jason.decode(line) do
                {:ok, %{} = m} ->
                  if Map.get(m, field) == word, do: [m], else: []

                {:error, _} ->
                  case on_err do
                    :raise ->
                      raise "JSON decode failed for line beginning: #{String.slice(line, 0, 200)}"

                    :count ->
                      [{:bad_json, line}]

                    :skip ->
                      []
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

  defp build_relations_index(senses, pos_fallback) do
    Enum.reduce(senses, %{}, fn s, acc ->
      meta = Map.get(s, "_meta", %{})
      word = normalize_word(Map.get(meta, "word") || Map.get(s, "word") || "")
      raw_pos = Map.get(meta, "pos") || Map.get(s, "pos") || ""
      pos = canonical_pos(raw_pos, pos_fallback)

      key = {word, pos}

      sense_syn = flatten_term_list(Map.get(s, "synonyms"))
      sense_ant = flatten_term_list(Map.get(s, "antonyms"))
      entry_syn = flatten_term_list(Map.get(meta, "entry_synonyms"))
      entry_ant = flatten_term_list(Map.get(meta, "entry_antonyms"))

      syn = MapSet.new(drop_self(uniq_terms(sense_syn ++ entry_syn), word))
      ant = MapSet.new(drop_self(uniq_terms(sense_ant ++ entry_ant), word))

      prev = Map.get(acc, key, %{syn: MapSet.new(), ant: MapSet.new()})

      acc
      |> Map.put(key, %{
        syn: MapSet.union(prev.syn, syn),
        ant: MapSet.union(prev.ant, ant)
      })
    end)
  end

  defp normalize_word(nil), do: ""
  defp normalize_word(s) when is_binary(s), do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")
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
    gloss =
      sense
      |> Map.get("glosses")
      |> List.wrap()
      |> Enum.find_value(fn
        g when is_binary(g) and g != "" -> String.trim(g)
        _ -> nil
      end) ||
        sense
        |> Map.get("raw_glosses")
        |> List.wrap()
        |> Enum.find_value(fn
          g when is_binary(g) and g != "" -> String.trim(g)
          _ -> nil
        end)

    if is_nil(gloss) or gloss == "" do
      nil
    else
      labels =
        (List.wrap(sense["tags"]) ++ List.wrap(sense["raw_tags"]))
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))
        |> Enum.filter(fn t ->
          lt = String.downcase(t)
          String.length(t) <= 24 and not String.starts_with?(lt, "of ") and not String.starts_with?(lt, "with ")
        end)
        |> Enum.map(&normalize_tag/1)
        |> Enum.uniq()

      q =
        case sense["qualifier"] do
          s when is_binary(s) ->
            s2 = String.trim(s)
            if s2 == "", do: nil, else: normalize_tag(s2)

          _ ->
            nil
        end

      labels =
        (if q, do: [q | labels], else: labels)
        |> Enum.uniq()

      if labels == [] do
        gloss
      else
        "(#{Enum.join(labels, ", ")}) " <> gloss
      end
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
  end

  defp uniq_terms(list) do
    list
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp drop_self(list, word_norm) do
    Enum.reject(list, fn t -> normalize_word(t) == word_norm end)
  end

  defp gram_function_from_tags(pos, tags, raw_tags) do
    allow =
      case pos do
        "noun" -> MapSet.new(~w(countable uncountable plural-only usually plural))
        "verb" ->
          MapSet.new(~w(transitive intransitive ditransitive ambitransitive copular auxiliary modal ergative impersonal))
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
    |> to_string()
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

