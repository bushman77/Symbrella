defmodule Db.Import do
  @moduledoc """
  Import helpers for Kaikki/Wiktextract English JSONL (hard-coded path by default).

  Public helpers:

    * `Db.Import.which/0`             -> prints the file path being used
    * `Db.Import.preview_first/0`     -> first entry's processed senses (no header text, nils -> "")
    * `Db.Import.preview_ids/0`       -> first entry's `word|pos|idx` IDs
    * `Db.Import.stream_senses/0`     -> stream of processed sense maps (defaults include unglossed)
    * `Db.Import.stream_brain_cells/0`-> stream shaped for :brain_cells inserts
    * `Db.Import.seed/1`              -> chunked `insert_all` into DB (Repo = Db)

  Notes:
  - `gram_function` is a **list** to match the DB column `text[]`.
  - Uses `insert_all` (no changesets); timestamps set in-code.
  """

  @file_path "apps/db/priv/kaikki.org-dictionary-English.jsonl"

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle
  )

  # POS-appropriate grammatical function labels (expand later if needed).
  @gram_keep %{
    "noun" => ~w(countable uncountable plural-only usually plural),
    "verb" =>
      ~w(transitive intransitive ditransitive ambitransitive copular auxiliary modal ergative impersonal),
    "adjective" => ~w(attributive-only predicative-only postpositive comparative-only)
  }

  @include_unglossed true
  @default_chunk 1_000

  def which do
    path = Path.expand(@file_path)
    IO.puts("Using file: " <> path)
    path
  end

  def preview_first do
    stream_entries()
    |> Stream.map(&to_senses(&1, @include_unglossed))
    |> Stream.reject(&Enum.empty?/1)
    |> Enum.take(1)
    |> List.first()
    |> case do
      nil -> []
      senses when is_list(senses) -> Enum.map(senses, &de_nilify/1)
    end
  end

  def preview_ids do
    case preview_first() do
      nil -> []
      senses when is_list(senses) -> Enum.map(senses, &Map.fetch!(&1, :id))
    end
  end

  def stream_senses do
    stream_entries()
    |> Stream.flat_map(&to_senses(&1, @include_unglossed))
  end

  def stream_brain_cells do
    stream_senses()
    |> Stream.map(&sense_to_brain_cell/1)
  end

  @doc """
  Seed brain_cells from the Kaikki/Wiktextract JSONL.

  ## Options
    * `:path`  - override file path (default: #{@file_path})
    * `:limit` - max senses to insert (default: :all)
    * `:pos`   - restrict to POS (CSV or list), e.g., "noun,verb" (default: all allowed)
    * `:chunk` - insert_all batch size (default: #{@default_chunk})
    * `:dry_run` - only count, don’t write (default: false)
    * `:include_unglossed` - include senses without gloss (default: #{@include_unglossed})
  """
  def seed(opts \\ []) do
    path = opts[:path] || Path.expand(@file_path)
    limit = opts[:limit] || :all
    chunk = opts[:chunk] || @default_chunk
    dry? = !!opts[:dry_run]
    inc_unglossed? = Map.get(opts_to_map(opts), :include_unglossed, @include_unglossed)

    pos_ok =
      case opts[:pos] do
        nil ->
          @allowed_pos

        s when is_binary(s) ->
          s
          |> String.split([",", " "], trim: true)
          |> Enum.map(&normalize_pos/1)
          |> Enum.filter(&(&1 in @allowed_pos))

        list when is_list(list) ->
          list |> Enum.map(&normalize_pos/1) |> Enum.filter(&(&1 in @allowed_pos))

        _ ->
          @allowed_pos
      end

    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    stream =
      File.stream!(path, [], :line)
      |> Stream.map(&String.trim_trailing/1)
      |> Stream.reject(&(&1 == ""))
      |> Stream.map(&Jason.decode!/1)
      |> Stream.filter(&english?/1)
      |> Stream.filter(&allowed_pos?/1)
      |> Stream.filter(fn %{"pos" => p} -> normalize_pos(p) in pos_ok end)
      |> Stream.flat_map(&to_senses(&1, inc_unglossed?))
      |> Stream.map(&sense_to_brain_cell/1)
      |> maybe_take(limit)
      |> Stream.map(&put_ts(&1, now))

    if dry? do
      produced = Enum.reduce(stream, 0, fn _, acc -> acc + 1 end)
      %{produced: produced, inserted: 0, chunks: 0, path: path, pos: pos_ok, dry_run: true}
    else
      {produced, inserted, chunks} =
        stream
        |> Stream.chunk_every(chunk)
        |> Enum.reduce({0, 0, 0}, fn batch, {p, i, c} ->
          {count, _} =
            Db.insert_all("brain_cells", batch,
              on_conflict: :nothing,
              conflict_target: [:id]
            )

          {p + length(batch), i + count, c + 1}
        end)

      %{
        produced: produced,
        inserted: inserted,
        chunks: chunks,
        path: path,
        pos: pos_ok,
        dry_run: false
      }
    end
  end

  # ---- streaming / decode ----
  defp stream_entries do
    File.stream!(Path.expand(@file_path), [], :line)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.reject(&(&1 == ""))
    |> Stream.map(&Jason.decode!/1)
    |> Stream.filter(&english?/1)
    |> Stream.filter(&allowed_pos?/1)
  end

  # ---- entry -> list of senses ----
  defp to_senses(entry, include_unglossed?) when is_map(entry) do
    word = entry["word"]
    pos0 = entry["pos"]
    pos = normalize_pos(pos0)
    senses = entry["senses"] || []

    entry_syns = extract_word_list(entry, "synonyms")
    entry_ants = extract_word_list(entry, "antonyms")

    senses
    |> Enum.map(fn sense ->
      gloss = first_gloss(sense)
      syns = merge_and_dedup(entry_syns, extract_word_list(sense, "synonyms"))
      ants = merge_and_dedup(entry_ants, extract_word_list(sense, "antonyms"))

      if include_unglossed? or (is_binary(gloss) and String.trim(gloss) != "") do
        %{
          id: nil,
          word: word,
          pos: pos,
          # may be nil
          definition: gloss,
          example: first_example(sense),
          gram_function: derive_gram_function(sense, pos),
          synonyms: drop_nil_and_self(syns, word),
          antonyms: drop_nil_and_self(ants, word)
        }
      else
        nil
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.with_index()
    |> Enum.map(fn {sense_map, idx} ->
      Map.put(sense_map, :id, "#{word}|#{pos}|#{idx}")
    end)
  end

  # ---- helpers ----
  defp first_gloss(sense) do
    cond do
      is_binary(sense["gloss"]) -> sense["gloss"]
      is_list(sense["glosses"]) -> Enum.find(sense["glosses"], &is_binary/1)
      is_list(sense["gloss"]) -> Enum.find(sense["gloss"], &is_binary/1)
      true -> nil
    end
  end

  defp first_example(sense) do
    exs = sense["examples"] || []

    if is_list(exs) do
      exs
      |> Enum.map(fn
        %{"text" => t} when is_binary(t) -> t
        t when is_binary(t) -> t
        _ -> nil
      end)
      |> Enum.reject(&is_nil/1)
      |> List.first()
    else
      nil
    end
  end

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # KEY FIX: only emit POS-sanctioned grammar labels; synthesize "usually plural"
  # for nouns *only if* both "usually" and "plural" tags are present.
  # All other meta tags (e.g., "usually", "capitalized", "initialism") are ignored.
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  defp derive_gram_function(sense, pos) do
    tags_list =
      case sense["tags"] do
        t when is_list(t) -> Enum.map(t, &normalize_tag/1)
        _ -> []
      end

    tags = MapSet.new(tags_list)

    case pos do
      "noun" ->
        gf =
          []
          |> add_if(tags, "countable")
          |> add_if(tags, "uncountable")
          |> add_if(tags, "plural-only")
          |> add_if_synth_usually_plural(tags)

        Enum.uniq(gf)

      "verb" ->
        keep = Map.get(@gram_keep, "verb", [])
        tags |> Enum.filter(&(&1 in keep)) |> Enum.uniq()

      "adjective" ->
        keep = Map.get(@gram_keep, "adjective", [])
        tags |> Enum.filter(&(&1 in keep)) |> Enum.uniq()

      _ ->
        []
    end
  end

  defp normalize_tag(t) when is_binary(t),
    do: t |> String.downcase() |> String.trim()

  defp normalize_tag(t), do: to_string(t) |> String.downcase() |> String.trim()

  defp add_if(acc, tags_set, label),
    do: if(MapSet.member?(tags_set, label), do: [label | acc], else: acc)

  defp add_if_synth_usually_plural(acc, tags_set) do
    cond do
      MapSet.member?(tags_set, "usually plural") ->
        ["usually plural" | acc]

      MapSet.member?(tags_set, "usually") and MapSet.member?(tags_set, "plural") ->
        ["usually plural" | acc]

      true ->
        acc
    end
  end

  # -------------------------------------------------------------------------

  defp extract_word_list(map, key) when is_map(map) do
    case Map.get(map, key) do
      nil ->
        []

      list when is_list(list) ->
        list
        |> Enum.flat_map(fn
          %{"word" => w} when is_binary(w) ->
            [String.trim(w)]

          %{"terms" => terms} when is_list(terms) ->
            terms
            |> Enum.flat_map(fn
              %{"word" => w} when is_binary(w) -> [String.trim(w)]
              s when is_binary(s) -> [String.trim(s)]
              _ -> []
            end)

          s when is_binary(s) ->
            [String.trim(s)]

          _ ->
            []
        end)
        |> Enum.reject(&(&1 == ""))

      _ ->
        []
    end
  end

  defp merge_and_dedup(a, b), do: Enum.uniq(a ++ b)

  defp drop_nil_and_self(list, word) do
    list
    |> Enum.reject(&is_nil/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.reject(&(&1 == word))
  end

  defp sense_to_brain_cell(%{
         id: id,
         word: word,
         pos: pos,
         definition: defn,
         example: example,
         gram_function: gf,
         synonyms: syns,
         antonyms: ants
       }) do
    %{
      id: id,
      word: word,
      norm: normalize_word(word),
      pos: pos,
      definition: defn,
      example: example,
      gram_function: gf || [],
      synonyms: syns,
      antonyms: ants,
      semantic_atoms: [],
      type: nil,
      status: "inactive",
      activation: 0.0,
      modulated_activation: 0.0,
      dopamine: 0.0,
      serotonin: 0.0,
      position: nil,
      connections: [],
      last_dose_at: nil,
      last_substance: nil,
      token_id: nil
    }
  end

  defp english?(%{"lang" => "English"}), do: true
  defp english?(%{"lang_code" => "en"}), do: true
  defp english?(_), do: false

  defp allowed_pos?(%{"pos" => pos}) when is_binary(pos),
    do: normalize_pos(pos) in @allowed_pos

  defp allowed_pos?(_), do: false

  defp normalize_pos(pos) when is_binary(pos) do
    pos
    |> String.downcase()
    |> String.replace(~r/[\s-]+/u, "_")
    |> String.trim()
  end

  defp normalize_word(word) when is_binary(word) do
    word
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp put_ts(map, now) when is_map(map),
    do: Map.merge(map, %{inserted_at: now, updated_at: now})

  defp maybe_take(stream, :all), do: stream
  defp maybe_take(stream, n) when is_integer(n) and n > 0, do: Stream.take(stream, n)
  defp maybe_take(stream, _), do: stream

  defp de_nilify(nil), do: ""
  defp de_nilify(%{} = map), do: Map.new(map, fn {k, v} -> {k, de_nilify(v)} end)
  defp de_nilify(list) when is_list(list), do: Enum.map(list, &de_nilify/1)
  defp de_nilify(other), do: other

  defp opts_to_map(opts) when is_list(opts), do: Map.new(opts)
end
