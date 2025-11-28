defmodule Db.JSONL.Fetch do
  @moduledoc false

  alias Db.JSONL.{IO, Util}

  @type row :: map()

  @spec fetch_word_stream(String.t(), keyword()) :: {:ok, Enumerable.t()} | {:error, any()}
  def fetch_word_stream(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)
    if word == "", do: {:error, :empty_word}, else: do_fetch_stream(word, opts)
  end

  @spec fetch_word(String.t(), keyword()) :: {:ok, list(row() | binary())} | {:error, any()}
  def fetch_word(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)

    if word == "" do
      {:error, :empty_word}
    else
      limit = Keyword.get(opts, :limit, :infinity)

      with {:ok, stream} <- do_fetch_stream(word, opts) do
        {:ok, stream |> Util.take_limit(limit) |> Enum.to_list()}
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
                  "entry_synonyms" => Util.flatten_term_list(Map.get(entry, "synonyms")),
                  "entry_antonyms" => Util.flatten_term_list(Map.get(entry, "antonyms"))
                }
                |> Enum.reject(fn {_k, v} -> is_nil(v) end)
                |> Map.new()

              entry
              |> Map.get("senses", [])
              |> List.wrap()
              |> Enum.map(fn s -> s |> Util.ensure_map() |> Map.put("_meta", meta) end)

            _other ->
              []
          end)
          |> Util.take_limit(limit)
          |> Enum.to_list()

        {:ok, senses}
      end
    end
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

    %{entries: length(entries), pos: pos, senses_total: senses_total, key_frequencies_top30: keys}
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
        s["glosses"] |> List.wrap() |> length() > 0 or
          s["raw_glosses"] |> List.wrap() |> length() > 0
      end)

    has_examples =
      Enum.count(senses, fn s -> s["examples"] |> List.wrap() |> length() > 0 end)

    %{
      senses: length(senses),
      with_gloss: has_gloss,
      with_examples: has_examples,
      key_frequencies_top30: keys
    }
  end

  # ───────────────────────── internals: fetch ─────────────────────────

  defp do_fetch_stream(word, opts) do
    path = IO.resolve_path(opts)
    field = opts |> Keyword.get(:field, "word") |> to_string()
    decode? = Keyword.get(opts, :decode?, true)
    on_err = Keyword.get(opts, :on_decode_error, :skip)

    unless File.exists?(path) do
      {:error, {:no_such_file, path}}
    else
      if decode? and not IO.jason_available?() do
        {:error, :jason_not_available}
      else
        {needle1, needle2} = IO.needles(field, word)

        stream =
          path
          |> IO.stream_lines()
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
end
