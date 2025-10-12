defmodule Llm.Util do
  @moduledoc """
  Pure, stateless helpers for `Llm`:

  - Input sanitization
  - JSON "rescue" decoding
  - TSV parsing (3–6 fields, incl. synonyms/antonyms CSV)
  - Entry shaping & validation
  - HTTP body map builders (format/keep_alive/options/temperature)
  - Misc utils (min num_predict, lemma filtering, syn/ant sanitation)
  """

  # ── Input normalization ──────────────────────────────────────────

  @spec sanitize_word(String.t()) :: String.t()
  def sanitize_word(word) when is_binary(word) do
    word
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/[\r\n]+/u, "")
    |> then(&Regex.replace(~r/[:;,.!?]+$/u, &1, ""))
  end

  @spec default_example(String.t() | nil) :: String.t()
  def default_example(lemma) do
    w = lemma |> to_string() |> String.downcase() |> String.trim()
    "This sentence includes #{w}."
  end

  # ── Request map builders (pure) ──────────────────────────────────

  @spec maybe_put_format(map(), keyword()) :: map()
  def maybe_put_format(body, opts) do
    case Keyword.get(opts, :format) do
      nil -> body
      fmt when is_binary(fmt) -> Map.put(body, "format", fmt)
      _ -> body
    end
  end

  @spec maybe_put_keep_alive(map(), keyword()) :: map()
  def maybe_put_keep_alive(body, opts) do
    case Keyword.get(opts, :keep_alive, "10m") do
      nil -> body
      v when is_binary(v) or is_integer(v) -> Map.put(body, "keep_alive", v)
      _ -> body
    end
  end

  @spec put_temperature(map(), any()) :: map()
  def put_temperature(body, t) when is_number(t),
    do: Map.update(body, "options", %{"temperature" => t}, &Map.put(&1, "temperature", t))

  def put_temperature(body, _), do: body

  @spec put_options(map(), map()) :: map()
  def put_options(body, %{} = more) when map_size(more) > 0,
    do: Map.update(body, "options", more, &Map.merge(&1, more))

  def put_options(body, _), do: body

  @spec ensure_min_predict(map(), integer()) :: map()
  def ensure_min_predict(opts_map, min) when is_map(opts_map) and is_integer(min) do
    current = Map.get(opts_map, :num_predict, Map.get(opts_map, "num_predict", 0))
    if current >= min, do: opts_map, else: Map.put(opts_map, :num_predict, min)
  end

  # ── JSON “rescue” decoding ───────────────────────────────────────

  @spec decode_strict_json(String.t()) :: {:ok, map()} | {:error, term()}
  def decode_strict_json(s) when is_binary(s) do
    s = s |> String.trim() |> strip_fences()

    case Jason.decode(s) do
      {:ok, data} ->
        {:ok, data}

      {:error, _} ->
        s2 = s |> extract_json_object() |> fix_trailing_commas()
        Jason.decode(s2)
    end
  end

  @spec strip_fences(String.t()) :: String.t()
  def strip_fences(s) do
    s
    |> String.trim_leading("```json")
    |> String.trim_leading("```")
    |> String.trim_trailing("```")
    |> String.trim()
  end

  # Robust substring extraction using :binary.match/:binary.matches
  @spec extract_json_object(String.t()) :: String.t()
  def extract_json_object(s) when is_binary(s) do
    case :binary.match(s, "{") do
      :nomatch ->
        s

      {start, _len} ->
        case :binary.matches(s, "}") do
          :nomatch ->
            s

          list when is_list(list) and list != [] ->
            {last_pos, _} = List.last(list)
            if last_pos >= start, do: :binary.part(s, start, last_pos - start + 1), else: s

          _ ->
            s
        end
    end
  end

  @spec fix_trailing_commas(String.t()) :: String.t()
  def fix_trailing_commas(s), do: Regex.replace(~r/,\s*([}\]])/, s, "\\1")

  # ── TSV parsing (3–6 fields) ─────────────────────────────────────

  @type entry_map :: map()

  @spec tsv_to_entries(String.t(), [String.t()]) :: [entry_map]
  def tsv_to_entries(tsv, allowed_pos) when is_binary(tsv) and is_list(allowed_pos) do
    tsv
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == "" or String.starts_with?(&1, "#")))
    |> Enum.map(&split_fields/1)
    |> Enum.filter(fn parts -> length(parts) >= 3 end) # accept 3–6 fields; salvage example if missing
    |> Enum.map(fn parts ->
      [pos, lemma, gloss | rest] = parts
      example0 = rest |> Enum.at(0, "") |> String.trim()
      syn_cell = rest |> Enum.at(1, "") |> String.trim()
      ant_cell = rest |> Enum.at(2, "") |> String.trim()

      example = if example0 == "", do: default_example(lemma), else: example0
      syns = parse_csv_words(syn_cell, lemma)
      ants = parse_csv_words(ant_cell, lemma)

      %{
        "pos" => String.downcase(pos),
        "lemma" => String.downcase(lemma),
        "short_gloss" => gloss,
        "example" => example,
        "synonyms" => syns,
        "antonyms" => ants
      }
    end)
    |> Enum.filter(&valid_entry?(&1, allowed_pos))
    |> Enum.uniq_by(fn e -> {e["pos"], e["lemma"], e["short_gloss"]} end)
  end

  def tsv_to_entries(_, _), do: []

  @spec split_fields(String.t()) :: [String.t()]
  def split_fields(line) do
    cond do
      String.contains?(line, "\t") -> String.split(line, "\t")
      String.contains?(line, " | ") -> String.split(line, " | ")
      String.contains?(line, " - ") -> String.split(line, " - ")
      String.contains?(line, " : ") -> String.split(line, " : ")
      String.contains?(line, " — ") -> String.split(line, " — ")
      String.contains?(line, " – ") -> String.split(line, " – ")
      true -> Regex.split(~r/\s{2,}/, line)
    end
  end

  @spec parse_csv_words(String.t(), String.t() | nil) :: [String.t()]
  def parse_csv_words(cell, lemma) when is_binary(cell) do
    cell
    |> String.split([",", "|"], trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.reject(&(&1 == "" or &1 == String.downcase(lemma || "")))
    |> Enum.filter(&single_word?/1)
    |> Enum.uniq()
    |> Enum.take(5)
  end

  def parse_csv_words(_, _), do: []

  @spec single_word?(String.t()) :: boolean()
  def single_word?(w) do
    # allow alphabetic + internal hyphen/apostrophe; reject spaces
    String.match?(w, ~r/^[a-z]+(?:[-'][a-z]+)*$/)
  end

  # ── Entry shaping & validation ───────────────────────────────────

  @spec only_word([map()], String.t()) :: [map()]
  def only_word(entries, w),
    do: Enum.filter(entries, fn e -> String.downcase(e["lemma"] || "") == w end)

  @spec prefer_exact_lemma(map(), String.t()) :: {:ok, map()}
  def prefer_exact_lemma(%{"word" => w, "entries" => entries} = data, word)
      when is_list(entries) and is_binary(w) do
    exact = Enum.filter(entries, fn e -> String.downcase(e["lemma"] || "") == word end)
    if exact != [], do: {:ok, %{"word" => w, "entries" => exact}}, else: {:ok, data}
  end

  def prefer_exact_lemma(other, _word), do: {:ok, other}

  @spec validate_pos_payload(map(), [String.t()]) :: :ok | {:error, term()}
  def validate_pos_payload(%{"word" => w, "entries" => entries}, allowed)
      when is_binary(w) and is_list(entries) do
    if Enum.all?(entries, &valid_entry?(&1, allowed)), do: :ok, else: {:error, :bad_entries}
  end

  def validate_pos_payload(_other, _allowed), do: {:error, :bad_shape}

  @spec valid_entry?(map(), [String.t()]) :: boolean()
  def valid_entry?(%{"pos" => pos, "lemma" => lemma, "short_gloss" => gloss, "example" => ex} = e, allowed)
      when is_binary(pos) and is_binary(lemma) and is_binary(gloss) and is_binary(ex) do
    syn_ok = Map.get(e, "synonyms", []) |> is_list()
    ant_ok = Map.get(e, "antonyms", []) |> is_list()
    pos in allowed and lemma != "" and gloss != "" and ex != "" and syn_ok and ant_ok
  end

  def valid_entry?(_, _), do: false

  # ── Syn/Ant cleanup ──────────────────────────────────────────────

  @spec sanitize_syn_ant(any(), String.t() | nil) :: [String.t()]
  def sanitize_syn_ant(list, lemma) when is_list(list) do
    list
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.reject(&(&1 == "" or &1 == String.downcase(lemma || "")))
    |> Enum.filter(&single_word?/1)
    |> Enum.uniq()
    |> Enum.take(5)
  end

  def sanitize_syn_ant(_, _), do: []
end

