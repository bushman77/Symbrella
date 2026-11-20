defmodule Core.Syntax.PosTagger do
  @moduledoc """
  P-036 (shadow): whole-sentence part-of-speech tagging.

  A bigram hidden-Markov-model Viterbi pass over base tokens. Each word's
  candidate tags are limited to the POS families its dictionary entries
  attest (`:active_cells`), plus a small closed augment (BE/DO/HAVE/modal
  forms may be `"auxiliary"`, verbal particles may be `"particle"`, the
  assistant's name is a `"proper_noun"`). Transition and emission
  probabilities come from `priv/pos/ewt_hmm.json`, derived from the
  Universal Dependencies English Web Treebank (CC BY-SA 4.0; see NOTICE.md
  and `scripts/build_pos_hmm.py`).

  Shadow only: `attach/1` records `:pos_tags` on the SemanticInput and no
  consumer reads it. It never chooses dictionary senses, changes tokens or
  writes memory. A bigram tagger cannot see long-range constraints (e.g.
  do-support in "What did the artist draw?" can leave "draw" as a noun);
  structural signatures remain responsible for those.
  """

  @max_tokens 64
  @data_key {__MODULE__, :tables}

  @aux_words MapSet.new(~w(
    am is are was were be been being do does did have has had
    can could may might must shall should will would
  ))
  @particle_words MapSet.new(~w(up down off on out in over away back around through))
  @assistant_names MapSet.new(~w(symbrella))

  @type tag_entry :: %{
          token_index: non_neg_integer(),
          word: String.t(),
          tag: String.t(),
          candidates: [String.t()]
        }

  @doc "Attach shadow `:pos_tags` to a SemanticInput-like map."
  @spec attach(map()) :: map()
  def attach(%{} = si) do
    Map.put(si, :pos_tags, tag_si(si))
  end

  def attach(other), do: other

  @doc "Tag the base tokens of a SemanticInput-like map. Returns [] when not applicable."
  @spec tag_si(map()) :: [tag_entry()]
  def tag_si(%{} = si) do
    nodes = base_nodes(Map.get(si, :tokens, []))
    attested = attested_families(Map.get(si, :active_cells, []))

    nodes
    |> Enum.map(fn n -> Map.put(n, :candidates, candidates(n.word, Map.get(attested, n.word, []))) end)
    |> tag()
  end

  def tag_si(_), do: []

  @doc """
  Tag nodes `[%{index: i, word: w, candidates: [tag]}]` in order.
  Returns [] for empty input, more than #{@max_tokens} nodes, or missing tables.
  """
  @spec tag([map()]) :: [tag_entry()]
  def tag(nodes) when is_list(nodes) and nodes != [] and length(nodes) <= @max_tokens do
    case tables() do
      {:ok, t} ->
        words = Enum.map(nodes, & &1.word)
        cands = Enum.map(nodes, &restrict(&1.candidates, t))
        path = viterbi(words, cands, t)

        Enum.zip_with([nodes, path, cands], fn [n, tag, cs] ->
          %{token_index: n.index, word: n.word, tag: tag, candidates: cs}
        end)

      :error ->
        []
    end
  end

  def tag(_), do: []

  @doc "Candidate tags for a word from its attested POS families plus the closed augment."
  @spec candidates(String.t(), [String.t()]) :: [String.t()]
  def candidates(word, attested) when is_binary(word) and is_list(attested) do
    extra =
      if(MapSet.member?(@aux_words, word), do: ["auxiliary"], else: []) ++
        if(MapSet.member?(@particle_words, word), do: ["particle"], else: []) ++
        if(MapSet.member?(@assistant_names, word), do: ["proper_noun"], else: [])

    (attested ++ extra) |> Enum.uniq() |> Enum.sort()
  end

  # ---------------------------------------------------------------------------

  defp restrict(cands, %{tags: tags}) do
    case Enum.filter(cands, &MapSet.member?(tags, &1)) do
      [] -> ["noun"]
      cs -> cs
    end
  end

  defp viterbi([w0 | rest], [c0 | crest], t) do
    init = Map.new(c0, fn tag -> {tag, {tr(t, "START", tag) + em(t, w0, tag), [tag]}} end)

    final =
      Enum.zip(rest, crest)
      |> Enum.reduce(init, fn {w, cs}, prev ->
        Map.new(cs, fn tag ->
          {s, path} =
            prev
            |> Enum.map(fn {p, {s, path}} -> {s + tr(t, p, tag), path} end)
            |> Enum.max_by(fn {s, path} -> {s, path} end)

          {tag, {s + em(t, w, tag), [tag | path]}}
        end)
      end)

    {_tag, {_s, path}} =
      Enum.max_by(final, fn {tag, {s, path}} -> {s + tr(t, tag, "END"), path} end)

    Enum.reverse(path)
  end

  defp tr(%{transitions: tt}, a, b) do
    case tt do
      %{^a => %{^b => v}} when is_number(v) -> v
      _ -> -30.0
    end
  end

  defp em(%{emissions: ee, floor: fl, unknown: un}, word, tag) do
    case Map.get(ee, word) do
      nil -> Map.get(un, tag, -30.0)
      row -> Map.get(row, tag, Map.get(fl, tag, -30.0))
    end
  end

  defp tables do
    case :persistent_term.get(@data_key, nil) do
      nil ->
        result = load_tables()
        :persistent_term.put(@data_key, result)
        result

      cached ->
        cached
    end
  end

  defp load_tables do
    path = Application.app_dir(:core, "priv/pos/ewt_hmm.json")

    with {:ok, raw} <- File.read(path),
         {:ok,
          %{
            "tags" => tags,
            "transitions" => tt,
            "emissions" => ee,
            "emission_floor" => fl,
            "unknown" => un
          }} <- JSON.decode(raw) do
      {:ok, %{tags: MapSet.new(tags), transitions: tt, emissions: ee, floor: fl, unknown: un}}
    else
      _ -> :error
    end
  end

  defp base_nodes(tokens) when is_list(tokens) do
    tokens
    |> Enum.filter(fn tok ->
      is_map(tok) and (get(tok, :n) || 1) == 1 and get(tok, :kind) not in [:punct, "punct"]
    end)
    |> Enum.map(fn tok -> %{index: get(tok, :index), word: norm(get(tok, :phrase))} end)
    |> Enum.filter(&(is_integer(&1.index) and &1.word != ""))
    |> Enum.sort_by(& &1.index)
  end

  defp base_nodes(_), do: []

  defp attested_families(cells) when is_list(cells) do
    cells
    |> Enum.filter(&is_map/1)
    |> Enum.reduce(%{}, fn cell, acc ->
      word = norm(get(cell, :norm) || get(cell, :word))
      fam = family(get(cell, :pos))

      if word == "" or is_nil(fam),
        do: acc,
        else: Map.update(acc, word, [fam], &[fam | &1])
    end)
    |> Map.new(fn {w, fams} -> {w, Enum.uniq(fams)} end)
  end

  defp attested_families(_), do: %{}

  defp family(nil), do: nil

  defp family(pos) do
    case pos |> to_string() |> String.downcase() do
      "pron" -> "pronoun"
      p when p in ["det", "article"] -> "determiner"
      p when p in ["aux", "modal"] -> "auxiliary"
      "" -> nil
      p -> p
    end
  end

  defp get(%{} = m, k), do: Map.get(m, k) || Map.get(m, Atom.to_string(k))

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.replace("’", "'")
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp norm(_), do: ""
end
