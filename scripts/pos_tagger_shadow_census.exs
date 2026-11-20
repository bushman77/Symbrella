# P-036: shadow census for Core.Syntax.PosTagger.
#
#   mix run scripts/pos_tagger_shadow_census.exs --corpus FILE [--arm chat|colony|all] [--out FILE]
#
# Read-only. For each corpus row (same JSON-in-markdown format as the R-001
# census corpora) it runs Core.resolve_input/2 with persistence disabled and
# compares:
#   1. the shadow tagger's tag per base token vs LIFG's chosen POS family;
#   2. declarative slot-signature matches over the tagger's tags (five specs
#      from the P-036 experiments) vs the existing Core.Syntax.Signatures ids.
# Output: aggregates (and with --out, per-row counts and ids) — never sentences.

defmodule PosShadow.Corpus do
  def load(path) do
    with {:ok, raw} <- File.read(path),
         [_, json] <- Regex.run(~r/```json\n(.*?)\n```/s, raw) || {:error, :no_json_block},
         {:ok, %{"corpus_id" => id, "rows" => rows}} when is_binary(id) and is_list(rows) <-
           JSON.decode(json) do
      {:ok, %{corpus_id: id, rows: rows, sha256: :crypto.hash(:sha256, raw) |> Base.encode16(case: :lower)}}
    else
      {:error, reason} -> {:error, reason}
      other -> {:error, {:invalid_corpus, other}}
    end
  end
end

defmodule PosShadow.Runner do
  @resolver_opts [
    mode: :prod, enrich_lexicon?: true, lexicon_stage?: true, response: :off,
    session_id: nil, experience_origin: nil, fuzzy: false, episodes: false,
    persist_episodes: false, hippocampus_encode?: false, comprehension_wm?: false,
    neuron_activation?: false, notify_brain?: false, synapse_shadow?: false,
    synaptic_feedback?: false, self_model: :off
  ]

  @particles ~w(up down off on out in over away back around through)
  @specs [
    {:verb_particle, [%{pos: ~w(verb)}, %{pos: ~w(adverb preposition particle), words: @particles}]},
    {:wh_be_activity, [%{pos: ~w(pronoun), words: ~w(what)}, %{pos: ~w(auxiliary verb), words: ~w(am is are)}, %{pos: ~w(pronoun)}, %{pos: ~w(verb), suffix: "ing"}]},
    {:wh_be_activity, [%{pos: ~w(pronoun), words: ~w(what)}, %{pos: ~w(auxiliary verb), words: ~w(am is are)}, %{pos: ~w(pronoun)}, %{words: ~w(up)}, %{words: ~w(to)}]},
    {:wh_aux_object, [%{pos: ~w(pronoun), words: ~w(what who)}, %{pos: ~w(auxiliary verb), words: ~w(do does did)}, %{pos: ~w(determiner)}, %{pos: ~w(noun)}, %{pos: ~w(verb)}]},
    {:svo, [%{pos: ~w(pronoun)}, %{pos: ~w(verb)}, %{pos: ~w(noun)}]}
  ]

  def run(%{"sentence" => sentence} = row) when is_binary(sentence) do
    si = Core.resolve_input(sentence, @resolver_opts)
    tags = List.wrap(Map.get(si, :pos_tags))

    lifg =
      si |> Map.get(:lifg_choices, []) |> List.wrap()
      |> Map.new(fn ch -> {Map.get(ch, :token_index), pos_of(Map.get(ch, :id))} end)

    compared = Enum.filter(tags, &Map.has_key?(lifg, &1.token_index))
    agree = Enum.count(compared, &(family(lifg[&1.token_index]) == &1.tag))

    slot = slot_matches(tags)
    existing = existing_matches(List.wrap(Map.get(si, :sentence_signatures)))

    %{
      case_id: row["case_id"], arm: row["arm"], family: row["family"],
      tokens: length(tags), compared: length(compared), agree: agree,
      slot: Enum.map(slot, &Tuple.to_list/1), existing: Enum.map(existing, &Tuple.to_list/1),
      both: MapSet.size(MapSet.intersection(slot, existing)),
      slot_only: MapSet.size(MapSet.difference(slot, existing)),
      existing_only: MapSet.size(MapSet.difference(existing, slot))
    }
  end

  defp slot_matches(tags) do
    nodes = Enum.map(tags, &%{index: &1.token_index, word: &1.word, pos: MapSet.new([&1.tag])})

    for {name, slots} <- @specs, k = length(slots), start <- 0..max(length(nodes) - k, -1)//1,
        win = Enum.slice(nodes, start, k), length(win) == k,
        consecutive?(win),
        Enum.zip(win, slots) |> Enum.all?(fn {n, s} -> fits?(n, s) end),
        into: MapSet.new(),
        do: {name, hd(win).index, List.last(win).index}
  end

  defp existing_matches(sigs) do
    sigs
    |> Enum.flat_map(fn
      %{id: id, span: {a, b}} ->
        case kind(id) do
          nil -> []
          k -> [{k, a, b}]
        end

      _ ->
        []
    end)
    |> MapSet.new()
  end

  defp kind(:verb_particle_construction), do: :verb_particle
  defp kind(:wh_be_activity_question), do: :wh_be_activity
  defp kind(:wh_aux_object_question), do: :wh_aux_object
  defp kind(id) when is_atom(id), do: if(String.contains?(Atom.to_string(id), "svo"), do: :svo, else: nil)
  defp kind(_), do: nil

  defp fits?(node, slot) do
    (slot[:pos] == nil or not MapSet.disjoint?(node.pos, MapSet.new(slot.pos))) and
      (slot[:words] == nil or node.word in slot.words) and
      (slot[:suffix] == nil or String.ends_with?(node.word, slot.suffix))
  end

  defp consecutive?(win) do
    win |> Enum.map(& &1.index) |> Enum.chunk_every(2, 1, :discard) |> Enum.all?(fn [a, b] -> b == a + 1 end)
  end

  defp pos_of(id) do
    case String.split(to_string(id), "|") do
      [_, pos | _] -> pos
      _ -> nil
    end
  end

  defp family(nil), do: nil

  defp family(pos) do
    case String.downcase(pos) do
      "pron" -> "pronoun"
      p when p in ["det", "article"] -> "determiner"
      p when p in ["aux", "modal"] -> "auxiliary"
      p -> p
    end
  end

  def aggregate(records) do
    sum = fn k -> records |> Enum.map(&Map.get(&1, k, 0)) |> Enum.sum() end
    compared = sum.(:compared)

    by_kind =
      for kind <- [:verb_particle, :wh_be_activity, :wh_aux_object, :svo], into: %{} do
        s = records |> Enum.flat_map(& &1.slot) |> Enum.count(&(hd(&1) == kind))
        e = records |> Enum.flat_map(& &1.existing) |> Enum.count(&(hd(&1) == kind))
        {kind, %{slot: s, existing: e}}
      end

    %{
      rows: length(records),
      tokens: sum.(:tokens),
      tagger_vs_lifg_pos: %{compared: compared, agree: sum.(:agree),
                            agreement: if(compared == 0, do: 0.0, else: Float.round(sum.(:agree) / compared, 4))},
      signatures: %{both: sum.(:both), slot_only: sum.(:slot_only), existing_only: sum.(:existing_only),
                    by_kind: by_kind}
    }
  end
end

{opts, rest, errors} =
  OptionParser.parse(System.argv(), strict: [corpus: :string, arm: :string, out: :string])

arm = opts[:arm] || "all"

if errors != [] or rest != [] or not is_binary(opts[:corpus]) or arm not in ["chat", "colony", "all"] do
  IO.puts(:stderr, "Usage: mix run scripts/pos_tagger_shadow_census.exs --corpus FILE [--arm chat|colony|all] [--out FILE]")
  System.halt(2)
end

corpus =
  case PosShadow.Corpus.load(opts[:corpus]) do
    {:ok, c} -> c
    {:error, reason} -> IO.puts(:stderr, "Rejected corpus: #{inspect(reason)}"); System.halt(2)
  end

rows = Enum.filter(corpus.rows, &(arm == "all" or &1["arm"] == arm))
records = Enum.map(rows, &PosShadow.Runner.run/1)

revision =
  case System.cmd("git", ["rev-parse", "--short", "HEAD"], stderr_to_stdout: true) do
    {sha, 0} -> String.trim(sha)
    _ -> :unknown
  end

report = %{
  census: :p036_pos_tagger_shadow, revision: revision, corpus_id: corpus.corpus_id,
  corpus_sha256: corpus.sha256, arm: arm, aggregate: PosShadow.Runner.aggregate(records)
}

IO.inspect(report, label: "P-036 POS tagger shadow census", limit: :infinity)

if is_binary(opts[:out]) do
  File.mkdir_p!(Path.dirname(opts[:out]))
  File.write!(opts[:out], JSON.encode!(Map.put(report, :records, records)))
  IO.puts("Per-row records written to #{opts[:out]} (no sentences).")
end
