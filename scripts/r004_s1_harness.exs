# P-039 / R-004 S-1: cross-construction semantic generalization harness.
#
#   mix run scripts/r004_s1_harness.exs --fixture FILE [--fixture FILE ...] [--out FILE]
#
# READ-ONLY research harness. Nothing here is wired into the pipeline and no
# production setting changes. Each sentence is resolved with the same
# persistence- and learning-disabled options as the P-030 census, then scored
# in three arms:
#
#   arm0  - the unchanged pipeline (SentenceMeaning / participant roles)
#   arm1  - Core.Syntax.Chart with rule_set: :clause_v1 + harness_override,
#           mapped through Core.Semantic.ClauseProposition
#   arm1t - arm1 with each node's dictionary POS restricted to the P-036
#           tagger-consistent POS (ablation only; the tagger is a prior,
#           never an authority)
#
# Fixtures are SYNTHETIC JSON authored independently of the implementer
# (COLLAB §34.8, §35.1). Format:
#
#   {"fixture_id": "...", "partition": "dev" | "heldout",
#    "items": [{"id": "...", "sentence": "...", "twin": "other-id" | null,
#               "expected": {"kind": "clause" | "abstain",
#                            "lemma": "chase", "agent": "dog" | "unexpressed",
#                            "patient": "cat", "tense": "past", "aspect": "simple",
#                            "polarity": "positive", "voice": "active",
#                            "attachment": "subject" | "object" | "predicate" |
#                                          "ambiguous" | null}}]}
#
# agent/patient are HEAD words. Outcome classes per item and arm:
#   correct          one reading (or, for "ambiguous", every attachment exposed),
#                    all gold fields equal
#   provisional      structure equal but lemma unresolved, or the gold reading
#                    is one of several exposed readings
#   abstain          no reading
#   wrong_provisional a non-matching reading explicitly marked provisional
#   wrong_confident  a non-matching reading presented as interpreted, a role
#                    swap, a reading for an abstain control, or an unexposed
#                    attachment choice
# A response that reads well is never evidence; only these fields are scored.

defmodule R004.S1 do
  @resolver_opts [
    mode: :prod,
    enrich_lexicon?: true,
    lexicon_stage?: true,
    response: :off,
    session_id: nil,
    experience_origin: nil,
    fuzzy: false,
    episodes: false,
    persist_episodes: false,
    hippocampus_encode?: false,
    comprehension_wm?: false,
    neuron_activation?: false,
    notify_brain?: false,
    synapse_shadow?: false,
    synaptic_feedback?: false,
    self_model: :off
  ]

  @harness [rule_set: :clause_v1, harness_override: true]
  @fields ~w(agent patient tense aspect polarity voice)

  def main(argv) do
    {opts, _, _} = OptionParser.parse(argv, strict: [fixture: :keep, out: :string])
    paths = Keyword.get_values(opts, :fixture)
    if paths == [], do: raise("usage: --fixture FILE [--fixture FILE ...] [--out FILE]")

    fixtures = Enum.map(paths, &load/1)

    rows =
      for fixture <- fixtures, item <- fixture.items do
        score_item(fixture, item)
      end

    report = %{
      harness: "r004_s1_v1",
      fixtures: Enum.map(fixtures, &Map.take(&1, [:fixture_id, :partition, :sha256, :count])),
      aggregates: aggregates(rows),
      role_swaps: role_swaps(rows),
      chart_ms: timing(rows),
      arm1_abstain_reasons: reasons(rows, "arm1"),
      arm1t_abstain_reasons: reasons(rows, "arm1t"),
      items: rows
    }

    print(report)

    case Keyword.get(opts, :out) do
      nil -> :ok
      out -> File.write!(out, JSON.encode!(report))
    end
  end

  # -- fixtures -----------------------------------------------------------------

  defp load(path) do
    raw = File.read!(path)
    data = JSON.decode!(raw)
    items = Map.fetch!(data, "items")

    %{
      fixture_id: Map.fetch!(data, "fixture_id"),
      partition: Map.fetch!(data, "partition"),
      sha256: :crypto.hash(:sha256, raw) |> Base.encode16(case: :lower),
      count: length(items),
      items: items
    }
  end

  # -- scoring ------------------------------------------------------------------

  defp score_item(fixture, %{"id" => id, "sentence" => sentence, "expected" => expected} = item) do
    si = Core.resolve_input(sentence, @resolver_opts)

    {arm1, arm1_ms, arm1_reason} = chart_arm(si, :full)
    {arm1t, _ms, arm1t_reason} = chart_arm(si, :tagger)

    arms = %{
      "arm0" => arm0_readings(si),
      "arm1" => arm1,
      "arm1t" => arm1t
    }

    %{
      id: id,
      partition: fixture.partition,
      twin: Map.get(item, "twin"),
      sentence: sentence,
      expected: expected,
      chart_ms: arm1_ms,
      reasons: %{"arm1" => arm1_reason, "arm1t" => arm1t_reason},
      outcomes:
        Map.new(arms, fn {arm, readings} ->
          {arm, classify(expected, readings)}
        end)
    }
  end

  defp classify(%{"kind" => "abstain"}, []), do: %{class: "correct", mismatches: []}

  defp classify(%{"kind" => "abstain"}, readings) do
    if Enum.all?(readings, &(&1.status == :provisional)),
      do: %{class: "wrong_provisional", mismatches: ["reading_for_abstain_control"]},
      else: %{class: "wrong_confident", mismatches: ["reading_for_abstain_control"]}
  end

  defp classify(_expected, []), do: %{class: "abstain", mismatches: []}

  defp classify(expected, readings) do
    scored = Enum.map(readings, &{&1, mismatches(expected, &1)})
    matching = Enum.filter(scored, fn {_r, m} -> structural(m) == [] end)
    swap? = Enum.any?(readings, &swapped?(expected, &1))
    attachment = Map.get(expected, "attachment")

    cond do
      swap? ->
        %{class: "wrong_confident", mismatches: ["role_swap"]}

      attachment == "ambiguous" and length(readings) == 1 ->
        %{class: "wrong_confident", mismatches: ["unexposed_attachment_choice"]}

      attachment == "ambiguous" and matching != [] and length(matching) == length(readings) ->
        lemma_class(matching)

      matching == [] ->
        {r, m} = hd(scored)

        if r.status == :provisional,
          do: %{class: "wrong_provisional", mismatches: m},
          else: %{class: "wrong_confident", mismatches: m}

      length(readings) == 1 ->
        lemma_class(matching)

      true ->
        %{class: "provisional", mismatches: ["multiple_readings_exposed"]}
    end
  end

  defp lemma_class(matching) do
    lemma_issues = matching |> Enum.flat_map(fn {_r, m} -> m end) |> Enum.uniq()
    provisional? = Enum.any?(matching, fn {r, _} -> r.status == :provisional end)

    cond do
      "lemma_mismatch" in lemma_issues -> %{class: "wrong_confident", mismatches: lemma_issues}
      lemma_issues != [] or provisional? -> %{class: "provisional", mismatches: lemma_issues}
      true -> %{class: "correct", mismatches: []}
    end
  end

  defp mismatches(expected, reading) do
    field_misses =
      for f <- @fields, Map.has_key?(expected, f), not is_nil(expected[f]),
          to_string(Map.get(reading, String.to_existing_atom(f))) != expected[f],
          do: "#{f}_mismatch"

    attachment_misses =
      case Map.get(expected, "attachment") do
        site when site in ["subject", "object", "predicate"] ->
          if site in reading.attachments, do: [], else: ["attachment_mismatch"]

        _ ->
          []
      end

    lemma_misses =
      case {Map.get(expected, "lemma"), reading.lemma} do
        {nil, _} -> []
        {_, nil} -> ["lemma_unresolved"]
        {same, same} -> []
        _ -> ["lemma_mismatch"]
      end

    field_misses ++ attachment_misses ++ lemma_misses
  end

  defp structural(misses), do: Enum.reject(misses, &(&1 in ["lemma_unresolved", "lemma_mismatch"]))

  defp swapped?(%{"agent" => a, "patient" => p}, reading)
       when is_binary(a) and is_binary(p) and a != p,
       do: to_string(reading.agent) == p and to_string(reading.patient) == a

  defp swapped?(_, _), do: false

  # -- arm 0: the unchanged pipeline ----------------------------------------------

  defp arm0_readings(si) do
    meaning = Map.get(si, :sentence_meaning) || %{}
    words = token_words(si)

    case meaning do
      %{kind: :participant_assertion, source_frame: frame,
        roles: %{actor: %{surface: actor}, patient: %{surface: patient},
                 predicate: %{lemma: lemma}}} ->
        [%{agent: last_word(actor), patient: last_word(patient), lemma: lemma,
           tense: frame_tense(frame), aspect: :simple, polarity: Map.get(meaning, :polarity),
           voice: :active, attachments: [], status: :structurally_interpreted}]

      %{kind: :structural_hypothesis, source_frame: frame, roles: roles} when is_map(roles) ->
        [%{agent: Map.get(words, Map.get(roles, :subject_head)),
           patient: Map.get(words, Map.get(roles, :object_head)), lemma: nil,
           tense: frame_tense(frame), aspect: :simple, polarity: nil, voice: :active,
           attachments: [], status: :provisional}]

      _ ->
        []
    end
  end

  defp frame_tense(frame) when frame in [:finite_past_svo, :finite_past_svo_candidate], do: :past
  defp frame_tense(frame) when frame in [:finite_present_svo, :finite_present_svo_candidate], do: :present
  defp frame_tense(_), do: nil

  # -- arm 1 / 1t: clause rules -----------------------------------------------------

  defp chart_arm(si, mode) do
    nodes = Core.Syntax.Signatures.chart_nodes(si)
    nodes = if mode == :tagger, do: restrict_to_tagger(nodes, si), else: nodes

    possessives =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.filter(&(&1.id == :possessive_noun_phrase))

    {micros, result} = :timer.tc(fn -> Core.Syntax.Chart.diagnose(si, nodes, possessives, @harness) end)

    readings =
      result.signatures
      |> Enum.filter(&(&1.id == :finite_clause))
      |> Enum.flat_map(fn sig ->
        case Core.Semantic.ClauseProposition.build(si, sig) do
          {:ok, p} -> [clause_reading(p)]
          :error -> []
        end
      end)

    {readings, micros / 1000, result.reason}
  end

  defp clause_reading(p) do
    head = fn
      %{head_surface: s} -> String.downcase(s)
      :unexpressed -> "unexpressed"
      _ -> nil
    end

    %{
      agent: head.(p.semantic_roles.agent),
      patient: head.(p.semantic_roles.patient),
      lemma: p.predicate.lemma,
      tense: p.tense,
      aspect: p.aspect,
      polarity: p.polarity,
      voice: p.voice,
      attachments: Enum.map(p.attachments, &to_string(&1.attached_to)),
      status: if(is_nil(p.predicate.lemma), do: :provisional, else: :structurally_interpreted)
    }
  end

  defp restrict_to_tagger(nodes, si) do
    tags =
      si
      |> Map.get(:pos_tags, [])
      |> List.wrap()
      |> Map.new(&{Map.get(&1, :token_index), Map.get(&1, :tag)})

    Enum.map(nodes, fn node ->
      case Map.get(tags, node.index) do
        tag when is_binary(tag) ->
          %{node | pos: MapSet.intersection(node.pos, MapSet.new(allowed(tag)))}

        _ ->
          node
      end
    end)
  end

  defp allowed("auxiliary"), do: ["auxiliary", "verb"]
  defp allowed("particle"), do: ["particle", "adverb"]
  defp allowed("adverb"), do: ["adverb", "particle"]
  defp allowed(tag), do: [tag]

  # -- helpers ----------------------------------------------------------------------

  defp token_words(si) do
    si
    |> Map.get(:tokens, [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{index: i, phrase: p} = t when is_integer(i) and is_binary(p) ->
        if Map.get(t, :n, 1) == 1, do: [{i, String.downcase(p)}], else: []

      _ ->
        []
    end)
    |> Map.new()
  end

  defp last_word(s) when is_binary(s), do: s |> String.split() |> List.last() |> String.downcase()
  defp last_word(_), do: nil

  # -- aggregates ---------------------------------------------------------------------

  @classes ~w(correct provisional abstain wrong_provisional wrong_confident)

  defp aggregates(rows) do
    for partition <- rows |> Enum.map(& &1.partition) |> Enum.uniq(),
        kind <- ["clause", "abstain"],
        arm <- ["arm0", "arm1", "arm1t"],
        subset <- [Enum.filter(rows, &(&1.partition == partition and &1.expected["kind"] == kind))],
        subset != [],
        into: %{} do
      counts = Map.new(@classes, fn c -> {c, Enum.count(subset, &(&1.outcomes[arm].class == c))} end)
      n = length(subset)

      {"#{partition}/#{kind}/#{arm}",
       Map.merge(counts, %{
         "n" => n,
         "correct_or_provisional_rate" =>
           Float.round((counts["correct"] + counts["provisional"]) / n, 4),
         "wrong_confident_rate" => Float.round(counts["wrong_confident"] / n, 4)
       })}
    end
  end

  defp role_swaps(rows) do
    Map.new(["arm0", "arm1", "arm1t"], fn arm ->
      {arm, Enum.count(rows, &("role_swap" in &1.outcomes[arm].mismatches))}
    end)
  end

  defp timing(rows) do
    ms = rows |> Enum.map(& &1.chart_ms) |> Enum.sort()
    n = length(ms)
    pct = fn p -> if n == 0, do: nil, else: Float.round(Enum.at(ms, min(n - 1, trunc(p * n))), 3) end
    %{"p50" => pct.(0.5), "p95" => pct.(0.95), "n" => n}
  end

  defp reasons(rows, arm) do
    rows
    |> Enum.filter(&(&1.outcomes[arm].class == "abstain"))
    |> Enum.frequencies_by(&inspect(&1.reasons[arm]))
  end

  defp print(report) do
    IO.puts("== R-004 S-1 harness (#{report.harness})")
    for f <- report.fixtures, do: IO.puts("fixture #{f.fixture_id} (#{f.partition}, n=#{f.count}) sha256=#{f.sha256}")

    for {key, a} <- Enum.sort(report.aggregates) do
      IO.puts(
        "#{String.pad_trailing(key, 26)} n=#{a["n"]} correct=#{a["correct"]} provisional=#{a["provisional"]} " <>
          "abstain=#{a["abstain"]} wrong_prov=#{a["wrong_provisional"]} wrong_conf=#{a["wrong_confident"]} " <>
          "c+p=#{a["correct_or_provisional_rate"]} wc=#{a["wrong_confident_rate"]}"
      )
    end

    IO.puts("role swaps: #{inspect(report.role_swaps)}")
    IO.puts("chart ms: #{inspect(report.chart_ms)}")
    IO.puts("arm1 abstain reasons: #{inspect(report.arm1_abstain_reasons)}")
    IO.puts("arm1t abstain reasons: #{inspect(report.arm1t_abstain_reasons)}")
  end
end

R004.S1.main(System.argv())
