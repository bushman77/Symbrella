# LU-00: initial language-understanding baseline.
#
# Usage from ~/Symbrella (development branch):
#   mix run scripts/benchmark_lu00.exs --list
#   mix run scripts/benchmark_lu00.exs --details
#   mix run scripts/benchmark_lu00.exs --save
#   mix run scripts/benchmark_lu00.exs --export-cases
#   mix run scripts/benchmark_lu00.exs --cases priv/lu00_cases.json --save
#
# Observation-only: Core.Social.SemanticProbe does not mutate cognitive memory,
# synaptic weights, episodes, or perform LLM requests. This is its deliberately
# restricted shadow Stage1 configuration, NOT production Core.resolve_input/2.
# No claim of full production accuracy should be made from these results.
#
# Exact sense evaluation requires HUMAN-REVIEWED numbered dictionary IDs.
# A gloss alone is not an independently checked numbered-sense label.
# Missing structure is a coverage gap, never a correct abstention by default.
#
# This source contains only synthetic evaluation prompts. Private annotations
# and runtime results remain under the umbrella-root priv/ directory.

defmodule Symbrella.LU00Benchmark do
  @moduledoc false

  # {id, category, pair, sentence, target, expected_pos, intended_gloss, expected}
  # All examples are provisional until independently reviewed. The expected
  # map describes independently intended semantic properties; it does NOT
  # pretend to be a verified numbered dictionary-sense ID.
  @rows [
    {"L001", "sense", "bank", "I deposited money at the bank.", "bank", "noun", "financial institution", %{}},
    {"L002", "sense", "bank", "I sat by the river bank.", "bank", "noun", "edge of a river", %{}},
    {"L003", "sense", "bat", "A bat flew out of the cave.", "bat", "noun", "flying mammal", %{}},
    {"L004", "sense", "bat", "He swung the bat at the ball.", "bat", "noun", "sporting implement", %{}},
    {"L005", "sense", "light", "She packed a light suitcase.", "light", "adjective", "not heavy", %{}},
    {"L006", "sense", "light", "The light filled the room.", "light", "noun", "illumination", %{}},
    {"L007", "sense", "draw", "They draw a picture.", "draw", "verb", "make a picture", %{}},
    {"L008", "sense", "draw", "They draw water from a well.", "draw", "verb", "extract water", %{}},
    {"L009", "sense", "seal", "A seal swam past the boat.", "seal", "noun", "marine mammal", %{}},
    {"L010", "sense", "seal", "The seal on the envelope was broken.", "seal", "noun", "closure or impression", %{}},
    {"L011", "sense", "spring", "Spring arrives after winter.", "spring", "noun", "season", %{}},
    {"L012", "sense", "spring", "The metal spring compressed.", "spring", "noun", "elastic coil", %{}},

    {"P001", "pos", "lead", "The lead pipe was heavy.", "lead", "noun", "metal", %{}},
    {"P002", "pos", "lead", "She will lead the team.", "lead", "verb", "guide", %{}},
    {"P003", "pos", "watch", "The watch stopped working.", "watch", "noun", "timepiece", %{}},
    {"P004", "pos", "watch", "Watch the road carefully.", "watch", "verb", "observe attentively", %{}},
    {"P005", "pos", "record", "The record contains the result.", "record", "noun", "stored information", %{}},
    {"P006", "pos", "record", "They record the results.", "record", "verb", "document information", %{}},

    {"R001", "roles", "chase", "The dog chased the cat.", nil, nil, nil, %{"subject" => "dog", "predicate" => "chased", "object" => "cat"}},
    {"R002", "roles", "chase", "The cat chased the dog.", nil, nil, nil, %{"subject" => "cat", "predicate" => "chased", "object" => "dog"}},
    {"R003", "roles", "help", "The teacher helped the student.", nil, nil, nil, %{"subject" => "teacher", "predicate" => "helped", "object" => "student"}},
    {"R004", "roles", "help", "The student helped the teacher.", nil, nil, nil, %{"subject" => "student", "predicate" => "helped", "object" => "teacher"}},
    {"R005", "roles", "call", "The captain called the mechanic.", nil, nil, nil, %{"subject" => "captain", "predicate" => "called", "object" => "mechanic"}},
    {"R006", "roles", "call", "The mechanic called the captain.", nil, nil, nil, %{"subject" => "mechanic", "predicate" => "called", "object" => "captain"}},

    {"N001", "polarity", "dog_chase", "The dog chased the cat.", nil, nil, nil, %{"polarity" => "positive"}},
    {"N002", "polarity", "dog_chase", "The dog did not chase the cat.", nil, nil, nil, %{"polarity" => "negative"}},
    {"N003", "polarity", "reviewer_verify", "The reviewer verified the report.", nil, nil, nil, %{"polarity" => "positive"}},
    {"N004", "polarity", "reviewer_verify", "The reviewer did not verify the report.", nil, nil, nil, %{"polarity" => "negative"}},
    {"N005", "polarity", "student_support", "The student supported the proposal.", nil, nil, nil, %{"polarity" => "positive"}},
    {"N006", "polarity", "student_support", "The student did not support the proposal.", nil, nil, nil, %{"polarity" => "negative"}},

    {"V001", "voice", "chase_voice", "The dog chased the cat.", nil, nil, nil, %{"subject" => "dog", "object" => "cat"}},
    {"V002", "voice", "chase_voice", "The cat was chased by the dog.", nil, nil, nil, %{"subject" => "dog", "object" => "cat"}},
    {"V003", "voice", "write_voice", "The author wrote the letter.", nil, nil, nil, %{"subject" => "author", "object" => "letter"}},
    {"V004", "voice", "write_voice", "The letter was written by the author.", nil, nil, nil, %{"subject" => "author", "object" => "letter"}},

    {"M001", "modality", "might", "The dog chased the cat.", nil, nil, nil, %{"modality" => "asserted"}},
    {"M002", "modality", "might", "The dog might chase the cat.", nil, nil, nil, %{"modality" => "possible"}},
    {"M003", "modality", "future", "The workers completed the bridge.", nil, nil, nil, %{"modality" => "asserted"}},
    {"M004", "modality", "future", "The workers might complete the bridge tomorrow.", nil, nil, nil, %{"modality" => "possible"}},

    {"E001", "embedding", "reported", "Alice said that the dog chased the cat.", nil, nil, nil, %{"attribution" => "Alice"}},
    {"E002", "embedding", "reported", "Alice said that the cat chased the dog.", nil, nil, nil, %{"attribution" => "Alice"}},
    {"E003", "embedding", "belief", "The teacher believes that the student passed.", nil, nil, nil, %{"attribution" => "teacher"}},
    {"E004", "embedding", "belief", "The student believes that the teacher passed.", nil, nil, nil, %{"attribution" => "student"}},

    {"W001", "mwe", "kick_bucket", "After a long illness, he kicked the bucket.", "kick the bucket", "phrase", "idiom meaning died", %{}},
    {"W002", "mwe", "kick_bucket", "He kicked the bucket across the yard.", "kick the bucket", "phrase", "literal physical kick", %{}},
    {"W003", "mwe", "break_ice", "She told a joke to break the ice.", "break the ice", "phrase", "ease social awkwardness", %{}},
    {"W004", "mwe", "break_ice", "She used a hammer to break the ice.", "break the ice", "phrase", "fracture frozen water", %{}},

    {"C001", "coreference", "dog", "It chased the cat.", nil, nil, nil, %{"context" => "I saw a big dog.", "referent" => "dog"}},
    {"C002", "coreference", "dog", "It ran under the table.", nil, nil, nil, %{"context" => "I saw a big cat.", "referent" => "cat"}},
    {"C003", "coreference", "person", "She opened the door.", nil, nil, nil, %{"context" => "Maya came home.", "referent" => "Maya"}},
    {"C004", "coreference", "person", "He opened the door.", nil, nil, nil, %{"context" => "Daniel came home.", "referent" => "Daniel"}},

    {"A001", "ambiguity", "bank", "I went to the bank.", "bank", "noun", "intentionally underspecified sense", %{"sense_ambiguous" => true}},
    {"A002", "ambiguity", "bat", "I found a bat.", "bat", "noun", "intentionally underspecified sense", %{"sense_ambiguous" => true}},
    {"A003", "ambiguity", "light", "The light was unusual.", "light", "noun", "underspecified use", %{"sense_ambiguous" => true}},
    {"A004", "ambiguity", "crane", "I looked at the crane.", "crane", "noun", "bird or lifting machine", %{"sense_ambiguous" => true}}
  ]

  @priv Path.expand("priv", File.cwd!())

  def run(args) do
    {opts, rest, errors} =
      OptionParser.parse(args,
        strict: [list: :boolean, details: :boolean, save: :boolean,
                 export_cases: :boolean, cases: :string]
      )

    if errors != [] or rest != [] do
      raise ArgumentError, "Unknown arguments: #{inspect({rest, errors})}"
    end

    ensure_priv!()
    cases = if opts[:cases], do: import_cases!(opts[:cases]), else: default_cases()
    validate!(cases)

    if opts[:export_cases] do
      target = Path.join(@priv, "lu00_cases.json")

      if File.exists?(target) do
        raise ArgumentError, "Refusing to overwrite existing annotations: #{target}"
      end

      File.write!(target, Jason.encode!(%{"version" => 1, "cases" => cases}, pretty: true))
      IO.puts("Exported provisional cases for PRIVATE human review: #{target}")
    end

    IO.puts("\n=== LU-00: read-only semantic shadow baseline ===")
    IO.puts("Cases: #{length(cases)}; provisional annotations; no self-training.")
    IO.puts("Adapter: Core.Social.SemanticProbe (NOT full production Core.resolve_input).")
    IO.puts("Neither lexical candidate recall nor exact-sense accuracy is inferred from glosses.")
    IO.puts("Cross-turn cases are registered but NOT evaluated by the single-turn adapter.\n")

    if opts[:list] do
      Enum.each(cases, fn c ->
        IO.puts("#{c["id"]} [#{c["category"]}] #{c["sentence"]}")
      end)
    else
      results = Enum.map(cases, &evaluate/1)
      freq = Enum.frequencies(Enum.map(results, & &1["classification"]))
      reviewed = Enum.count(cases, &(&1["annotation_status"] == "reviewed"))
      summary = %{
        "version" => 1,
        "adapter" => "social_shadow_read_only_not_production",
        "cases" => length(cases),
        "reviewed_annotations" => reviewed,
        "classifications" => freq,
        "exact_sense_accuracy" => "not_measured_without_reviewed_gold_and_full_slate",
        "production_comprehension_accuracy" => "not_measured"
      }

      Enum.each(results, fn row ->
        IO.puts("#{row["id"]} [#{row["category"]}] #{row["classification"]}")
        if opts[:details], do: IO.inspect(row, pretty: true, limit: :infinity)
      end)

      IO.inspect(summary, label: "LU-00 SUMMARY", pretty: true)
      if opts[:save] do
        target = Path.join(@priv, "lu00_baseline.json")
        payload = %{
          "summary" => summary,
          "cases_sha256" => digest(cases),
          "records" => results
        }
        File.write!(target, Jason.encode!(payload, pretty: true))
        IO.puts("Wrote private, bounded diagnostic report: #{target}")
      end
    end
  end

  defp default_cases do
    Enum.map(@rows, fn {id, category, pair, sentence, target, pos, gloss, expected} ->
      %{
        "id" => id,
        "category" => category,
        "pair" => pair,
        "sentence" => sentence,
        "target_word" => target,
        "expected_pos" => pos,
        "intended_gloss" => gloss,
        "expected" => expected,
        "gold_id" => nil,
        "annotation_status" => "provisional"
      }
    end)
  end

  defp import_cases!(input) do
    file = Path.expand(input, File.cwd!())
    unless String.starts_with?(file, @priv <> "/") do
      raise ArgumentError, "--cases input must be under the existing umbrella-root priv/"
    end

    file
    |> File.read!()
    |> Jason.decode!()
    |> Map.fetch!("cases")
  end

  defp validate!(cases) when is_list(cases) and length(cases) in 40..200 do
    ids = Enum.map(cases, &Map.fetch!(&1, "id"))

    valid? = length(Enum.uniq(ids)) == length(ids) and
      Enum.all?(cases, fn c ->
        is_binary(c["id"]) and is_binary(c["category"]) and
          is_binary(c["sentence"]) and byte_size(c["sentence"]) <= 400 and
          c["annotation_status"] in ["provisional", "reviewed"] and
          (is_nil(c["gold_id"]) or is_binary(c["gold_id"])) and
          is_map(c["expected"])
      end)

    unless valid?, do: raise(ArgumentError, "Invalid LU-00 case annotations")
    :ok
  end

  defp validate!(_), do: raise(ArgumentError, "Expected 40–200 LU-00 cases")

  defp evaluate(c) do
    base = %{
      "id" => c["id"],
      "category" => c["category"],
      "pair" => c["pair"],
      "sentence" => c["sentence"],
      "annotation_status" => c["annotation_status"]
    }

    # The read-only adapter intentionally has no per-session discourse memory.
    if Map.has_key?(c["expected"], "context") do
      Map.merge(base, %{
        "classification" => "not_evaluated_cross_turn",
        "reason" => "single_turn_adapter_no_discourse_history",
        "expected" => c["expected"]
      })
    else
      case Core.Social.SemanticProbe.run(c["sentence"], emit_telemetry?: false) do
        {:ok, observation} ->
          inspect_observation(base, c, observation)

        {:error, reason} ->
          Map.merge(base, %{
            "classification" => "infrastructure_failure",
            "reason" => to_string(reason)
          })
      end
    end
  end

  defp inspect_observation(base, c, observation) do
    dictionary = get_in(observation, [:syntax, :audit, :ltm_status])
    lifg = get_in(observation, [:lifg, :status])
    proposition = Map.get(observation, :reported_proposition) || %{}
    sentence_meaning = Map.get(observation, :sentence_meaning) || %{}
    lexical = lexical_diagnostic(c, observation)
    structural = structural_diagnostic(c, proposition)

    classification =
      cond do
        dictionary in [:lookup_failed, :disabled] or lifg != :scored ->
          "infrastructure_failure"

        dictionary == :no_matches ->
          "lexical_inventory_gap"

        Map.get(c["expected"], "sense_ambiguous") == true ->
          "ambiguity_requires_review"

        c["category"] == "mwe" ->
          "not_evaluated_mwe_slate_hidden"

        lexical["result"] == "target_unresolved" ->
          "lexical_target_missing"

        lexical["result"] == "no_choice" ->
          "lexical_no_winner"

        lexical["result"] == "pos_mismatch" ->
          "pos_selection_mismatch"

        lexical["result"] == "reviewed_gold_mismatch" ->
          "sense_selection_mismatch_recall_unknown"

        structural["result"] == "mismatch" ->
          "compositional_mismatch"

        structural["result"] == "coverage_gap" ->
          "structural_coverage_gap"

        structural["result"] == "match" ->
          "observed_structural_match"

        lexical["result"] == "pos_match" ->
          "pos_match_sense_unscored"

        true ->
          "diagnostic_only"
      end

    Map.merge(base, %{
      "classification" => classification,
      "dictionary_status" => as_text(dictionary),
      "lifg_status" => as_text(lifg),
      "sentence_meaning_status" => as_text(Map.get(sentence_meaning, :status)),
      "proposition_status" => as_text(Map.get(proposition, :status)),
      "lexical" => lexical,
      "structural" => structural
    })
  end

  defp lexical_diagnostic(c, observation) do
    word = c["target_word"]
    gold_id = if c["annotation_status"] == "reviewed", do: c["gold_id"], else: nil

    if not is_binary(word) do
      %{"result" => "not_targeted"}
    else
      # Uses the same basic lexical tokenizer as the shadow probe. Do not infer
      # a Stage1 *slate* from the dictionary inventory or the truncated choices.
      tokens = Core.LIFG.Input.tokenize(c["sentence"], fuzzy: false, max_wordgram_n: 1)
      indices =
        tokens
        |> Enum.filter(fn token ->
          Map.get(token, :n, 1) == 1 and
            normalize(Map.get(token, :phrase)) == normalize(word)
        end)
        |> Enum.map(&Map.get(&1, :index))

      [index] = if length(indices) == 1, do: indices, else: [nil]
      choice = Enum.find(get_in(observation, [:lifg, :choices]) || [],
        &(is_integer(index) and &1.token_index == index))
      id = choice && Map.get(choice, :chosen_id)

      selected_pos =
        case id && String.split(id, "|", parts: 3) do
          [_, pos, _] -> pos
          _ -> nil
        end

      result =
        cond do
          not is_integer(index) -> "target_unresolved"
          is_nil(id) -> "no_choice"
          is_binary(gold_id) and id != gold_id -> "reviewed_gold_mismatch"
          is_binary(gold_id) and id == gold_id -> "reviewed_gold_match"
          is_binary(c["expected_pos"]) and selected_pos != c["expected_pos"] -> "pos_mismatch"
          is_binary(c["expected_pos"]) -> "pos_match"
          true -> "not_scored"
        end

      %{
        "result" => result,
        "target" => word,
        "selected_id" => id,
        "selected_pos" => selected_pos,
        "expected_pos" => c["expected_pos"],
        "gold_id" => gold_id,
        "candidate_recall" => "not_measured_stage1_slate_not_exposed",
        "margin" => if(choice, do: Map.get(choice, :margin), else: nil),
        "intended_gloss" => c["intended_gloss"]
      }
    end
  end

  defp structural_diagnostic(c, proposition) do
    keys = ~w(subject predicate object polarity relation count)
    expected = Map.take(c["expected"], keys)

    if map_size(expected) == 0 do
      %{"result" => "not_targeted"}
    else
      if Map.get(proposition, :status) != :attributed_structural_interpretation do
        %{"result" => "coverage_gap", "expected" => expected,
          "reason" => as_text(Map.get(proposition, :reason))}
      else
        observed =
          Enum.into(Map.keys(expected), %{}, fn key ->
            {key, as_text(observed_field(proposition, key))}
          end)

        mismatches =
          expected
          |> Enum.filter(fn {key, value} ->
            normalize(Map.get(observed, key)) != normalize(value)
          end)
          |> Enum.map(&elem(&1, 0))

        %{
          "result" => if(mismatches == [], do: "match", else: "mismatch"),
          "expected" => expected,
          "observed" => observed,
          "mismatch_fields" => mismatches
        }
      end
    end
  end

  defp observed_field(proposition, "subject"), do: Map.get(proposition, :subject)
  defp observed_field(proposition, "predicate"), do: Map.get(proposition, :predicate)
  defp observed_field(proposition, "object"), do: Map.get(proposition, :object)
  defp observed_field(proposition, "polarity"), do: Map.get(proposition, :polarity)
  defp observed_field(proposition, "relation"), do: Map.get(proposition, :relation)
  defp observed_field(proposition, "count"), do: Map.get(proposition, :reported_count)

  defp normalize(nil), do: ""

  defp normalize(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/[^\p{L}\p{N}]+/u, " ")
    |> String.replace(~r/\A(?:the|a|an)\s+/u, "")
    |> String.trim()
  end

  defp normalize(value), do: value |> as_text() |> normalize()

  defp as_text(nil), do: nil
  defp as_text(value) when is_atom(value), do: Atom.to_string(value)
  defp as_text(value) when is_binary(value), do: value
  defp as_text(value) when is_number(value), do: to_string(value)
  defp as_text(value), do: inspect(value, limit: 4)

  defp ensure_priv! do
    unless File.dir?(@priv) do
      raise ArgumentError, "Existing umbrella-root priv/ directory is required; refusing to create folders"
    end
  end

  defp digest(cases) do
    cases
    |> Jason.encode!()
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end
end

Symbrella.LU00Benchmark.run(System.argv())
