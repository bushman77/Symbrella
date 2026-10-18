# Production-pipeline LIFG word-sense audit. No user episodic writes.
#
#   mix run scripts/benchmark_lifg_senses.exs
#   mix run scripts/benchmark_lifg_senses.exs --sentence "I deposited cash at the bank." --word bank
#   mix run scripts/benchmark_lifg_senses.exs --gold tmp/lifg_gold.jsonl
#
# Gold JSONL: one record per sentence, e.g.
# {"sentence":"I deposited cash at the bank.","word":"bank",
#  "expected_pos":"noun","expected_id":"bank|noun|<REVIEWED_REAL_ID>",
#  "meaning":"financial institution"}
# expected_id must be verified by a human against YOUR current dictionary.
# The default examples have interpretation prompts, NOT numbered-sense gold.
#
# Unlike lexical inventory/shadow audits, this runs Core.resolve_input with
# production lexical options and inspects actual Stage1 and final LIFG choices.
# Response/episode persistence and session-scoped WM updates are disabled.
# Like other production pipeline probes, it may still emit Brain telemetry or
# transient runtime state. No training or correctness claims are made here.

defmodule Symbrella.BenchmarkLIFGSenses do
  @cases [
    %{
      sentence: "I deposited cash at the bank.", word: "bank",
      expected_pos: "noun", meaning: "financial institution"
    },
    %{
      sentence: "We sat on the bank of the river.", word: "bank",
      expected_pos: "noun", meaning: "edge of a river"
    },
    %{
      sentence: "A bat flew through the cave.", word: "bat",
      expected_pos: "noun", meaning: "flying mammal"
    },
    %{
      sentence: "He swung the bat at the ball.", word: "bat",
      expected_pos: "noun", meaning: "sporting implement"
    },
    %{
      sentence: "They draw a picture.", word: "draw",
      expected_pos: "verb", meaning: "make a picture by drawing"
    },
    %{
      sentence: "They draw water from a well.", word: "draw",
      expected_pos: "verb", meaning: "take water out of a well"
    },
    %{
      sentence: "She carried a light suitcase.", word: "light",
      expected_pos: "adjective", meaning: "not heavy"
    },
    %{
      sentence: "The light filled the room.", word: "light",
      expected_pos: "noun", meaning: "illumination"
    }
  ]

  def run(argv) do
    {opts, rest, errors} =
      OptionParser.parse(argv,
        strict: [sentence: :string, word: :string, gold: :string, help: :boolean,
                 grammar_weight: :float, sense_focus: :boolean]
      )

    if errors != [] or rest != [] do
      IO.inspect({rest, errors}, label: "Invalid arguments")
      System.halt(2)
    end

    if opts[:help] do
      IO.puts("Usage: mix run scripts/benchmark_lifg_senses.exs [--sentence TEXT --word WORD | --gold FILE] [--grammar-weight FLOAT] [--no-sense-focus]")
    else
      cases = cases(opts)
      IO.puts("\n=== Current production LIFG word-sense evaluation ===")
      IO.puts("NO episode writes. Stage1 probabilities are per-token competitive scores, not correctness probabilities.")
      IO.puts("Default meaning descriptions are prompts for human inspection, NOT numbered-sense gold labels.")
      IO.puts("A real exact-sense accuracy figure requires independently reviewed expected_id values.")
      IO.inspect(%{grammar_weight_override: opts[:grammar_weight],
                    sense_focus_override: opts[:sense_focus],
                   scores_override: :all,
                   trace_word_scores?: true},
        label: "Benchmark lexical options (nil = configured production default)")
      IO.puts("")

      results = Enum.map(cases, &evaluate(&1, opts))
      summarize(results)
    end
  end

  defp cases(opts) do
    cond do
      is_binary(opts[:gold]) and (opts[:sentence] || opts[:word]) ->
        raise ArgumentError, "--gold cannot be mixed with --sentence or --word"

      is_binary(opts[:gold]) ->
        opts[:gold]
        |> File.stream!([], :line)
        |> Enum.with_index(1)
        |> Enum.flat_map(fn {line, number} ->
          if String.trim(line) == "" do
            []
          else
            case Jason.decode(line) do
              {:ok, %{"sentence" => sentence, "word" => word} = data}
                  when is_binary(sentence) and is_binary(word) ->
                [%{
                  sentence: sentence,
                  word: word,
                  expected_pos: Map.get(data, "expected_pos"),
                  expected_id: Map.get(data, "expected_id"),
                  meaning: Map.get(data, "meaning")
                }]

              other ->
                raise ArgumentError, "Invalid gold JSONL line #{number}: #{inspect(other)}"
            end
          end
        end)

      is_binary(opts[:sentence]) and is_binary(opts[:word]) ->
        [%{sentence: opts[:sentence], word: opts[:word], meaning: nil,
           expected_pos: nil, expected_id: nil}]

      opts[:sentence] || opts[:word] ->
        raise ArgumentError, "Supply both --sentence and --word"

      true ->
        @cases
    end
  end

  defp evaluate(item, opts) do
    sentence = String.trim(item.sentence)
    word = String.downcase(String.trim(item.word))
    cells = Db.brain_cells_for_norm(word) |> List.wrap()
    by_id = cells |> Enum.filter(&is_map/1) |> Map.new(&{Map.get(&1, :id), &1})

    # Keep the actual configured grammar policy by default. The previous
    # benchmark unconditionally forced grammar_weight: 1.0, which made the
    # so-called production baseline a special experiment rather than the
    # normal production lexical path. An explicit flag enables comparison.
    lifg_opts =
      [scores: :all, trace_word_scores?: true]
      |> then(fn base ->
        case opts[:grammar_weight] do
          weight when is_number(weight) -> Keyword.put(base, :grammar_weight, weight)
          _ -> base
        end
      end)
      |> then(fn base ->
        if Keyword.has_key?(opts, :sense_focus),
          do: Keyword.put(base, :sense_focus?, opts[:sense_focus]),
          else: base
      end)

    si =
      Core.resolve_input(sentence,
        mode: :prod,
        enrich_lexicon?: true,
        lexicon_stage?: true,
        response: :off,
        persist_episodes: false,
        comprehension_wm?: false,
        lifg_opts: lifg_opts
      )

    target_indices =
      si.tokens
      |> List.wrap()
      |> Enum.filter(fn t ->
        Map.get(t, :n, 1) == 1 and
          normalize(Map.get(t, :phrase, "")) == word
      end)
      |> Enum.map(&Map.get(&1, :index))

    if length(target_indices) != 1 do
      IO.puts("\n--- #{sentence} ---")
      IO.inspect(%{word: word, token_matches: target_indices,
                   status: :target_missing_or_ambiguous},
        label: "SENSE TEST BLOCKED")
      %{grade: :ungradable, pos_grade: :ungradable, note: :target_missing}
    else
      [index] = target_indices
      final = Enum.find(si.lifg_choices || [], &(Map.get(&1, :token_index) == index))
      report =
        si.trace
        |> Core.LIFG.ScoreAudit.trace_reports()
        |> List.first()
        |> case do
          %{entries: entries} -> Enum.find(entries, &(Map.get(&1, :token_index) == index))
          _ -> nil
        end

      final_id = if is_map(final), do: Map.get(final, :id), else: nil
      raw_id = if is_map(report), do: Map.get(report, :raw_winner), else: nil

      # Core intentionally omits candidate_probs when the slate exceeds 64
      # senses. The raw Stage1 audit preserves the actual top eight, so the
      # benchmark can show evidence even for 156-way "draw" competitions.
      probs = if is_map(final), do: Map.get(final, :candidate_probs), else: nil

      ranked =
        cond do
          is_map(probs) and map_size(probs) > 0 ->
            probs
            |> Enum.filter(fn {id, p} -> is_binary(id) and is_number(p) end)
            |> Enum.sort_by(fn {id, p} -> {-p, id} end)

          is_map(report) ->
            report
            |> Map.get(:reported_top_candidates, [])
            |> Enum.flat_map(fn
              %{id: id, probability: p} when is_binary(id) and is_number(p) ->
                [{id, p}]
              _ -> []
            end)

          true ->
            []
        end

      rows =
        ranked
        |> Enum.take(8)
        |> Enum.map(fn {id, p} ->
          cell = Map.get(by_id, id, %{})
          %{
            id: id,
            probability: Float.round(p * 1.0, 6),
            definition: compact(Map.get(cell, :definition), 190),
            context_fit:
              if(map_size(cell) > 0,
                do: Brain.LIFG.ContextFit.score(sentence, word, cell),
                else: nil)
          }
        end)

      target_token =
        Enum.find(si.tokens || [], fn token ->
          Map.get(token, :index) == index and Map.get(token, :n, 1) == 1
        end)

      covering_mwes =
        case Map.get(target_token || %{}, :span) do
          {start, len} when is_integer(start) and is_integer(len) ->
            si.tokens
            |> List.wrap()
            |> Enum.filter(fn token ->
              case Map.get(token, :span) do
                {mwe_start, mwe_len}
                    when is_integer(mwe_start) and is_integer(mwe_len) ->
                  Map.get(token, :n, 1) > 1 and mwe_start <= start and
                    start + len <= mwe_start + mwe_len
                _ -> false
              end
            end)
            |> Enum.take(6)
            |> Enum.map(&Map.take(&1, [:index, :phrase, :span, :n]))

          _ -> []
        end

      mwe_decisions =
        si
        |> Map.get(:mwe_choices, [])
        |> List.wrap()
        |> Enum.filter(&is_map/1)
        |> Enum.take(8)
        |> Enum.map(&Map.take(&1,
          [:token_index, :id, :lemma, :score, :token_span, :span, :source]))

      # The final token list does not necessarily contain Core's virtual MWE
      # token. Check the actual accepted semantic choices and their original
      # base-token spans instead of claiming an empty token cover means no MWE.
      covering_semantic_choices =
        si
        |> Map.get(:semantic_choices, [])
        |> List.wrap()
        |> Enum.filter(fn
          %{kind: :mwe, token_span: {first, last}} ->
            is_integer(first) and is_integer(last) and index >= first and
              index <= last
          _ ->
            false
        end)
        |> Enum.take(8)
        |> Enum.map(&Map.take(&1,
          [:token_index, :id, :phrase, :token_span, :source, :score]))

      # Observe candidate signatures that *claim* this word as a grammatical
      # role, including conflicting interpretations. No hypothesis is promoted
      # by this report or assumed to be correct merely because it exists.
      impacting_signatures =
        si
        |> Map.get(:sentence_signatures, [])
        |> List.wrap()
        |> Enum.filter(fn
          %{roles: roles} when is_map(roles) ->
            Enum.any?(roles, fn {_name, role_index} -> role_index == index end)
          _ ->
            false
        end)
        |> Enum.take(16)
        |> Enum.map(&Map.take(&1,
          [:id, :span, :roles, :required_features, :evidence, :source]))

      required_pos = Core.LIFG.SignatureRoles.required_pos(si, index)

      structural_pos =
        si
        |> Map.get(:role_competition)
        |> case do
          %{roles: roles} when is_map(roles) -> Map.get(roles, index)
          _ -> nil
        end

      word_flow =
        si.trace
        |> List.wrap()
        |> Enum.find_value(fn
          %{stage: :lifg_word_flow_audit, meta: %{} = meta} -> meta
          _ -> nil
        end)

      word_flow_for_target =
        if is_map(word_flow) do
          for {name, key} <- [
                {:before_shadow, :before_shadow},
                {:after_shadow, :after_shadow},
                {:after_promotion, :after_promotion}
              ], into: %{} do
            entries = Map.get(word_flow, key, [])
            {name, Enum.find(entries, fn e ->
              Map.get(e, :token_index) == index or
                (case Map.get(e, :token_span) do
                   {first, last} when is_integer(first) and is_integer(last) ->
                     first <= index and index <= last
                   _ -> false
                 end)
            end)}
          end
        else
          %{audit: :not_available}
        end

      # A candidate can claim an object slot even when the proposed
      # object is independently attested as a finite verb.
      following_finite_past =
        si
        |> Map.get(:sentence_signatures, [])
        |> List.wrap()
        |> Enum.filter(fn
          %{id: :finite_past_candidate, roles: %{verb: verb}} ->
            verb == index + 1
          _ ->
            false
        end)
        |> Enum.map(&Map.take(&1, [:id, :span, :roles, :evidence]))

      expected_id = Map.get(item, :expected_id)
      expected_pos = Map.get(item, :expected_pos)
      valid_gold? = is_binary(expected_id) and Map.has_key?(by_id, expected_id)
      grade =
        cond do
          is_nil(expected_id) -> :not_labeled
          not valid_gold? -> :invalid_gold_id
          is_nil(final_id) -> :no_final_choice
          final_id == expected_id -> :correct
          true -> :incorrect
        end

      pos = sense_pos(final_id)
      pos_grade =
        cond do
          not is_binary(expected_pos) -> :not_labeled
          is_nil(final_id) -> :no_final_choice
          pos == expected_pos -> :correct
          true -> :incorrect
        end

      IO.puts("\n--- #{sentence} ---")
      IO.inspect(%{
        target: word, index: index, intended_meaning: item.meaning,
        expected_pos: expected_pos, pos_result: pos_grade,
        expected_id: expected_id, exact_sense_result: grade,
        stage1_winner: raw_id, final_winner: final_id,
        stage1_winner_definition:
          compact(Map.get(Map.get(by_id, raw_id, %{}), :definition), 190),
        grammar_required_pos: required_pos,
        adjudicated_structural_pos: structural_pos,
        context_focus: if(is_map(final), do: Map.get(final, :sense_focus), else: nil),
        mwe_covering_target: covering_mwes,
        mwe_semantic_choices_covering_target: covering_semantic_choices,
        mwe_decisions: if(is_nil(final_id), do: mwe_decisions, else: []),
        grammar_signatures_for_target: impacting_signatures,
        following_finite_past_hypotheses: following_finite_past,
        word_flow_for_target: word_flow_for_target,
        final_winner_definition:
          compact(Map.get(Map.get(by_id, final_id, %{}), :definition), 240),
        changed_after_stage1?: is_binary(raw_id) and raw_id != final_id,
        stage1_top_probability: if(is_map(report), do: report.reported_top_probability),
        stage1_margin: if(is_map(report), do: report.reported_probability_margin),
        stage1_candidate_count:
          if(is_map(report), do: report.reported_candidate_count),
        stage1_pos_mass: if(is_map(report), do: report.pos_probability_mass),
        full_score_map?: is_map(probs) and map_size(probs) > 0,
        top_senses_from: if(is_map(probs) and map_size(probs) > 0,
          do: :final_full_distribution, else: :bounded_stage1_audit),
        scored_gold_rank:
          if(valid_gold?, do: rank(ranked, expected_id), else: nil),
        gold_rank_limited?: valid_gold? and not (is_map(probs) and map_size(probs) > 0)
      }, label: "Current LIFG decision", pretty: true, width: 115, limit: :infinity)
      IO.inspect(rows, label: "Top scored dictionary senses", pretty: true,
        width: 115, limit: :infinity)

      if is_nil(final_id) do
        IO.puts("No final unigram choice. See MWE coverage/decisions; this case is not a valid POS decision.")
      end

      %{grade: grade, pos_grade: pos_grade,
        context_focus: if(is_map(final), do: Map.get(final, :sense_focus), else: nil),
        note: if(is_nil(report), do: :raw_audit_missing)}
    end
  end

  defp summarize(results) do
    reviewed = Enum.filter(results, &(&1.grade in [:correct, :incorrect, :no_final_choice]))
    pos = Enum.filter(results, &(&1.pos_grade in [:correct, :incorrect]))
    invalid = Enum.count(results, &(&1.grade == :invalid_gold_id))
    missed = Enum.count(results, &(&1.pos_grade == :no_final_choice))
    focuses = results |> Enum.map(&Map.get(&1, :context_focus)) |> Enum.filter(&is_map/1)

    IO.puts("\n=== Results ===")
    IO.inspect(%{
      cases: length(results),
      with_reviewed_gold_id: length(reviewed),
      exact_sense_correct: Enum.count(reviewed, &(&1.grade == :correct)),
      exact_sense_accuracy:
        if(reviewed == [], do: :not_measured,
          else: Enum.count(reviewed, &(&1.grade == :correct)) / length(reviewed)),
      pos_selections_scored: length(pos),
      pos_correct: Enum.count(pos, &(&1.pos_grade == :correct)),
      pos_accuracy_among_selected:
        if(pos == [], do: :not_measured,
          else: Enum.count(pos, &(&1.pos_grade == :correct)) / length(pos)),
      no_final_unigram_choice: missed,
      all_cases_correct_pos_and_selected:
        Enum.count(pos, &(&1.pos_grade == :correct)),
      all_cases_pos_and_selection_rate:
        if(results == [], do: :not_measured,
          else: Enum.count(pos, &(&1.pos_grade == :correct)) / length(results)),
      focused_tokens: Enum.count(focuses, &(&1.mode == :focused)),
      focused_context_evaluations: Enum.sum(Enum.map(focuses, & &1.context_evaluations)),
      skipped_context_evaluations: Enum.sum(Enum.map(focuses, & &1.context_skipped)),
      invalid_gold_ids: invalid,
      blocked_cases: Enum.count(results, &(&1.grade == :ungradable))
    }, pretty: true, width: 115)
    IO.puts("Inspect real numbered dictionary definitions before labeling gold IDs. No model, database, or training changes were requested.")
  end

  defp rank(rows, id) do
    case Enum.find_index(rows, fn {candidate_id, _} -> candidate_id == id end) do
      nil -> nil
      index -> index + 1
    end
  end

  defp normalize(value) when is_binary(value),
    do: value |> String.downcase() |> String.replace(~r/[^\p{L}\p{N}]/u, "")
  defp normalize(_), do: ""

  defp sense_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp sense_pos(_), do: nil

  defp compact(value, max) when is_binary(value),
    do: value |> String.trim() |> String.slice(0, max)
  defp compact(_, _), do: nil
end

Symbrella.BenchmarkLIFGSenses.run(System.argv())
