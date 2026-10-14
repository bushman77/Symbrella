# Causal, read-only inspection of the MWE decision chain for one real
# production sentence. No dictionary entries or selection rules are edited.
#
# Run from repository root:
#   mix run scripts/trace_question_mwe_promotion.exs
#
# Optional grammar-weight comparison (two production pipeline runs):
#   MWE_COMPARE_GRAMMAR=1 mix run scripts/trace_question_mwe_promotion.exs
#
# NOTE: Core.resolve_input/2 runs the regular production cognitive pipeline
# and may update its usual STM, mood, episode or memory state. Use a private
# diagnostic session. This script does not invoke LLM/chat or external search.

defmodule Symbrella.TraceQuestionMWEPromotion do
  alias Brain.LIFG.ContextFit
  alias Core.MWE.GrammarPrecedence
  import Ecto.Query, only: [from: 2]

  @question "What do you know about Japan's railgun development?"
  @phrase "what do you know"

  def run do
    IO.puts("\n=== LIFG / sentence-signature / MWE promotion proof ===")
    IO.puts("Question: #{inspect(@question)}")
    IO.puts("No sense selection or grammar rules are changed.\n")

    trace_once(:configured, nil)

    if System.get_env("MWE_COMPARE_GRAMMAR") == "1" do
      trace_once(:grammar_1, 1.0)
    end

    IO.puts("\nINTERPRETATION GUIDE:")
    IO.puts("* raw_winner = what Brain.LIFG returned for the virtual phrase.")
    IO.puts("* final_winner = Core.LIFG.Attach's post-score compatibility choice.")
    IO.puts("* lexical_gate_passed? = exact dictionary/span/source checks passed.")
    IO.puts("* promoted? = phrase actually entered canonical semantic_choices.")
    IO.puts("* A complete sentence frame can reject a lexical phrase before LIFG.")
    IO.puts("* If rejected, no virtual MWE score entry is expected for that phrase.")
    IO.puts("* ATL unresolved does NOT by itself revoke a promoted MWE.")
    IO.puts("* This trace describes the decisions; it does not validate their meaning.")
  end

  defp trace_once(label, grammar_override) do
    opts =
      [mode: :prod,
       enrich_lexicon?: true,
       lexicon_stage?: true,
       session_id: "mwe-promotion-proof-#{label}",
       lifg_opts: [trace_mwe_promotion?: true]]
      |> maybe_grammar_override(grammar_override)

    IO.puts("\n--- Run #{label} ---")
    si = Core.resolve_input(@question, opts)

    grammar_weight =
      si
      |> Map.get(:lifg_opts, [])
      |> Keyword.get(:grammar_weight, 0.0)

    IO.inspect(%{
      intent: si.intent,
      conversation_act: si.conversation_act,
      grammar_weight: grammar_weight,
      selected_action: si.selected_action
    }, label: "Question classification")

    print_pos_evidence(si)

    covers =
      si
      |> list(:token_cover)
      |> Enum.filter(fn x -> Map.get(x, :kind) == :phrase end)
      |> Enum.map(&Map.take(&1,
        [:phrase, :token_span, :dictionary_backed?, :lexical_pos, :sense_ids, :score]))

    IO.inspect(covers, label: "Token-cover phrase candidates", limit: :infinity,
      pretty: true, width: 110)

    IO.inspect(Map.get(si, :sentence_competition),
      label: "Sentence-level competing interpretations",
      limit: :infinity, pretty: true, width: 110)

    sense_units =
      si
      |> list(:mwe_lexicon)
      |> Enum.filter(fn unit -> Map.get(unit, :phrase) == @phrase end)

    lexical = Enum.map(sense_units, fn unit ->
      %{
        span: unit.span,
        senses: Enum.map(List.wrap(unit.senses), fn sense ->
          Map.take(sense, [:id, :pos, :definition, :example, :activation])
        end)
      }
    end)

    IO.inspect(lexical, label: "Exact phrase dictionary senses",
      limit: :infinity, pretty: true, width: 110)

    for unit <- sense_units, sense <- List.wrap(Map.get(unit, :senses)) do
      report = ContextFit.explain(@question, @phrase, sense)
      IO.inspect(%{
        id: Map.get(sense, :id),
        context_fit: Map.get(report, :score),
        context_terms: Map.get(report, :context_terms),
        matches: Map.get(report, :matches),
        contributions: Map.get(report, :contributions)
      }, label: "Dictionary context fit", pretty: true, width: 110, limit: 14)
    end

    signatures =
      si
      |> list(:sentence_signatures)
      |> Enum.map(&Map.take(&1, [:id, :span, :roles, :evidence, :required_features]))
    IO.inspect(signatures, label: "Available sentence signatures",
      limit: :infinity, pretty: true, width: 110)

    audits =
      si
      |> list(:trace)
      |> Enum.filter(fn
        %{stage: stage} when stage in [:mwe_lifg_score_audit, :mwe_promotion_audit] -> true
        _ -> false
      end)
      |> Enum.reverse()
      |> Enum.map(&Map.take(&1, [:stage, :reason, :meta]))

    IO.inspect(audits, label: "Actual scoring and promotion trace",
      limit: :infinity, pretty: true, width: 110)

    chosen =
      si
      |> list(:mwe_choices)
      |> Enum.filter(&(Map.get(&1, :phrase) |> normalize() == @phrase))
      |> Enum.map(&Map.take(&1, [:id, :score, :token_span, :source, :context_score]))

    IO.inspect(chosen, label: "Final LIFG phrase candidates", limit: :infinity)

    canonical =
      si
      |> list(:semantic_choices)
      |> Enum.filter(fn c ->
        Map.get(c, :kind) == :mwe or
          Map.get(c, :token_index) in 0..3
      end)
      |> Enum.map(&Map.take(&1, [:id, :kind, :token_index, :token_span, :score]))

    IO.inspect(canonical, label: "Final canonical semantic choices",
      limit: :infinity, pretty: true)

    IO.puts("\nSentence-role preservation checks:")
    for phrase <- ["what do you know", "do you", "know about"] do
      case Enum.find(list(si, :mwe_lexicon), &(&1.phrase == phrase)) do
        nil ->
          IO.puts("NOT APPLICABLE #{phrase} — no dictionary phrase in this run")

        %{span: span} ->
          rejected? = Core.Syntax.SentenceCompetition.reject_phrase?(si, span)

          selected? =
            Enum.any?(covers, &(Map.get(&1, :token_span) == span)) or
              Enum.any?(canonical, fn c ->
                Map.get(c, :token_span) == span and Map.get(c, :kind) == :mwe
              end)

          IO.puts("#{if(rejected? and not selected?, do: "PASS", else: "FAIL")} " <>
                    "#{phrase} at #{inspect(span)} — rejected?=#{rejected?} selected?=#{selected?}")
      end
    end

    atl =
      si
      |> Map.get(:atl_slate, %{})
      |> case do
        %{} = slate -> Map.get(slate, :constituents, [])
        _ -> []
      end
      |> List.wrap()
      |> Enum.map(&Map.take(&1, [:id, :span, :status, :roles, :unresolved_roles]))

    IO.inspect(atl, label: "ATL role-resolution results",
      limit: :infinity, pretty: true)

    IO.inspect(list(si, :lexical_constructions),
      label: "Post-ATL verified lexical constructions", limit: :infinity)

    IO.inspect(
      Map.take(Map.get(si, :comprehension) || %{},
        [:understood, :uncertain, :unresolved, :syntactic_relations,
         :degraded?, :reasons]),
      label: "Comprehension result", pretty: true
    )

    gate =
      Enum.map(chosen, fn %{token_span: span} = entry ->
        %{
          id: entry.id,
          question_conflict?:
            GrammarPrecedence.incompatible_question_mwe?(si, @phrase, ["phrase"]),
          verb_frame_conflict?:
            GrammarPrecedence.incompatible_verb_mwe?(si, span, ["phrase"]),
          canonical?:
            Enum.any?(list(si, :semantic_choices), &(&1.id == entry.id)),
          atl_unresolved?: Enum.any?(atl, &(&1.status == :unresolved)),
          lexical_recheck_after_atl?:
            Enum.any?(list(si, :trace), fn
              %{stage: :mwe_promotion_audit, meta: %{pass: n}} when n > 2 -> true
              _ -> false
            end)
        }
      end)

    IO.inspect(gate, label: "Final causal checks", pretty: true, limit: :infinity)
  end


  # Explain a missing sentence frame with the exact POS evidence seen by the
  # detector, compared with the POS rows currently stored in the local DB.
  # Read-only and bounded: one grouped query for the eight base-token norms.
  defp print_pos_evidence(si) do
    words =
      si
      |> list(:tokens)
      |> Enum.filter(fn token -> Map.get(token, :n, 1) == 1 end)
      |> Enum.with_index()
      |> Enum.map(fn {token, fallback_index} ->
        surface = Map.get(token, :phrase) || Map.get(token, "phrase") || ""
        index = Map.get(token, :index) || fallback_index
        %{index: index, surface: surface, norm: lexical_norm(surface)}
      end)

    norms = words |> Enum.map(& &1.norm) |> Enum.reject(&(&1 == "")) |> Enum.uniq()

    active_pos =
      si
      |> list(:active_cells)
      |> Enum.reduce(%{}, fn cell, acc ->
        norm = lexical_norm(Map.get(cell, :norm) || Map.get(cell, :word))
        pos = Map.get(cell, :pos)

        if norm in norms and not is_nil(pos) do
          Map.update(acc, norm, MapSet.new([to_string(pos)]),
            &MapSet.put(&1, to_string(pos)))
        else
          acc
        end
      end)

    db_pos =
      try do
        Db.all(
          from(c in Db.BrainCell,
            where: c.norm in ^norms,
            group_by: [c.norm, c.pos],
            select: %{norm: c.norm, pos: c.pos}
          )
        )
        |> Enum.reduce(%{}, fn %{norm: norm, pos: pos}, acc ->
          if not is_nil(pos) do
            Map.update(acc, norm, MapSet.new([to_string(pos)]),
              &MapSet.put(&1, to_string(pos)))
          else
            acc
          end
        end)
      rescue
        error ->
          IO.puts("Dictionary POS diagnostic unavailable: #{Exception.message(error)}")
          %{}
      end

    inspected =
      Enum.map(words, fn word ->
        Map.merge(word, %{
          active_pos: sorted_pos(active_pos, word.norm),
          db_pos: sorted_pos(db_pos, word.norm)
        })
      end)

    IO.inspect(inspected, label: "Token POS diagnostic (active cells vs dictionary)",
      pretty: true, limit: :infinity, width: 120)

    predicate = Enum.find(inspected, &(&1.index == 3))
    prep = Enum.find(inspected, &(&1.index == 4))

    if predicate && prep do
      IO.inspect(%{
        predicate_surface: predicate.surface,
        active_verb?: "verb" in predicate.active_pos,
        db_verb?: "verb" in predicate.db_pos,
        preposition_surface: prep.surface,
        active_preposition?: "preposition" in prep.active_pos,
        db_preposition?: "preposition" in prep.db_pos
      }, label: "WH-topic frame prerequisites")
    end
  end

  defp sorted_pos(by_norm, norm) do
    by_norm |> Map.get(norm, MapSet.new()) |> MapSet.to_list() |> Enum.sort()
  end

  defp lexical_norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp lexical_norm(_), do: ""

  defp maybe_grammar_override(opts, nil), do: opts

  defp maybe_grammar_override(opts, value) do
    Keyword.update!(opts, :lifg_opts, &Keyword.put(&1, :grammar_weight, value))
  end

  defp list(si, key) do
    si
    |> Map.get(key, [])
    |> List.wrap()
  end

  defp normalize(value) when is_binary(value), do: String.downcase(String.trim(value))
  defp normalize(_), do: ""
end

Symbrella.TraceQuestionMWEPromotion.run()
