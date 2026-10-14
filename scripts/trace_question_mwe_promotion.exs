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

    covers =
      si
      |> list(:token_cover)
      |> Enum.filter(fn x -> Map.get(x, :kind) == :phrase end)
      |> Enum.map(&Map.take(&1,
        [:phrase, :token_span, :dictionary_backed?, :lexical_pos, :sense_ids, :score]))

    IO.inspect(covers, label: "Token-cover phrase candidates", limit: :infinity,
      pretty: true, width: 110)

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
        [:understood, :uncertain, :unresolved, :degraded?, :reasons]),
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
