defmodule Core.LIFG.Attach do
  @moduledoc """
  Core-side LIFG runner + attachment helpers.

  Extracted from Core.ex to keep the main pipeline slim.
  Responsibilities:
    • Call Brain.LIFG (safely) and attach :lifg_choices / :atl_slate / trace
    • Apply tiny tie-break priors and MWE-friendly bumps
    • Retain original unigram choices; Core.MWE.Decisions exclusively owns
      semantic shadowing after validating lexical phrase promotion
  """

  # ── knobs ─────────────────────────────────────────────────────────────
  @lifg_tie_epsilon 0.01
  @lifg_prob_epsilon 0.01

  # Bias knobs for multi-word expressions (config-driven):
  #   :mwe_greet_phrase_bump — extra score for greeting phrases
  #   :mwe_general_bump      — extra score for non-greeting MWEs
  @greet_phrase_bump Application.compile_env(:core, :mwe_greet_phrase_bump, 0.01)
  @mwe_general_bump Application.compile_env(:core, :mwe_general_bump, 0.008)

  alias Brain

  @spec run_and_attach(map(), keyword()) :: map()
  def run_and_attach(%{tokens: tokens} = si, lifg_opts) when is_list(tokens) do
    # Pass intent bias downstream (non-breaking; ignored if unused)
    lifg_opts2 = Keyword.put(lifg_opts, :intent_bias, Map.get(si, :intent_bias, %{}))

    {lifg_input, virtual_mwes} = Core.MWE.LIFGBridge.prepare(si, lifg_opts)

    case safe_lifg_run(lifg_input, lifg_opts2) do
      {:ok, %{si: si_after, choices: raw_choices, slate: slate, flips: flips}}
      when is_list(raw_choices) and is_map(slate) ->
        tokens_for_choices = Map.get(si_after, :tokens, tokens)
        intent_fallback = Map.get(si, :intent)

        all_choices =
          raw_choices
          |> Enum.map(fn ch ->
            token_index = Map.get(ch, :token_index, 0)
            t = Enum.at(tokens_for_choices, token_index, %{})
            token_norm = t |> Map.get(:phrase, "") |> norm()

            chosen_id = Map.get(ch, :chosen_id)
            scores = Map.get(ch, :scores) || %{}
            feats = Map.get(ch, :features) || %{}
            alt_ids = Map.get(ch, :alt_ids, [])
            margin = Map.get(ch, :margin, 0.0)
            prob_margin = Map.get(ch, :prob_margin, 0.0)

            phrase_bump_for = fn id, thin? ->
              cond do
                Map.get(t, :n, 1) > 1 and id_norm(id) == token_norm ->
                  base = if thin?, do: @mwe_general_bump, else: 0.0

                  greet =
                    if Map.get(si_after, :intent, intent_fallback) == :greet,
                      do: @greet_phrase_bump,
                      else: 0.0

                  base + greet

                true ->
                  0.0
              end
            end

            base_score =
              if is_binary(chosen_id) and is_map(scores),
                do: Map.get(scores, chosen_id, 0.0),
                else: Map.get(feats, :score_norm, 0.0)

            candidates =
              [chosen_id | alt_ids]
              |> Enum.uniq()
              |> Enum.reject(&is_nil/1)

            matching = Enum.filter(candidates, &(id_norm(&1) == token_norm))

            # Tiny prior only for razor-thin cases
            pos_prior = %{
              "phrase" => 0.03,
              "noun" => 0.01,
              "verb" => 0.0,
              "adjective" => 0.0
            }

            thin? = margin < @lifg_tie_epsilon or prob_margin < @lifg_prob_epsilon
            score_of = fn id -> Map.get(scores, id, base_score) + phrase_bump_for.(id, thin?) end

            pick_with_prior = fn ids ->
              Enum.max_by(
                ids,
                fn id ->
                  candidate_score = score_of.(id)
                  prior = Map.get(pos_prior, id_pos(id), 0.0)

                  candidate_score * (1.0 + prior)
                end,
                fn -> chosen_id end
              )
            end

            {chosen_id2, score2} =
              cond do
                matching != [] and thin? ->
                  best = pick_with_prior.(matching)
                  {best, Map.get(scores, best, base_score)}

                matching != [] ->
                  best = Enum.max_by(matching, &score_of.(&1), fn -> chosen_id end)
                  {best, Map.get(scores, best, base_score)}

                thin? ->
                  best = pick_with_prior.(candidates)
                  {best, Map.get(scores, best, base_score)}

                true ->
                  {chosen_id, base_score}
              end

            # Preserve recognized infinitive roles through the final
            # compatibility tie-break, which otherwise favors nouns.
            {chosen_id2, score2} =
              prefer_signature_role(
                si_after, token_index, candidates, scores, chosen_id2, score2, lifg_opts
              )

            chosen_candidate = candidate_for_choice(si_after, token_index, chosen_id2)

            %{
              token_index: token_index,
              lemma: choice_lemma(chosen_candidate, token_norm),
              id: chosen_id2,
              pos: candidate_pos(chosen_candidate) || id_pos(chosen_id2),
              source: candidate_source(chosen_candidate),
              definition:
                candidate_field(chosen_candidate, [:definition, :def, :gloss, :meaning]),
              example: candidate_field(chosen_candidate, [:example, :ex, :usage, :sample]),
              alt_ids: alt_ids,
              semantic_control: Map.get(ch, :semantic_control),
              sense_focus: Map.get(ch, :sense_focus),
              definition_context_evidence: Map.get(ch, :definition_context_evidence, []),
              # Full Stage1 post-softmax distribution. A second-pass LIFG
              # update requires this exact prior, not the winning score alone.
              candidate_probs:
                if(is_map(scores) and
                    (map_size(scores) <= 64 or
                       (Keyword.get(lifg_opts, :synapse_eval_full_scores?, false) == true and
                          map_size(scores) <= 256)),
                  do: scores, else: nil),
              score: score2
            }
          end)

        {word_choices, mwe_choices} =
          Core.MWE.LIFGBridge.split_choices(all_choices, virtual_mwes)

        # Never drop lexical unigram choices merely because a virtual MWE
        # won Stage1. The phrase must pass Core.MWE.Decisions' lexical,
        # structural and span checks before shadowing in semantic_choices.
        lifg_choices = word_choices

        si_clean =
          if map_size(virtual_mwes) > 0 do
            si_after
            |> Map.put(:tokens, tokens)
            |> Map.update(:sense_candidates, %{}, fn buckets ->
              Map.drop(buckets, Map.keys(virtual_mwes))
            end)
          else
            si_after
          end

        si_clean
        # Restore the read-only attention projection from the pre-LIFG SI.
        # It must not be mistaken for a replacement candidate slate.
        |> Map.put(:lexical_focus, Map.get(si, :lexical_focus))
        |> Map.put(:mwe_lexicon, Map.get(si, :mwe_lexicon, []))
        |> Map.put(:mwe_choices, mwe_choices)
        |> Map.put(:atl_slate, slate)
        |> Map.put(:lifg_choices, lifg_choices)
        |> Map.put(:acc_conflict, Map.get(si_after, :acc_conflict, 0.0))
        |> Core.Pipeline.Trace.append(
          :lifg_run,
          decision: :attached,
          reason: :stage1_choices_available,
          scores: %{choice_count: length(raw_choices), flips: flips},
          meta: %{choice_count: length(raw_choices), flips: flips}
        )
        |> Core.MWE.Decisions.attach()
        |> maybe_trace_word_flow(word_choices, lifg_choices, lifg_opts)
        |> maybe_trace_mwe_scores(raw_choices, all_choices, virtual_mwes, lifg_opts)
        |> maybe_trace_word_scores(raw_choices, all_choices, tokens_for_choices,
          virtual_mwes, lifg_opts)

      _ ->
        si
    end
  end

  def run_and_attach(si, _lifg_opts), do: si

  # Compare the complete lexical word choices with the validated semantic
  # MWE view. Only Core.MWE.Decisions may suppress words from semantic_choices.
  # The legacy :after_shadow key remains for trace-reader compatibility,
  # but refers to retained lexical words, not premature shadowing.
  defp maybe_trace_word_flow(si, raw_words, shadowed_words, opts) do
    if Keyword.get(opts, :trace_word_scores?, false) == true do
      compact = fn choices ->
        choices
        |> List.wrap()
        |> Enum.filter(&is_map/1)
        |> Enum.take(32)
        |> Enum.map(&Map.take(&1, [:token_index, :id, :pos, :kind, :token_span]))
      end

      Core.Pipeline.Trace.append(si, :lifg_word_flow_audit,
        decision: :observed,
        reason: :unigrams_retained_until_canonical_mwe_promotion,
        meta: %{
          before_shadow: compact.(raw_words),
          after_shadow: compact.(shadowed_words),
          retained_words: compact.(shadowed_words),
          after_promotion: compact.(Map.get(si, :semantic_choices, [])),
          mwe_candidates:
            Map.get(si, :mwe_choices, [])
            |> List.wrap()
            |> Enum.take(12)
            |> Enum.map(&Map.take(&1,
              [:token_index, :id, :token_span, :span, :source]))
        }
      )
    else
      si
    end
  end

  # Diagnostic only. A small winning probability can reflect a large
  # candidate bucket; do not interpret it as sentence-level confidence.
  defp maybe_trace_word_scores(si, raw, final, tokens, virtual, opts) do
    if Keyword.get(opts, :trace_word_scores?, false) == true do
      # Trace the effective mode for this call: a diagnostic can request
      # :all even when runtime defaults to :top2 or :none.
      scores_mode =
        Keyword.get(opts, :scores,
          Application.get_env(:brain, :lifg_stage1_scores_mode, :all))
      entries = Core.LIFG.ScoreAudit.summarize(raw, final, tokens, virtual, scores_mode)

      Core.Pipeline.Trace.append(si, :lifg_word_score_audit,
        decision: :observed,
        reason: :per_token_competitive_probabilities,
        meta: %{scores_mode: scores_mode, entries: entries}
      )
    else
      si
    end
  end

  # Opt-in, observation-only trace of the virtual MWE candidates used by
  # LIFG. Unlike the normal compact choices, this records raw scorer IDs,
  # candidate scores, and the result of Core's post-LIFG tie-break.
  # Does not change any lexical or semantic winner.
  defp maybe_trace_mwe_scores(si, raw_choices, final_choices, virtual_mwes, opts) do
    if Keyword.get(opts, :trace_mwe_promotion?, false) == true and
         map_size(virtual_mwes) > 0 do
      entries =
        virtual_mwes
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.take(12)
        |> Enum.map(fn {index, cover} ->
          raw = Enum.find(raw_choices, &(Map.get(&1, :token_index) == index)) || %{}
          selected = Enum.find(final_choices, &(Map.get(&1, :token_index) == index)) || %{}
          # :alt_ids lists only retry-eligible alternatives, not all
          # candidates. Keep the complete bounded score map to reveal
          # runner-up senses even when :alt_ids is empty.
          scored =
            case Map.get(raw, :scores) do
              %{} = all ->
                all
                |> Enum.filter(fn {id, score} ->
                  is_binary(id) and is_number(score)
                end)
                |> Enum.sort_by(fn {_, score} -> -score end)
                |> Enum.take(16)
                |> Map.new()

              _ -> %{}
            end

          %{
            phrase: Map.get(cover, :phrase),
            span: Map.get(cover, :token_span),
            virtual_index: index,
            raw_winner: Map.get(raw, :chosen_id),
            final_winner: Map.get(selected, :id),
            raw_candidate_scores: scored,
            raw_candidate_score_kind: :post_softmax_probability,
            raw_alt_ids: Map.get(raw, :alt_ids, []),
            raw_margin: Map.get(raw, :margin),
            raw_probability_margin: Map.get(raw, :prob_margin),
            final_score: Map.get(selected, :score)
          }
        end)

      Core.Pipeline.Trace.append(si, :mwe_lifg_score_audit,
        decision: :observed,
        reason: :virtual_phrase_candidate_scores,
        meta: %{
          grammar_weight: Keyword.get(opts, :grammar_weight, 0.0),
          scores_mode: Application.get_env(:brain, :lifg_stage1_scores_mode, :all),
          entries: entries
        }
      )
    else
      si
    end
  end

  # ───────────────────────── Brain call (safe) ─────────────────────────

  defp safe_lifg_run(si, opts) do
    cond do
      Code.ensure_loaded?(Brain) and function_exported?(Brain, :lifg_run, 2) ->
        try do
          apply(Brain, :lifg_run, [si, opts])
        rescue
          _ -> apply(Brain.LIFG, :run, [si, opts])
        end

      Code.ensure_loaded?(Brain.LIFG) and function_exported?(Brain.LIFG, :run, 2) ->
        apply(Brain.LIFG, :run, [si, opts])

      true ->
        {:error, :lifg_unavailable}
    end
  end

  # Respect complete signature POS requirements through the final tie-break:
  # infinitive complements and bounded verb/object/particle predicates.
  # Never fabricate or relabel senses. Opt-in through grammar_weight.
  defp prefer_signature_role(si, index, candidates, scores, winner, score, opts) do
    weight = Keyword.get(opts, :grammar_weight, 0.0)

    required_pos =
      if is_number(weight) and weight > 0 do
        Core.LIFG.SignatureRoles.required_pos(si, index)
      end

    matching =
      if required_pos,
        do: Enum.filter(candidates, &(id_pos(&1) == required_pos)),
        else: []

    if matching != [] and id_pos(winner) != required_pos do
      best = Enum.max_by(matching, &Map.get(scores, &1, 0.0))
      {best, Map.get(scores, best, score)}
    else
      {winner, score}
    end
  end

  # ───────────────────────── MWE shadowing ─────────────────────────

  # ───────────────────────── id helpers ─────────────────────────

  defp id_norm(nil), do: nil

  defp id_norm(id) when is_binary(id) do
    id
    |> String.split("|")
    |> List.first()
  end

  defp id_pos(nil), do: nil

  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, p | _] -> p
      _ -> nil
    end
  end

  defp candidate_for_choice(si, token_index, chosen_id)
       when is_map(si) and is_integer(token_index) do
    sc = Map.get(si, :sense_candidates) || Map.get(si, "sense_candidates") || %{}

    bucket =
      Map.get(sc, token_index) ||
        Map.get(sc, Integer.to_string(token_index)) ||
        Map.get(sc, to_string(token_index)) ||
        []

    Enum.find(List.wrap(bucket), fn
      %{} = cand -> candidate_field(cand, [:id, :chosen_id]) == chosen_id
      _ -> false
    end)
  end

  defp candidate_for_choice(_si, _token_index, _chosen_id), do: nil

  defp choice_lemma(%{} = cand, fallback) do
    candidate_field(cand, [:lemma, :canonical, :norm, :word]) || fallback
  end

  defp choice_lemma(_cand, fallback), do: fallback
  defp candidate_pos(%{} = cand), do: candidate_field(cand, [:pos, :chosen_pos])
  defp candidate_pos(_), do: nil

  defp candidate_source(%{} = cand), do: candidate_field(cand, [:source, :src])
  defp candidate_source(_), do: nil

  defp candidate_field(%{} = cand, keys) when is_list(keys) do
    Enum.find_value(keys, fn key ->
      Map.get(cand, key) || Map.get(cand, Atom.to_string(key))
    end)
  end

  defp candidate_field(_, _), do: nil

  # ───────────────────────── norm ─────────────────────────

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(v) do
    v
    |> Kernel.to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end
end
