# apps/core/lib/core/recall/execute.ex
defmodule Core.Recall.Execute do
  @moduledoc """
  Execute a recall plan against long-term memory (LTM/DB).

  Hooks (safe, opt-in/out via options):

  • Attach — Brain.Hippocampus.attach_episodes/2
      - Enabled by default with hippo: true
      - Default opts: hippo_opts: [limit: 3, min_jaccard: 0.1]
      - After attaching we apply a local Jaccard gate to honor hippo_opts[:min_jaccard]
        by comparing query norms (from the current SI) against each episode’s norms.
        Episode norms are taken from `episode.norms` when present, or derived from
        `episode.slate` (`winners` and/or `tokens`) as a fallback.

  • Write — Brain.Hippocampus.encode/2
      - Enabled by default with hippo_write: true
      - Stores slate: %{tokens: si.tokens}, meta: %{scope: %{source: si.source || :core}}
      - We write **after** attach so the just-written episode does not echo into
        the same recall cycle and bypass `min_jaccard`.

  Strategies supported:
    :exact     – DB word existence (already present).
    :synonym   – NEW: harvest synonyms from Lexicon rows and recall those words.
    :embedding – placeholder (no-op here).
  """

  alias Core.SemanticInput, as: SI
  alias Core.Recall.Plan
  alias Brain.Hippocampus
  alias Db.Lexicon, as: DbLex

  @type exists_fun :: (binary() -> boolean())
  @type neg_exists_fun :: (binary() -> boolean())
  @type neg_put_fun :: (binary() -> :ok)

  @max_tok_id 9_223_372_036_854_775_807
  @default_limit 8
  @default_budget_ms 40

  @hippo_default_opts [limit: 3, min_jaccard: 0.1]

  @synonym_score 0.65
  @synonym_reason :synonym

  @spec execute(SI.t(), Plan.t(), keyword()) :: SI.t()
  def execute(%SI{} = si, %Plan{} = plan, opts \\ []) do
    t0 = now_ms()
    exists_fun = Keyword.get(opts, :exists_fun, default_exists_fun())
    neg_exists = Keyword.get(opts, :neg_exists_fun, default_neg_exists_fun())
    neg_put = Keyword.get(opts, :neg_put_fun, default_neg_put_fun())

    {candidates, covered_tok_ids} = candidate_keys(si)

    candidates2 =
      Enum.reject(candidates, fn {_key, tok_ids, _prio} ->
        Enum.any?(tok_ids, &(&1 in covered_tok_ids))
      end)

    {added_refs, counts, budget_hit?} =
      run_strategies(
        candidates2,
        plan.strategies,
        exists_fun,
        neg_exists,
        neg_put,
        plan.max_items,
        plan.budget_ms,
        t0,
        []
      )

    si2 = merge_and_trace(si, added_refs, counts, plan.budget_ms, budget_hit?, t0)

    # ---- Hippocampus attach (+ local Jaccard gate; default on) ----
    si3 = maybe_attach_episodes(si2, opts)

    # ---- Hippocampus write (safe; default on) — do this AFTER attach to avoid echo ----
    _ = maybe_write_episode(si3, opts)

    si3
  end

  # ---------- candidate key extraction ----------

  # Returns {candidates, covered_tok_ids}
  # candidates: [{key :: binary, tok_ids :: [non_neg_integer], prio :: 0 | 1}]
  defp candidate_keys(%SI{tokens: tokens} = si) do
    indexed = Enum.with_index(tokens)

    cands =
      indexed
      |> Enum.flat_map(fn {tok, idx} ->
        case tok do
          %Core.Token{phrase: phrase, mw: true} when is_binary(phrase) and phrase != "" ->
            [{normalize(phrase), [idx], 0}]

          %Core.Token{phrase: phrase} when is_binary(phrase) and phrase != "" ->
            [{normalize(phrase), [idx], 1}]

          %{} = m ->
            raw = Map.get(m, :norm) || Map.get(m, :text) || Map.get(m, :phrase)
            key = if is_binary(raw), do: normalize(raw), else: ""
            prio = if Map.get(m, :is_mwe_head, false), do: 0, else: 1
            [{key, [Map.get(m, :id, idx)], prio}]

          _ ->
            []
        end
      end)
      |> Enum.reject(fn {k, _, _} -> k in [nil, ""] end)

    merged =
      Enum.reduce(cands, %{}, fn {k, tok_ids, prio}, acc ->
        Map.update(acc, {k, prio}, MapSet.new(tok_ids), &MapSet.union(&1, MapSet.new(tok_ids)))
      end)

    list =
      merged
      |> Enum.map(fn {{k, prio}, set} -> {k, Enum.sort(MapSet.to_list(set)), prio} end)
      |> Enum.sort_by(fn {_k, tok_ids, prio} -> {prio, Enum.min(tok_ids)} end)

    covered = covered_tok_ids_from_active(si)
    {list, covered}
  end

  defp covered_tok_ids_from_active(%SI{active_cells: refs}) do
    refs
    |> Enum.flat_map(fn
      %{source: src, matched_tokens: mts} when src in [:runtime, :recency] ->
        Enum.map(mts, & &1.tok_id)
      _ ->
        []
    end)
    |> Enum.uniq()
  end

  defp covered_tok_ids_from_active(_), do: []

  # ---------- strategy runner ----------

  defp run_strategies(
         candidates,
         strategies,
         exists_fun,
         neg_exists,
         neg_put,
         max_items,
         budget_ms,
         t0,
         acc_refs
       ) do
    counts = %{exact: 0, synonym: 0, embedding: 0}

    limit =
      case max_items do
        :infinity -> :infinity
        n when is_integer(n) and n > 0 -> n
        _ -> @default_limit
      end

    Enum.reduce_while(strategies, {acc_refs, counts, false}, fn strat, {refs, cnts, _bhit} ->
      time_left =
        case budget_ms do
          :infinity -> :infinity
          n when is_integer(n) and n > 0 -> n - (now_ms() - t0)
          _ -> @default_budget_ms - (now_ms() - t0)
        end

      cond do
        time_left != :infinity and time_left <= 0 ->
          {:halt, {refs, cnts, true}}

        limit != :infinity and length(refs) >= limit ->
          {:halt, {refs, cnts, false}}

        strat == :exact ->
          slots_left =
            case limit do
              :infinity -> :infinity
              n -> n - length(refs)
            end

          {added, new_cnts, bh} =
            exact_pass(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left, t0)

          {:cont, {refs ++ added, bump(cnts, :exact, new_cnts.exact), bh}}

        strat == :synonym ->
          slots_left =
            case limit do
              :infinity -> :infinity
              n -> n - length(refs)
            end

          {added, add_cnt, bh} =
            synonym_pass(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left, t0, refs)

          {:cont, {refs ++ added, %{cnts | synonym: cnts.synonym + add_cnt}, bh}}

        strat == :embedding ->
          {:cont, {refs, cnts, false}}

        true ->
          {:cont, {refs, cnts, false}}
      end
    end)
  end

  defp bump(cnts, key, add), do: Map.update!(cnts, key, &(&1 + add))

  # ---------- exact pass ----------

  defp exact_pass(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0) do
    do_exact(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, [], 0)
  end

  defp do_exact([], _exists_fun, _neg_exists, _neg_put, _slots_left, _tl, _t0, acc, cnt),
    do: {Enum.reverse(acc), %{exact: cnt}, false}

  defp do_exact(
         [{_key, _tok_ids, _prio} | _] = _rest,
         _exists_fun,
         _neg_exists,
         _neg_put,
         slots_left,
         _tl,
         _t0,
         acc,
         cnt
       )
       when is_integer(slots_left) and slots_left <= 0,
       do: {Enum.reverse(acc), %{exact: cnt}, false}

  defp do_exact(
         [{key, tok_ids, _prio} | rest],
         exists_fun,
         neg_exists,
         neg_put,
         slots_left,
         time_left_ms,
         t0,
         acc,
         cnt
       ) do
    elapsed = now_ms() - t0

    if time_left_ms != :infinity and elapsed >= time_left_ms do
      {Enum.reverse(acc), %{exact: cnt}, true}
    else
      cond do
        exists_fun.(key) ->
          ref = to_active_ref_exact(key, tok_ids)
          next_slots = case slots_left do :infinity -> :infinity; n -> n - 1 end
          do_exact(rest, exists_fun, neg_exists, neg_put, next_slots, time_left_ms, t0, [ref | acc], cnt + 1)

        neg_exists.(key) ->
          do_exact(rest, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, acc, cnt)

        true ->
          _ = safe_neg_put(neg_put, key)
          do_exact(rest, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, acc, cnt)
      end
    end
  end

  defp to_active_ref_exact(key, tok_ids) do
    %{
      id: {:db_word, key},
      matched_tokens: Enum.map(tok_ids, &%{tok_id: &1, conf: nil}),
      activation_snapshot: 0.0,
      source: :db_recall,
      reason: :exact,
      score: 1.0,
      ts_ms: now_ms()
    }
  end

  # ---------- synonym pass (NEW) ----------

  # Produces references for synonyms of candidate keys.
  # - uses Db.Lexicon.fetch_by_norms(_)/1 (or *_grouped) if available
  # - respects time & slot budget
  # - consults neg cache & exists_fun to avoid expensive misses
  # - gives each ref reason=:synonym and score=@synonym_score
  defp synonym_pass(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, already_refs) do
    # stop early on no slots
    if is_integer(slots_left) and slots_left <= 0 do
      {[], 0, false}
    else
      elapsed = now_ms() - t0
      if time_left_ms != :infinity and elapsed >= time_left_ms do
        {[], 0, true}
      else
        keys =
          candidates
          |> Enum.map(fn {k, _ids, _p} -> k end)
          |> Enum.uniq()

        syn_map = synonyms_for_keys(keys)

        # ensure we don't duplicate ids that exact pass added
        existing_ids = MapSet.new(Enum.map(already_refs, & &1.id))

        {refs, taken, bhit} =
          do_synonyms(candidates, syn_map, exists_fun, neg_exists, neg_put,
            slots_left, time_left_ms, t0, existing_ids, [], 0
          )

        {Enum.reverse(refs), taken, bhit}
      end
    end
  end

  defp do_synonyms([], _syn_map, _exists_fun, _neg_exists, _neg_put, _slots_left, _tl, _t0, _seen_ids, acc, cnt),
    do: {acc, cnt, false}

  defp do_synonyms(
         [{_key, _tok_ids, _prio} | _] = _rest,
         _syn_map,
         _exists_fun,
         _neg_exists,
         _neg_put,
         slots_left,
         _time_left_ms,
         _t0,
         _seen_ids,
         acc,
         cnt
       )
       when is_integer(slots_left) and slots_left <= 0,
       do: {acc, cnt, false}

  defp do_synonyms(
         [{key, tok_ids, _prio} | rest],
         syn_map,
         exists_fun,
         neg_exists,
         neg_put,
         slots_left,
         time_left_ms,
         t0,
         seen_ids,
         acc,
         cnt
       ) do
    elapsed = now_ms() - t0
    if time_left_ms != :infinity and elapsed >= time_left_ms do
      {acc, cnt, true}
    else
      syns = Map.get(syn_map, key, MapSet.new()) |> MapSet.to_list()

      {acc2, cnt2, slots2, seen2} =
        Enum.reduce_while(syns, {acc, cnt, slots_left, seen_ids}, fn syn, {acc_i, cnt_i, slots_i, seen_i} ->
          cond do
            is_integer(slots_i) and slots_i <= 0 ->
              {:halt, {acc_i, cnt_i, slots_i, seen_i}}

            neg_exists.(syn) ->
              {:cont, {acc_i, cnt_i, slots_i, seen_i}}

            exists_fun.(syn) ->
              ref = to_active_ref_synonym(key, syn, tok_ids)
              # ensure uniqueness across whole run
              if MapSet.member?(seen_i, ref.id) do
                {:cont, {acc_i, cnt_i, slots_i, seen_i}}
              else
                next_slots = case slots_i do :infinity -> :infinity; n -> n - 1 end
                {:cont, {[ref | acc_i], cnt_i + 1, next_slots, MapSet.put(seen_i, ref.id)}}
              end

            true ->
              _ = safe_neg_put(neg_put, syn)
              {:cont, {acc_i, cnt_i, slots_i, seen_i}}
          end
        end)

      do_synonyms(rest, syn_map, exists_fun, neg_exists, neg_put, slots2, time_left_ms, t0, seen2, acc2, cnt2)
    end
  end

  defp to_active_ref_synonym(src_key, syn, tok_ids) do
    %{
      id: {:db_synonym, src_key, syn},
      matched_tokens: Enum.map(tok_ids, &%{tok_id: &1, conf: nil}),
      activation_snapshot: 0.0,
      source: :db_recall,
      reason: @synonym_reason,
      score: @synonym_score,
      ts_ms: now_ms()
    }
  end

  # ---------- synonym harvesting helpers ----------

  defp synonyms_for_keys(keys) when is_list(keys) do
    norms =
      keys
      |> Enum.map(&normalize/1)
      |> Enum.reject(&(&1 in [nil, ""]))
      |> Enum.uniq()

    grouped =
      cond do
        Code.ensure_loaded?(DbLex) and function_exported?(DbLex, :fetch_by_norms_grouped, 1) ->
          safe_fetch_grouped(norms)

        Code.ensure_loaded?(DbLex) and function_exported?(DbLex, :fetch_by_norms, 1) ->
          rows = safe_fetch_flat(norms)
          group_rows_by_norm(rows)

        true ->
          %{}
      end

    Enum.reduce(norms, %{}, fn k, acc ->
      syns =
        grouped
        |> Map.get(k, [])
        |> synonyms_from_rows()
        |> Enum.map(&normalize/1)
        |> Enum.reject(&(&1 in [nil, "", k])) # drop self
        |> MapSet.new()

      Map.put(acc, k, syns)
    end)
  end

  defp safe_fetch_grouped(norms) do
    try do
      DbLex.fetch_by_norms_grouped(norms) || %{}
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  defp safe_fetch_flat(norms) do
    try do
      DbLex.fetch_by_norms(norms) || []
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  defp group_rows_by_norm(rows) when is_list(rows) do
    Enum.group_by(rows, fn r ->
      cond do
        is_map(r) and is_binary(Map.get(r, :norm)) -> normalize(Map.get(r, :norm))
        is_map(r) and is_binary(Map.get(r, "norm")) -> normalize(Map.get(r, "norm"))
        is_map(r) and is_binary(Map.get(r, :word)) -> normalize(Map.get(r, :word))
        is_map(r) and is_binary(Map.get(r, "word")) -> normalize(Map.get(r, "word"))
        is_map(r) and is_binary(Map.get(r, :id)) -> r |> Map.get(:id) |> String.split("|") |> hd()
        is_map(r) and is_binary(Map.get(r, "id")) -> r |> Map.get("id") |> String.split("|") |> hd()
        true -> "__unknown__"
      end
    end)
  end

  defp synonyms_from_rows(rows) when is_list(rows) do
    rows
    |> Enum.flat_map(fn r ->
      syns = Map.get(r, :synonyms) || Map.get(r, "synonyms") || []
      case syns do
        l when is_list(l) -> l
        _ -> []
      end
    end)
    |> Enum.uniq()
  end

  # ---------- merge & trace ----------

  defp merge_and_trace(%SI{} = si, added_refs, counts, budget_ms, budget_hit?, t0) do
    merged =
      (si.active_cells ++ added_refs)
      |> Enum.uniq_by(& &1.id)
      |> Enum.sort_by(fn r -> {-round(activation_of(r) * 1.0e6), min_tok_id(r)} end)

    ev = %{
      stage: :recall_exec,
      ts_ms: now_ms(),
      meta: %{
        counts: counts,
        latency_ms: max(now_ms() - t0, 0),
        budget_ms: budget_ms,
        budget_hit?: budget_hit?
      }
    }

    %SI{si | active_cells: merged, trace: si.trace ++ [ev]}
  end

  # ---- Hippocampus write (safe) ----
  defp maybe_write_episode(%SI{} = si, opts) do
    enabled? = Keyword.get(opts, :hippo_write, true)

    cond do
      not enabled? ->
        :ok

      not is_list(si.tokens) or si.tokens == [] ->
        :ok

      ensure_hippo_started() and Code.ensure_loaded?(Hippocampus) and function_exported?(Hippocampus, :encode, 2) ->
        slate = %{tokens: si.tokens}
        meta  = %{scope: %{source: si.source || :core}}

        try do
          _ = Hippocampus.encode(slate, meta)
          :ok
        rescue
          _ -> :ok
        catch
          _, _ -> :ok
        end

      true ->
        :ok
    end
  end

  # ---- Hippocampus attach (safe) + local min_jaccard filter ----
  defp maybe_attach_episodes(%SI{} = si, opts) do
    enabled?   = Keyword.get(opts, :hippo, true)
    hippo_opts = Keyword.get(opts, :hippo_opts, @hippo_default_opts)

    cond do
      # When disabled, DO NOT create :evidence at all (tests expect nil)
      not enabled? ->
        si

      ensure_hippo_started() and Code.ensure_loaded?(Hippocampus) and function_exported?(Hippocampus, :attach_episodes, 2) ->
        qset = query_norms(si)
        cues_list =
          case qset do
            %MapSet{} -> MapSet.to_list(qset)
            other -> other
          end

        hippo_opts2 =
          hippo_opts
          |> Keyword.put_new(:cues, cues_list)
          |> Keyword.put_new(:query_norms, qset)

        # Primary: let Hippocampus attach onto SI
        si_attached =
          try do
            Hippocampus.attach_episodes(si, hippo_opts2)
          rescue
            _ -> si
          catch
            _, _ -> si
          end

        # Fallback: if nothing attached, call recall directly (min_jaccard: 0.0),
        # then apply the local Jaccard gate below.
        si_with_eps =
          case episodes_list(si_attached) do
            [] when is_list(cues_list) and cues_list != [] ->
              try do
                rec = Hippocampus.recall(cues_list, Keyword.put(hippo_opts2, :min_jaccard, 0.0))
                put_episodes(si_attached, rec)
              rescue
                _ -> si_attached
              catch
                _, _ -> si_attached
              end

            _ ->
              si_attached
          end

        si_with_eps
        |> apply_jaccard_filter(hippo_opts2)
        |> ensure_episodes_key()

      true ->
        # If Hippo isn’t available, leave SI untouched to avoid creating :evidence
        si
    end
  end

  # ---- Local Jaccard gate (query norms vs episode norms) ----
  # Apply only if we have non-empty query cues; otherwise skip gating.
  # Also bump each kept record’s :score to at least the computed Jaccard.
  defp apply_jaccard_filter(%SI{} = si, hippo_opts) do
    min = hippo_opts |> Keyword.get(:min_jaccard, 0.0) |> to_float()
    episodes = episodes_list(si)

    cond do
      episodes == [] ->
        si

      min <= 0.0 ->
        si

      true ->
        q_norms = Keyword.get(hippo_opts, :query_norms) || query_norms(si)
        qset = to_set(q_norms)

        if MapSet.size(qset) == 0 do
          si
        else
          filtered =
            episodes
            |> Enum.reduce([], fn rec, acc ->
              e_set = episode_norms(rec)

              if MapSet.size(e_set) == 0 do
                acc
              else
                j = jaccard(qset, e_set)
                # keep only if j > min (strict), and bump score to at least j
                if j > min do
                  new_score = max(Map.get(rec, :score, 0.0), j)
                  [Map.put(rec, :score, new_score) | acc]
                else
                  acc
                end
              end
            end)
            |> Enum.reverse()

          put_episodes(si, filtered)
        end
    end
  end

  # Robust episodes getter/setter that do NOT rely on Access on structs
  defp episodes_list(%SI{} = si) do
    ev = Map.get(si, :evidence)
    case ev do
      %{} -> Map.get(ev, :episodes, [])
      _   -> []
    end
  end

  # Ensure evidence[:episodes] exists when we actually attached; do not call when hippo: false.
  defp ensure_episodes_key(%SI{} = si) do
    ev = Map.get(si, :evidence)

    cond do
      is_map(ev) and Map.has_key?(ev, :episodes) ->
        si

      is_map(ev) ->
        :maps.put(:evidence, Map.put(ev, :episodes, []), si)

      true ->
        :maps.put(:evidence, %{episodes: []}, si)
    end
  end

  # Set/replace the episodes list on SI.evidence (works on structs without :evidence field)
  defp put_episodes(%SI{} = si, list) when is_list(list) do
    ev     = Map.get(si, :evidence) || %{}
    new_ev = Map.put(ev, :episodes, list)
    :maps.put(:evidence, new_ev, si)
  end

  # ---- Episode norms derivation (fallback if :episode lacks :norms) -------
  defp episode_norms(%{episode: %{norms: %MapSet{} = ms}}), do: ms

  defp episode_norms(%{episode: %{slate: slate}}) when is_map(slate) do
    winners =
      slate
      |> Map.get(:winners, [])
      |> List.wrap()
      |> Enum.flat_map(fn
        %{} = m ->
          v = Map.get(m, :lemma) || Map.get(m, :norm) || Map.get(m, :text) || Map.get(m, :phrase)
          if is_binary(v) and v != "", do: [normalize(v)], else: []
        _ -> []
      end)

    toks =
      slate
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.flat_map(fn
        %{} = m ->
          v = Map.get(m, :norm) || Map.get(m, :text) || Map.get(m, :phrase)
          if is_binary(v) and v != "", do: [normalize(v)], else: []
        %Core.Token{phrase: p} when is_binary(p) and p != "" ->
          [normalize(p)]
        _ ->
          []
      end)

    (winners ++ toks) |> MapSet.new()
  end

  defp episode_norms(_), do: MapSet.new()

  # ---- Query norms helpers -------------------------------------------------

  defp query_norms(%SI{} = si) do
    if Code.ensure_loaded?(Brain.Hippocampus.Normalize) and
         function_exported?(Brain.Hippocampus.Normalize, :norms, 1) do
      try do
        Brain.Hippocampus.Normalize.norms(si)
      rescue
        _ -> best_effort_norms(si)
      catch
        _, _ -> best_effort_norms(si)
      end
    else
      best_effort_norms(si)
    end
  end

  defp best_effort_norms(si) do
    w = winners_to_norms(si)
    if set_nonempty?(w) do
      w
    else
      t = tokens_to_norms(Map.get(si, :tokens))
      if set_nonempty?(t) do
        t
      else
        sentence_to_norms(Map.get(si, :sentence))
      end
    end
  end

  defp set_nonempty?(%MapSet{} = s), do: MapSet.size(s) > 0
  defp set_nonempty?(_), do: false

  defp winners_to_norms(%SI{} = si) do
    slate = Map.get(si, :slate)
    winners =
      case slate do
        %{} -> Map.get(slate, :winners)
        _ -> nil
      end

    winners
    |> List.wrap()
    |> Enum.flat_map(fn
      %{} = m ->
        val = Map.get(m, :lemma) || Map.get(m, :norm) || Map.get(m, :text) || Map.get(m, :phrase)
        if is_binary(val) and val != "", do: [normalize(val)], else: []
      _ -> []
    end)
    |> MapSet.new()
  end

  # Core.Token does NOT have :norm — use :phrase (and accept maps with :norm/:text/:phrase)
  defp tokens_to_norms(tokens) when is_list(tokens) do
    tokens
    |> Enum.flat_map(fn
      %Core.Token{phrase: p} when is_binary(p) and p != "" ->
        [normalize(p)]

      %{} = m ->
        case (Map.get(m, :norm) || Map.get(m, :text) || Map.get(m, :phrase)) do
          v when is_binary(v) and v != "" -> [normalize(v)]
          _ -> []
        end

      _ -> []
    end)
    |> MapSet.new()
  end

  defp tokens_to_norms(_), do: MapSet.new()

  defp sentence_to_norms(sentence) when is_binary(sentence) and sentence != "" do
    sentence
    |> String.downcase()
    |> String.split(~r/[^\p{L}\p{N}_]+/u, trim: true)
    |> MapSet.new()
  end

  defp sentence_to_norms(_), do: MapSet.new()

  # ---- Jaccard ------------------------------------------------------------

  defp jaccard(a, b) do
    a = to_set(a)
    b = to_set(b)
    union = MapSet.size(MapSet.union(a, b))
    if union == 0, do: 0.0, else: MapSet.size(MapSet.intersection(a, b)) / union
  end

  defp to_set(%MapSet{} = ms), do: ms

  defp to_set(list) when is_list(list) do
    list
    |> Enum.map(fn
      %Core.Token{phrase: p} when is_binary(p) -> p
      %{} = m -> Map.get(m, :lemma) || Map.get(m, :norm) || Map.get(m, :text) || Map.get(m, :phrase) || ""
      v -> v
    end)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 in [nil, ""]))
    |> MapSet.new()
  end

  defp to_set(_), do: MapSet.new()

  # ---- utilities ----------------------------------------------------------

  defp activation_of(%Db.BrainCell{activation: a}) when is_number(a), do: a
  defp activation_of(%{activation_snapshot: a}) when is_number(a),    do: a
  defp activation_of(%{activation: a}) when is_number(a),             do: a
  defp activation_of(_),                                              do: 0.0

  defp min_tok_id(%{matched_tokens: [%{tok_id: t0} | _]}), do: t0
  defp min_tok_id(%{matched_tokens: _mts}),                do: 0
  defp min_tok_id(%{token_id: tid}) when is_integer(tid),  do: tid
  defp min_tok_id(%Db.BrainCell{token_id: tid}) when is_integer(tid), do: tid
  defp min_tok_id(_),                                      do: @max_tok_id

  defp default_exists_fun() do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :word_exists?, 1) do
      fn key ->
        try do
          Db.word_exists?(key)
        rescue
          _ -> false
        catch
          _, _ -> false
        end
      end
    else
      fn _ -> false end
    end
  end

  defp default_neg_exists_fun() do
    if Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :exists?, 1) do
      &Core.NegCache.exists?/1
    else
      fn _ -> false end
    end
  end

  defp default_neg_put_fun() do
    if Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :put, 1) do
      &Core.NegCache.put/1
    else
      fn _ -> :ok end
    end
  end

  defp safe_neg_put(fun, key) do
    try do
      fun.(key)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp normalize(<<>>), do: <<>>
  defp normalize(bin) when is_binary(bin), do: String.downcase(String.trim(bin))

  defp to_float(x) when is_float(x),   do: x
  defp to_float(x) when is_integer(x), do: x / 1.0
  defp to_float(_),                    do: 0.0

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end

  # Ensure the Hippocampus GenServer is running (used in attach/write paths)
  defp ensure_hippo_started do
    case Process.whereis(Brain.Hippocampus) do
      pid when is_pid(pid) ->
        true

      _ ->
        try do
          case Brain.Hippocampus.start_link() do
            {:ok, _pid} -> true
            {:error, {:already_started, _pid}} -> true
            _ -> false
          end
        rescue
          _ -> false
        catch
          _, _ -> false
        end
    end
  end
end

