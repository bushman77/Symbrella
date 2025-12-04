defmodule Brain.LIFG.MWE do
  @moduledoc """
  MWE + backfill helpers for LIFG Stage-1.

  Responsibilities:
    - Emit a safe `|phrase|fallback` candidate for MWEs that have no compatible phrase senses yet
      (but *not* for partial/function-word-edge phrases like "kick the" / "the bucket")
    - Backfill real phrase/unigram candidates from `si.active_cells`
    - Optionally backfill from Db when enabled via `:db_backfill?` or app env
    - Optionally absorb unigram cell senses into overlapping MWE buckets
  """

  import Ecto.Query, only: [from: 2]

  alias Brain.Utils.Safe
  alias Db.BrainCell

  @mwe_fallback_event [:brain, :pmtg, :mwe_fallback_emitted]
  @unigram_backfill_event [:brain, :lifg, :unigram_backfill_emitted]

  # Function word buckets (normalized)
  @preps ~w(of to in on at by for from with about into over after between through during before under without within along across behind beyond up down off near among)
  @dets ~w(the a an this that these those some any each every no neither either)
  @conjs ~w(and or but nor so yet for)
  @auxes ~w(be am is are was were being been do does did have has had having)
  @modals ~w(can could may might must shall should will would)
  @pron ~w(i you he she it we they me him her us them my your his her our their mine yours hers ours theirs myself yourself himself herself itself ourselves yourselves themselves)
  @neg ~w(not never)

  # ── Public entry points ───────────────────────────────────────────────

  @doc """
  If a multiword token has no compatible MWE senses, synthesize a lightweight phrase
  `|phrase|fallback` candidate, then backfill real phrase cells from active_cells/Db (if enabled).

  Notes:
    - Early-return if fallback disabled
    - Suppresses fallbacks for phrases that start/end with function words
      (prevents "kick the", "the bucket", etc.)
  """
  @spec ensure_mwe_candidates(map(), keyword()) :: map()
  def ensure_mwe_candidates(%{tokens: tokens} = si, opts) when is_list(tokens) do
    enable? =
      Keyword.get(
        opts,
        :mwe_fallback,
        Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
      )

    if not enable? do
      si
    else
      sc0 = Map.get(si, :sense_candidates, %{})

      {sc, emitted} =
        Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
          token_n = Safe.get(tok, :n, if(Safe.get(tok, :mw, false), do: 2, else: 1))
          phrase = Safe.get(tok, :phrase) || Safe.get(tok, :lemma)
          mw? = Safe.get(tok, :mw, token_n > 1)

          cond do
            not mw? or is_nil(phrase) ->
              {acc, n}

            # IMPORTANT: "compatible" means a *real* phrase sense, not our fallback
            has_compatible_mwe?(acc, idx) ->
              {acc, n}

            not allow_fallback_phrase?(phrase) ->
              {acc, n}

            true ->
              score_guess =
                idx
                |> unigram_neighbor_idxs()
                |> Enum.map(fn j ->
                  acc
                  |> Map.get(j, [])
                  |> Enum.map(&Map.get(&1, :score, 0.0))
                  |> Enum.max(fn -> 0.0 end)
                end)
                |> case do
                  [] -> 0.25
                  xs -> Enum.sum(xs) / max(length(xs), 1)
                end
                |> min(0.45)

              candidate = %{
                id: "#{phrase}|phrase|fallback",
                lemma: phrase,
                norm: phrase,
                mw: true,
                pos: :phrase,
                rel_prior: 0.30,
                score: Float.round(score_guess, 4),
                source: :mwe_fallback
              }

              updated = Map.update(acc, idx, [candidate], fn lst -> [candidate | lst] end)

              safe_exec_telemetry(@mwe_fallback_event, %{count: 1}, %{
                token_index: idx,
                phrase: phrase,
                score: candidate.score
              })

              {updated, n + 1}
          end
        end)

      si1 = if emitted > 0, do: Map.put(si, :sense_candidates, sc), else: si
      backfill_real_mwe_from_active_cells(si1, opts)
    end
  end

  def ensure_mwe_candidates(si, _opts), do: si

  @doc """
  Opt-in: absorb unigram senses from active_cells into overlapping MWE buckets.
  Enabled with opts: `[absorb_unigrams_into_mwe?: true]`.
  """
  @spec absorb_unigrams_into_mwe(map(), keyword()) :: map()
  def absorb_unigrams_into_mwe(%{tokens: toks} = si, opts) when is_list(toks) do
    if not Keyword.get(opts, :absorb_unigrams_into_mwe?, false) do
      si
    else
      sc0 = Map.get(si, :sense_candidates, %{})
      cells = Safe.get(si, :active_cells, []) |> Enum.map(&Safe.to_plain/1)

      cells_by_norm =
        Enum.group_by(cells, fn c ->
          (Safe.get(c, :norm) || Safe.get(c, :word) || "") |> down()
        end)

      updated =
        Enum.reduce(Enum.with_index(toks), sc0, fn {tok, idx}, acc ->
          n = Safe.get(tok, :n, 1)
          mw? = Safe.get(tok, :mw, n > 1)
          mwe_span = Safe.get(tok, :span)

          if mw? and is_tuple(mwe_span) do
            child_norms =
              toks
              |> Enum.with_index()
              |> Enum.filter(fn {t, j} ->
                j != idx and Safe.get(t, :n, 1) == 1 and inside?(Safe.get(t, :span), mwe_span)
              end)
              |> Enum.map(fn {t, _} ->
                (Safe.get(t, :phrase) || Safe.get(t, :word) || "") |> down()
              end)
              |> Enum.reject(&(&1 == ""))

            absorbed =
              child_norms
              |> Enum.flat_map(&Map.get(cells_by_norm, &1, []))
              |> Enum.map(fn c ->
                norm = (Safe.get(c, :norm) || Safe.get(c, :word) || "") |> to_string()

                %{
                  id: to_string(Safe.get(c, :id)),
                  lemma: norm,
                  norm: norm,
                  mw: false,
                  pos: Safe.get(c, :pos),
                  rel_prior: 0.20,
                  score: 0.20,
                  source: :absorbed_unigram
                }
              end)

            if absorbed == [] do
              acc
            else
              merged =
                (absorbed ++ Map.get(acc, idx, []))
                |> Enum.uniq_by(&(&1[:id] || &1["id"]))

              Map.put(acc, idx, merged)
            end
          else
            acc
          end
        end)

      Map.put(si, :sense_candidates, updated)
    end
  end

  def absorb_unigrams_into_mwe(si, _opts), do: si

  @doc """
  If a multiword token only has a `|phrase|fallback`, inject real phrase cells
  from `si.active_cells`, and optionally from Db when enabled.

  Opt:
    * `:db_backfill?` (default from app env `:brain, :lifg_db_backfill`, true)
  """
  @spec backfill_real_mwe_from_active_cells(map(), keyword()) :: map()
  def backfill_real_mwe_from_active_cells(si, opts \\ [])

  def backfill_real_mwe_from_active_cells(si, opts) when is_map(si) and is_list(opts) do
    sc0 = Map.get(si, :sense_candidates, %{})
    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :active_cells, [])

    phrase_cells_by_norm =
      cells
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.filter(fn c ->
        id = to_string(Safe.get(c, :id) || "")
        String.contains?(id, "|phrase|") and not String.ends_with?(id, "|phrase|fallback")
      end)
      |> Enum.group_by(fn c ->
        norm_key(Safe.get(c, :norm) || Safe.get(c, :word))
      end)

    needed_norms =
      tokens
      |> Enum.with_index()
      |> Enum.reduce(MapSet.new(), fn {tok, idx}, acc ->
        phrase = Safe.get(tok, :phrase)
        mw? = Safe.get(tok, :mw, false) or Safe.get(tok, :n, 1) > 1
        bucket = Map.get(sc0, idx, [])

        cond do
          not mw? or is_nil(phrase) ->
            acc

          has_real_phrase_bucket?(bucket) ->
            acc

          not has_fallback_bucket?(bucket) ->
            acc

          true ->
            MapSet.put(acc, norm_key(phrase))
        end
      end)
      |> MapSet.to_list()

    db_backfill? =
      Keyword.get(opts, :db_backfill?, Application.get_env(:brain, :lifg_db_backfill, true))

    db_phrase_cells_by_norm =
      if db_backfill? and needed_norms != [] do
        fetch_phrase_cells_by_norm(needed_norms)
      else
        %{}
      end

    {sc, added} =
      Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
        phrase = Safe.get(tok, :phrase)
        mw? = Safe.get(tok, :mw, false) or Safe.get(tok, :n, 1) > 1
        bucket = Map.get(acc, idx, [])

        cond do
          not mw? or is_nil(phrase) ->
            {acc, n}

          has_real_phrase_bucket?(bucket) ->
            {acc, n}

          not has_fallback_bucket?(bucket) ->
            {acc, n}

          true ->
            nk = norm_key(phrase)

            list =
              Map.get(phrase_cells_by_norm, nk, []) ++
                Map.get(db_phrase_cells_by_norm, nk, [])

            list = Enum.uniq_by(list, fn c -> to_string(Safe.get(c, :id) || "") end)

            case list do
              [] ->
                {acc, n}

              rows ->
                cands =
                  Enum.map(rows, fn c ->
                    id = Safe.get(c, :id) || "#{phrase}|phrase|0"
                    act = Safe.get(c, :activation, 0.25)

                    %{
                      id: to_string(id),
                      lemma: phrase,
                      norm: phrase,
                      mw: true,
                      pos: :phrase,
                      rel_prior: 0.30,
                      activation: act * 1.0,
                      score: 0.30,
                      source: :phrase_backfill
                    }
                  end)

                {Map.put(acc, idx, cands ++ bucket), n + length(cands)}
            end
        end
      end)

    if added > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  @doc """
  If a unigram token has no unigram candidates but `active_cells` has matching real cells,
  inject small-prior unigram candidates.
  """
  @spec backfill_unigrams_from_active_cells(map(), keyword()) :: map()
  def backfill_unigrams_from_active_cells(%{} = si, opts) when is_list(opts) do
    sc0 = Map.get(si, :sense_candidates, %{})
    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :active_cells, [])

    cells_by_norm =
      cells
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.reject(fn c ->
        id = to_string(Safe.get(c, :id) || "")
        String.contains?(id, "|phrase|")
      end)
      |> Enum.group_by(fn c ->
        norm_key(Safe.get(c, :norm) || Safe.get(c, :word))
      end)

    {sc, added} =
      tokens
      |> Enum.with_index()
      |> Enum.reduce({sc0, 0}, fn {tok, idx}, {acc, n} ->
        n_tok = Safe.get(tok, :n, 1)
        mw? = Safe.get(tok, :mw, n_tok > 1)

        surface =
          Safe.get(tok, :phrase) ||
            Safe.get(tok, :word) ||
            Safe.get(tok, :lemma)

        cond do
          mw? or is_nil(surface) ->
            {acc, n}

          true ->
            nk = norm_key(surface)
            bucket = Map.get(acc, idx, [])

            if Enum.any?(bucket, &unigram_candidate?/1) do
              {acc, n}
            else
              list0 = Map.get(cells_by_norm, nk, [])

              list =
                case list0 do
                  [] ->
                    db_backfill? =
                      Keyword.get(
                        opts,
                        :db_backfill?,
                        Application.get_env(:brain, :lifg_db_backfill, true)
                      )

                    if db_backfill? do
                      fetch_unigram_cells_by_norm([nk]) |> Map.get(nk, [])
                    else
                      []
                    end

                  xs ->
                    xs
                end

              case list do
                [] ->
                  {acc, n}

                rows ->
                  existing_ids =
                    bucket
                    |> Enum.map(fn c -> c[:id] || c["id"] end)
                    |> MapSet.new()

                  cands =
                    rows
                    |> Enum.map(&cell_to_unigram_candidate(&1, surface))
                    |> Enum.reject(&(is_nil(&1[:id]) or MapSet.member?(existing_ids, &1[:id])))

                  if cands == [] do
                    {acc, n}
                  else
                    safe_exec_telemetry(@unigram_backfill_event, %{count: length(cands)}, %{
                      token_index: idx,
                      norm: surface
                    })

                    {Map.put(acc, idx, cands ++ bucket), n + length(cands)}
                  end
              end
            end
        end
      end)

    if added > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  def backfill_unigrams_from_active_cells(si, _opts), do: si

  @doc """
  Build a debug map of heads per token index using child unigrams inside each MWE span.
  """
  @spec heads_for_indices(map(), [integer()]) :: map()
  def heads_for_indices(si, idxs) when is_list(idxs) do
    toks = Safe.get(si, :tokens, [])

    Enum.reduce(idxs, %{}, fn idx, acc ->
      heads =
        case Enum.at(toks, idx) do
          nil ->
            []

          tok ->
            span = Safe.get(tok, :span)

            toks
            |> Enum.with_index()
            |> Enum.filter(fn {t, j} ->
              j != idx and Safe.get(t, :n, 1) == 1 and inside?(Safe.get(t, :span), span)
            end)
            |> Enum.map(fn {t, _} ->
              Safe.get(t, :phrase) || Safe.get(t, :word) || Safe.get(t, :lemma) || ""
            end)
            |> Enum.map(&down/1)
            |> Enum.reject(&(&1 in ["", "a", "an", "the"]))
        end

      Map.put(acc, idx, heads)
    end)
  end

  def heads_for_indices(_si, _idxs), do: %{}

  # ── Private helpers ──────────────────────────────────────────────────

  defp fetch_phrase_cells_by_norm(norms) when is_list(norms) do
    norms = norms |> Enum.reject(&(&1 in [nil, ""])) |> Enum.uniq()

    if norms == [] do
      %{}
    else
      try do
        rows =
          Db.all(
            from(c in BrainCell,
              where: c.norm in ^norms and like(c.id, ^"%|phrase|%"),
              select: c
            )
          )

        rows
        |> Enum.map(&Safe.to_plain/1)
        |> Enum.group_by(fn c -> norm_key(Safe.get(c, :norm) || Safe.get(c, :word)) end)
      rescue
        _ -> %{}
      end
    end
  end

  defp fetch_unigram_cells_by_norm(norms) when is_list(norms) do
    norms = norms |> Enum.reject(&(&1 in [nil, ""])) |> Enum.uniq()

    if norms == [] do
      %{}
    else
      try do
        rows =
          Db.all(
            from(c in BrainCell,
              where: c.norm in ^norms and not like(c.id, ^"%|phrase|%"),
              select: c
            )
          )

        rows
        |> Enum.map(&Safe.to_plain/1)
        |> Enum.group_by(fn c -> norm_key(Safe.get(c, :norm) || Safe.get(c, :word)) end)
      rescue
        _ -> %{}
      end
    end
  end

  # “Compatible MWE” === has a *real* phrase sense, not our fallback.
  defp has_compatible_mwe?(sc, idx) do
    sc
    |> Map.get(idx, [])
    |> Enum.any?(&real_phrase_candidate?/1)
  end

  defp has_real_phrase_bucket?(bucket) when is_list(bucket),
    do: Enum.any?(bucket, &real_phrase_candidate?/1)

  defp has_real_phrase_bucket?(_), do: false

  defp has_fallback_bucket?(bucket) when is_list(bucket) do
    Enum.any?(bucket, fn c ->
      id = to_string(c[:id] || c["id"] || "")
      String.ends_with?(id, "|phrase|fallback")
    end)
  end

  defp has_fallback_bucket?(_), do: false

  defp real_phrase_candidate?(c) when is_map(c) do
    id = to_string(c[:id] || c["id"] || "")

    cond do
      id == "" ->
        false

      String.ends_with?(id, "|phrase|fallback") ->
        false

      String.contains?(id, "|phrase|") ->
        true

      true ->
        pos = c[:pos] || c["pos"]
        norm = to_string(c[:norm] || c["norm"] || c[:lemma] || c["lemma"] || "")
        pos in [:phrase, "phrase"] and String.contains?(norm, " ")
    end
  end

  defp real_phrase_candidate?(_), do: false

  # Neighbor heuristic: immediate neighbors as unigram companions
  defp unigram_neighbor_idxs(idx) when is_integer(idx),
    do: [idx - 1, idx + 1] |> Enum.filter(&(&1 >= 0))

  defp unigram_neighbor_idxs(_), do: []

  # IMPORTANT: spans are treated as {start, end}, not {start, len}
  defp inside?({s, e}, {ps, pe})
       when is_integer(s) and is_integer(e) and is_integer(ps) and is_integer(pe) do
    s >= ps and e <= pe
  end

  defp inside?(_, _), do: false

  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  defp down(_), do: ""

  defp norm_key(v) when is_binary(v), do: down(v)
  defp norm_key(_), do: ""

  defp cell_to_unigram_candidate(cell, surface) do
    raw_pos = Safe.get(cell, :pos) || Safe.get(cell, "pos") || :other

    pos_norm =
      raw_pos
      |> to_string()
      |> String.trim()
      |> String.downcase()
      |> String.replace(~r/\s+/, "_")
      |> String.replace("-", "_")

    raw_id = Safe.get(cell, :id) || Safe.get(cell, "id")

    id =
      case raw_id do
        nil ->
          "#{down(surface)}|#{pos_norm}|fallback"

        "" ->
          "#{down(surface)}|#{pos_norm}|fallback"

        v ->
          s = to_string(v)
          if s in ["", "nil"], do: "#{down(surface)}|#{pos_norm}|fallback", else: s
      end

    %{
      id: id,
      lemma: surface,
      norm: surface,
      mw: false,
      # FIX: canonicalize cand.pos to match synthesized id + tests ("proper noun" -> "proper_noun")
      pos: pos_norm,
      rel_prior: 0.20,
      activation: (Safe.get(cell, :activation, 0.25) || 0.25) * 1.0,
      score: 0.30,
      source: :active_cells
    }
  end

  defp unigram_candidate?(cand) when is_map(cand) do
    nrm = to_string(cand[:norm] || cand["norm"] || cand[:lemma] || cand["lemma"] || "")
    nrm != "" and not String.contains?(nrm, " ")
  end

  defp unigram_candidate?(_), do: false

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end

  # Reject fallback MWEs that start/end with function words (prevents "kick the", "the bucket", etc.)
  defp allow_fallback_phrase?(phrase) when is_binary(phrase) do
    parts =
      phrase
      |> down()
      |> String.split(" ", trim: true)

    case parts do
      [] ->
        false

      [_] ->
        false

      _ ->
        head = hd(parts)
        tail = List.last(parts)
        not (function_word_str?(head) or function_word_str?(tail))
    end
  end

  defp allow_fallback_phrase?(_), do: false

  defp function_word_str?(w) when is_binary(w) do
    w in @preps or w in @dets or w in @conjs or w in @auxes or w in @modals or w in @pron or
      w in @neg
  end

  defp function_word_str?(_), do: false
end

