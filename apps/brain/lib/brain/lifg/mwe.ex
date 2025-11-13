defmodule Brain.LIFG.MWE do
  @moduledoc """
  MWE + unigram-backfill helpers for LIFG.
  Extracted from `Brain.LIFG` to reduce size and isolate concerns.
  """

  alias Brain.Utils.Safe

  # ── Public entry points used by Brain.LIFG ────────────────────────────

  @doc """
  If a multiword token has no compatible MWE senses, synthesize a lightweight phrase
  `|phrase|fallback` candidate, then backfill real phrase cells from active_cells (if present).
  """
  def ensure_mwe_candidates(%{tokens: tokens} = si, opts) when is_list(tokens) do
    enable? =
      Keyword.get(
        opts,
        :mwe_fallback,
        Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
      )

    unless enable?, do: si

    sc0 = Map.get(si, :sense_candidates, %{})

    {sc, emitted} =
      Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
        token_n = Map.get(tok, :n, if(Map.get(tok, :mw, false), do: 2, else: 1))
        phrase = Map.get(tok, :phrase) || Map.get(tok, :lemma)
        mw? = Map.get(tok, :mw, token_n > 1)

        cond do
          not mw? or is_nil(phrase) ->
            {acc, n}

          has_compatible_mwe?(acc, idx) ->
            {acc, n}

          true ->
            # heuristic score: bounded so real phrase cells can win
            score_guess =
              acc
              |> unigram_neighbor_idxs(idx)
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

            safe_exec_telemetry([:brain, :pmtg, :mwe_fallback_emitted], %{count: 1}, %{
              token_index: idx,
              phrase: phrase,
              score: candidate.score
            })

            {updated, n + 1}
        end
      end)

    si1 = if emitted > 0, do: Map.put(si, :sense_candidates, sc), else: si
    backfill_real_mwe_from_active_cells(si1)
  end

  def ensure_mwe_candidates(si, _opts), do: si

  @doc """
  Opt-in: absorb unigram senses from active_cells into overlapping MWE buckets.
  Enabled with opts: `[absorb_unigrams_into_mwe?: true]`.
  """
  def absorb_unigrams_into_mwe(%{tokens: toks} = si, opts) when is_list(toks) do
    unless Keyword.get(opts, :absorb_unigrams_into_mwe?, false), do: si

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

  def absorb_unigrams_into_mwe(si, _opts), do: si

  @doc """
  If a multiword token only has a `|phrase|fallback`, inject real phrase cells from si.active_cells.
  """
  def backfill_real_mwe_from_active_cells(si) do
    sc0 = Map.get(si, :sense_candidates, %{})
    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :active_cells, [])

    phrase_cells_by_norm =
      cells
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.filter(fn c ->
        p = Safe.get(c, :pos)
        to_string(p) == "phrase"
      end)
      |> Enum.group_by(fn c ->
        String.downcase(to_string(Safe.get(c, :norm) || Safe.get(c, :word)))
      end)

    {sc, added} =
      Enum.reduce(Enum.with_index(tokens), {sc0, 0}, fn {tok, idx}, {acc, n} ->
        phrase = Safe.get(tok, :phrase)
        mw? = Safe.get(tok, :mw, false) or Safe.get(tok, :n, 1) > 1
        bucket = Map.get(acc, idx, [])

        has_real? =
          Enum.any?(bucket, fn c ->
            pos = Safe.get(c, :pos)
            norm = Safe.get(c, :norm) || Safe.get(c, :lemma) || ""
            (pos == :phrase or to_string(pos) == "phrase") and String.contains?(norm, " ")
          end)

        cond do
          not mw? or is_nil(phrase) or has_real? ->
            {acc, n}

          true ->
            norm_key = String.downcase(to_string(phrase))

            case Map.get(phrase_cells_by_norm, norm_key, []) do
              [] ->
                {acc, n}

              list ->
                cands =
                  Enum.map(list, fn c ->
                    id = Safe.get(c, :id) || "#{phrase}|phrase|0"

                    %{
                      id: to_string(id),
                      lemma: phrase,
                      norm: phrase,
                      mw: true,
                      pos: :phrase,
                      rel_prior: 0.30,
                      score: 0.30,
                      source: :active_cells
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
  def backfill_unigrams_from_active_cells(si, _opts) when is_map(si) do
    sc0 = Map.get(si, :sense_candidates, %{})
    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :active_cells, [])

    cells_by_norm =
      cells
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.reject(fn c ->
        pos = Safe.get(c, :pos)
        to_string(pos) == "phrase"
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

            have_unigram? =
              Enum.any?(bucket, &unigram_candidate?/1)

            if have_unigram? do
              {acc, n}
            else
              case Map.get(cells_by_norm, nk, []) do
                [] ->
                  {acc, n}

                list ->
                  existing_ids =
                    bucket
                    |> Enum.map(fn c -> c[:id] || c["id"] end)
                    |> MapSet.new()

                  cands =
                    list
                    |> Enum.map(&cell_to_unigram_candidate(&1, surface))
                    |> Enum.reject(&(is_nil(&1[:id]) or MapSet.member?(existing_ids, &1[:id])))

                  if cands == [] do
                    {acc, n}
                  else
                    :telemetry.execute(
                      [:brain, :lifg, :unigram_backfill_emitted],
                      %{count: length(cands)},
                      %{token_index: idx, norm: surface}
                    )

                    {Map.put(acc, idx, cands ++ bucket), n + length(cands)}
                  end
              end
            end
        end
      end)

    if added > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  @doc """
  Build a debug map of heads per token index using child unigrams inside each MWE span.
  """
  def heads_for_indices(si, idxs) do
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

  # ── Private helpers ──────────────────────────────────────────────────

  defp has_compatible_mwe?(sc, idx) do
    sc
    |> Map.get(idx, [])
    |> Enum.any?(fn c ->
      norm = c[:norm] || c["norm"] || c[:lemma] || c["lemma"] || ""
      String.contains?(norm, " ")
    end)
  end

  # Neighbor heuristic: immediate neighbors as unigram companions
  defp unigram_neighbor_idxs(_sc, idx), do: [idx - 1, idx + 1] |> Enum.filter(&(&1 >= 0))

  defp inside?({s, l}, {ps, pl})
       when is_integer(s) and is_integer(l) and is_integer(ps) and is_integer(pl) do
    e = s + l
    pe = ps + pl
    s >= ps and e <= pe
  end

  defp inside?(_, _), do: false

  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  defp down(_), do: ""

  defp norm_key(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  defp norm_key(_), do: ""

  defp cell_to_unigram_candidate(cell, surface) do
    id = Safe.get(cell, :id) || Safe.get(cell, "id")
    pos = Safe.get(cell, :pos) || Safe.get(cell, "pos") || :other

    %{
      id: to_string(id),
      lemma: surface,
      norm: surface,
      mw: false,
      pos: pos,
      # Small priors that let real phrase/unigram cells compete fairly:
      rel_prior: 0.20,
      # If DB carries an activation, pass it through; else a gentle baseline
      activation:
        (Safe.get(cell, :activation, Safe.get(cell, :modulated_activation, 0.25)) || 0.25) * 1.0,
      score: 0.30,
      source: :active_cells
    }
  end

  defp unigram_candidate?(cand) do
    nrm = cand[:norm] || cand["norm"] || cand[:lemma] || cand["lemma"] || ""
    nrm = to_string(nrm)
    nrm != "" and not String.contains?(nrm, " ")
  end

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end
