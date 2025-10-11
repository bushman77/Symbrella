# apps/core/lib/core/lexicon.ex
defmodule Core.Lexicon do
  @moduledoc ~S"""
  Core ↔ Lexicon wiring.

  • `all/2` — run the lexicon stage (scrape/normalize/upsert), seed **only MWE fallbacks**,
    then re-query storage by norms and merge into SI.
  • `lookup/1` — proxy to the external `Lexicon` client (guarded).
  • `ensure_cells_from_tokens/1` — **MWE-only** seeding:
      - If the token is a phrase (contains a space), try to attach a **real POS**:
          · POS from `lookup/1` of the phrase
          · POS from the phrase’s head word via tokens and/or `lookup/1`
        Insert one fallback per POS found: `norm|<pos>|fallback`.
        If none found, insert a conservative `norm|phrase|fallback`.
      - If the token is a unigram → **do not seed any fallback** (indexed senses only).
      - Never insert a fallback when any (norm, pos) already exists in DB **or** is
        already planned in this run (prevents `"phrase|fallback"` appearing alongside
        `pos|fallback` for the same phrase).
  • `bulk_upsert_senses/1` — delegate to DB (guarded & sanitized).

  Invariants:
  - Never insert `|unk|`/`|seed|` segments.
  - Only allowed POS are persisted.
  - IDs are canonical: "norm|pos|suffix" (suffix is "fallback" for seeds).

  Rationale:
  - BrainCells for unigrams should come from real, indexed senses (e.g., `|pos|1`/`|pos|2`…).
    Fallbacks are reserved for MWEs to keep the graph useful without polluting unigrams.
  """

  alias Core.SemanticInput, as: SI
  alias Core.Lexicon.Stage
  alias Db.Lexicon, as: DbLex
  alias Db

  require Logger

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle
  )

  @id_regex ~r/^[^|]+?\|(noun|verb|adjective|adverb|interjection|phrase|proper_noun|pronoun|determiner|preposition|conjunction|numeral|particle)(\|[A-Za-z0-9_]+)?$/

  @atomizable_keys ~w(
    id word norm pos definition example gram_function
    synonyms antonyms semantic_atoms
    type status activation modulated_activation dopamine serotonin
    position connections last_dose_at last_substance token_id
    inserted_at updated_at
  )a
  @atomizable_key_strings Enum.map(@atomizable_keys, &Atom.to_string/1)

  # ───────────────────────── Public API ─────────────────────────

  @doc """
  Run the lexicon stage (if enabled), seed **MWE** fallbacks only, then re-query storage
  by norms and merge into `SI.active_cells`.

  NOTE: Stage inserts **indexed senses** first; then we seed phrase fallbacks and
  skip any (norm, POS) that already exist.
  """
  @spec all(SI.t(), Keyword.t()) :: SI.t()
  def all(%SI{} = si, opts \\ []) do
    if Keyword.get(opts, :lexicon_stage?, true) do
      si1 = Stage.run(si)

      tokens = Map.get(si1, :tokens, [])
      _ = ensure_cells_from_tokens(tokens)

      norms =
        tokens
        |> tokens_to_norms()

      rows = if norms == [], do: [], else: safe_fetch_by_norms(norms)

      if rows == [] do
        si1
      else
        merged =
          (si1.active_cells || [])
          |> Kernel.++(rows)
          |> dedup_by_id()

        %{si1 | active_cells: merged}
      end
    else
      si
    end
  end

  @doc """
  Proxy to the external `Lexicon` app (used by LV fallback display). Guarded.

  Uses dynamic dispatch (`apply/3`) to avoid compile-time warnings when
  only one arity is available in the external client.
  """
  @spec lookup(String.t()) :: map()
  def lookup(word) when is_binary(word) do
    try do
      if Code.ensure_loaded?(Lexicon) do
        cond do
          function_exported?(Lexicon, :lookup, 1) ->
            apply(Lexicon, :lookup, [word])

          function_exported?(Lexicon, :lookup, 2) ->
            apply(Lexicon, :lookup, [word, 8])

          true ->
            %{word: word, senses: []}
        end
      else
        %{word: word, senses: []}
      end
    rescue
      _ -> %{word: word, senses: []}
    catch
      _, _ -> %{word: word, senses: []}
    end
  end

  @doc ~S"""
  **MWE-only** seeding of BrainCells.

  For each token:
    • If `norm` contains a space → collect POS candidates from:
        - `lookup(norm)` (phrase senses)
        - head-word POS inferred from tokens and/or `lookup(head)`
      Subtract any POS already present in DB for that norm, and subtract any POS
      **already planned in this run** to avoid intra-turn races.
      Insert one row per remaining POS as `norm|<pos>|fallback`.

      If no POS found (and neither DB nor planned rows for this norm), insert a conservative
      `norm|phrase|fallback`.

    • Else (unigram): **skip** (no fallback).

  Telemetry:
    - `[:core, :lexicon, :seed_phrase_with_pos_emitted]` %{norm, poses}
    - `[:core, :lexicon, :seed_fallback_emitted]` %{norm}
    - `[:core, :lexicon, :seed_unigram_skipped]` %{norm, reason: :unigram_no_fallback}
  """
  @spec ensure_cells_from_tokens([map()]) :: :ok
  def ensure_cells_from_tokens(tokens) when is_list(tokens) do
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    # Memoize within a single call:
    #   - surface → list(pos) discovered via lookup()
    #   - {:existing, norm} → MapSet(pos) already present in DB
    #   - {:planned, norm}  → MapSet(pos) scheduled to be inserted in this run
    cache = :ets.new(:lexicon_cache, [:set, :private])

    {rows, _skips, _planned} =
      tokens
      |> Enum.reduce({[], [], %{}}, fn tok, {rows_acc, skip_acc, planned} ->
        norm = tok_norm(tok)
        kind = tok_kind(tok)

        cond do
          not is_binary(norm) or norm == "" ->
            {rows_acc, skip_acc, planned}

          kind in [:punct, "punct"] ->
            {rows_acc, [norm | skip_acc], planned}

          String.contains?(norm, " ") ->
            # PHRASE (MWE): try to attach a real POS, then subtract existing & planned POS
            phrase_poses = cached_allowed_pos_set(norm, cache)

            head = rightmost_content_word(norm)

            head_token_pos =
              tokens
              |> token_pos_for_surface(head)
              |> pos_alias_to_allowed()
              |> case do
                p when is_binary(p) and p in @allowed_pos -> [p]
                _ -> []
              end

            head_lookup_poses =
              if is_binary(head) and head != "" do
                cached_allowed_pos_set(head, cache)
              else
                []
              end

            existing = existing_pos_set(norm, cache)
            already_planned = Map.get(planned, norm, MapSet.new())

            pos_set =
              (phrase_poses ++ head_token_pos ++ head_lookup_poses)
              |> Enum.filter(&(&1 in @allowed_pos))
              |> Enum.uniq()
              |> MapSet.new()
              |> MapSet.difference(existing)
              |> MapSet.difference(already_planned)
              |> MapSet.to_list()

            cond do
              pos_set != [] ->
                {added_rows, rows_acc2, planned2} =
                  Enum.reduce(pos_set, {[], rows_acc, planned}, fn pos, {added, acc, pl} ->
                    row = %{
                      id: "#{norm}|#{pos}|fallback",
                      status: "active",
                      type: "fallback",
                      norm: norm,
                      pos: pos,
                      word: norm,
                      inserted_at: now,
                      updated_at: now
                    }

                    case validate_row(row) do
                      :ok ->
                        new_pl =
                          Map.update(pl, norm, MapSet.new([pos]), fn set -> MapSet.put(set, pos) end)

                        {[row | added], [row | acc], new_pl}

                      {:error, _} ->
                        {added, acc, pl}
                    end
                  end)

                :telemetry.execute(
                  [:core, :lexicon, :seed_phrase_with_pos_emitted],
                  %{count: length(added_rows)},
                  %{norm: norm, poses: pos_set}
                )

                {Enum.uniq_by(added_rows ++ rows_acc2, & &1.id), skip_acc, planned2}

              true ->
                # Nothing deduced — only insert phrase|fallback if:
                #  - DB doesn't already have phrase
                #  - not already planned any POS (including 'phrase') for this norm
                already_planned_any? =
                  case Map.get(planned, norm) do
                    nil -> false
                    set when is_struct(set, MapSet) -> MapSet.size(set) > 0
                    _ -> true
                  end

                if not MapSet.member?(existing, "phrase") and not already_planned_any? do
                  row = %{
                    id: "#{norm}|phrase|fallback",
                    status: "active",
                    type: "fallback",
                    norm: norm,
                    pos: "phrase",
                    word: norm,
                    inserted_at: now,
                    updated_at: now
                  }

                  case validate_row(row) do
                    :ok ->
                      planned2 =
                        Map.update(planned, norm, MapSet.new(["phrase"]), fn set ->
                          MapSet.put(set, "phrase")
                        end)

                      :telemetry.execute(
                        [:core, :lexicon, :seed_fallback_emitted],
                        %{count: 1},
                        %{norm: norm}
                      )

                      {[row | rows_acc], skip_acc, planned2}

                    {:error, _} ->
                      {rows_acc, skip_acc, planned}
                  end
                else
                  {rows_acc, skip_acc, planned}
                end
            end

          true ->
            # UNIGRAM: do not seed fallbacks
            :telemetry.execute(
              [:core, :lexicon, :seed_unigram_skipped],
              %{count: 1},
              %{norm: norm, reason: :unigram_no_fallback}
            )

            {rows_acc, [norm | skip_acc], planned}
        end
      end)

    # Persist validated rows
    _ =
      if rows != [] do
        Db.insert_all(Db.BrainCell, rows, on_conflict: :nothing)
      end

    :ets.delete(cache)
    :ok
  end

  # Back-compat shim
  @spec ensure_cells([String.t()]) :: :ok
  def ensure_cells(norms) when is_list(norms) do
    tokens = Enum.map(norms, &%{phrase: &1})
    ensure_cells_from_tokens(tokens)
  end

  @doc """
  DB bulk upsert for senses (called by Stage). Guarded **and sanitized**.
  """
  @spec bulk_upsert_senses(list()) :: :ok
  def bulk_upsert_senses(rows) when is_list(rows) do
    sanitized =
      rows
      |> Enum.map(&normalize_row/1)
      |> Enum.reduce([], fn row, acc ->
        case validate_row(row) do
          :ok -> [row | acc]
          {:error, reason} ->
            id = Map.get(row, :id) || Map.get(row, "id")
            pos = Map.get(row, :pos) || Map.get(row, "pos")
            :telemetry.execute(
              [:core, :lexicon, :placeholder_rejected],
              %{count: 1},
              %{id: id, pos: pos, reason: reason}
            )
            acc
        end
      end)

    try do
      if sanitized != [] and Code.ensure_loaded?(DbLex) and
           function_exported?(DbLex, :bulk_upsert_senses, 1) do
        DbLex.bulk_upsert_senses(Enum.reverse(sanitized))
      else
        :ok
      end
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  def bulk_upsert_senses(_), do: :ok

  # ─── helpers ──────────────────────────────────────────────────────────────

  defp safe_fetch_by_norms(norms) when is_list(norms) do
    try do
      if Code.ensure_loaded?(DbLex) and function_exported?(DbLex, :fetch_by_norms, 1) do
        DbLex.fetch_by_norms(norms)
      else
        []
      end
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  defp tokens_to_norms(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(&tok_norm/1)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp tokens_to_norms(_), do: []

  # —— token extraction & helpers ——

  defp tok_norm(%{phrase: p}) when is_binary(p), do: normalize(p)
  defp tok_norm(%{"phrase" => p}) when is_binary(p), do: normalize(p)
  defp tok_norm(%{word: w}) when is_binary(w), do: normalize(w)
  defp tok_norm(%{"word" => w}) when is_binary(w), do: normalize(w)
  defp tok_norm(_), do: ""

  defp tok_kind(%{kind: k}), do: k
  defp tok_kind(%{"kind" => k}), do: k
  defp tok_kind(_), do: nil

  defp normalize(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  defp normalize(_), do: ""

  # Return **all** allowed POS discovered via lookup for a surface (word or phrase)
  defp cached_allowed_pos_set(surface, tab) when is_binary(surface) do
    case :ets.lookup(tab, surface) do
      [{^surface, poses}] when is_list(poses) ->
        poses

      [] ->
        poses = allowed_pos_from_lookup(surface)
        :ets.insert(tab, {surface, poses})
        poses
    end
  end

  defp allowed_pos_from_lookup(surface) when is_binary(surface) do
    case lookup(surface) do
      %{senses: senses} when is_list(senses) ->
        senses
        |> Enum.map(&Map.get(&1, :pos) || Map.get(&1, "pos"))
        |> Enum.map(&pos_alias_to_allowed/1)
        |> Enum.filter(&(&1 in @allowed_pos))
        |> Enum.uniq()

      _ ->
        []
    end
  end

  # Track which POS already exist in DB for a given norm; memoized per call.
  defp existing_pos_set(norm, tab) when is_binary(norm) do
    case :ets.lookup(tab, {:existing, norm}) do
      [{{:existing, ^norm}, set}] ->
        set

      [] ->
        rows = safe_fetch_by_norms([norm])
        poses =
          rows
          |> Enum.map(fn r -> Map.get(r, :pos) || Map.get(r, "pos") end)
          |> Enum.filter(&is_binary/1)
          |> Enum.map(&String.downcase/1)

        set = MapSet.new(poses)
        :ets.insert(tab, {{:existing, norm}, set})
        set
    end
  end

  # Head word = rightmost non-empty token from the phrase
  defp rightmost_content_word(norm) when is_binary(norm) do
    norm
    |> String.split(~r/\s+/, trim: true)
    |> Enum.reverse()
    |> Enum.find(fn w -> is_binary(w) and w != "" end)
  end

  # Try to read POS for a given surface from the provided tokens (no guards calling local fns).
  defp token_pos_for_surface(tokens, surface) when is_list(tokens) and is_binary(surface) do
    down = normalize(surface)

    Enum.find_value(tokens, fn tok ->
      candidate =
        cond do
          is_binary(Map.get(tok, :phrase)) -> Map.get(tok, :phrase)
          is_binary(Map.get(tok, "phrase")) -> Map.get(tok, "phrase")
          is_binary(Map.get(tok, :word)) -> Map.get(tok, :word)
          is_binary(Map.get(tok, "word")) -> Map.get(tok, "word")
          true -> nil
        end

      pos =
        Map.get(tok, :pos) ||
          Map.get(tok, "pos") ||
          Map.get(tok, :tag) ||
          Map.get(tok, "tag")

      if is_binary(candidate) and normalize(candidate) == down and is_binary(pos),
        do: pos,
        else: nil
    end)
  end

  defp token_pos_for_surface(_, _), do: nil

  # POS aliasing
  defp pos_alias_to_allowed(nil), do: nil
  defp pos_alias_to_allowed(pos) when is_atom(pos),
    do: pos |> Atom.to_string() |> pos_alias_to_allowed()

  defp pos_alias_to_allowed(pos) when is_binary(pos) do
    p = pos |> String.trim() |> String.downcase()

    case p do
      # Already canonical
      c when c in @allowed_pos -> c

      # Universal / common aliases
      "n"      -> "noun"
      "v"      -> "verb"
      "adj"    -> "adjective"
      "adjective" -> "adjective"
      "adv"    -> "adverb"
      "adverb" -> "adverb"
      "intj"   -> "interjection"
      "interj" -> "interjection"
      "propn"  -> "proper_noun"
      "proper" -> "proper_noun"
      "pron"   -> "pronoun"
      "det"    -> "determiner"
      "adp"    -> "preposition"
      "prep"   -> "preposition"
      "cconj"  -> "conjunction"
      "sconj"  -> "conjunction"
      "conj"   -> "conjunction"
      "num"    -> "numeral"
      "part"   -> "particle"

      # English tagset hints
      "aux"    -> "verb"
      "modal"  -> "verb"

      # we never persist these
      "punct"  -> nil
      "sym"    -> nil
      "x"      -> nil
      "unk"    -> nil

      _ -> nil
    end
  end

  defp get_id(%Db.BrainCell{id: id}), do: id
  defp get_id(%{id: id}) when is_binary(id), do: id
  defp get_id(%{"id" => id}) when is_binary(id), do: id
  defp get_id(_), do: nil

  defp dedup_by_id(list) do
    {acc, _seen} =
      Enum.reduce(list, {[], MapSet.new()}, fn x, {acc, seen} ->
        case get_id(x) do
          nil -> {acc, seen}
          id ->
            if MapSet.member?(seen, id) do
              {acc, seen}
            else
              {[x | acc], MapSet.put(seen, id)}
            end
        end
      end)

    Enum.reverse(acc)
  end

  # --- sanitation / validation for outbound writes -------------------------

  defp normalize_row(row) when is_map(row) do
    row
    |> normalize_keys()
    |> then(fn r ->
      id   = Map.get(r, :id)
      pos  = Map.get(r, :pos)
      word = Map.get(r, :word)
      norm = Map.get(r, :norm) || (if is_binary(word), do: normalize(word), else: nil)

      r
      |> Map.put(:id,   if(is_binary(id),   do: id   |> String.trim() |> String.replace(~r/\s+/, " "), else: id))
      |> Map.put(:pos,  if(is_binary(pos),  do: pos  |> String.trim() |> String.downcase(),             else: pos))
      |> Map.put(:word, if(is_binary(word), do: word |> String.trim() |> String.replace(~r/\s+/, " "), else: word))
      |> Map.put(:norm, norm)
    end)
  end

  defp normalize_row(_other), do: %{}

  defp normalize_keys(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {k, v}, acc ->
      key_atom =
        cond do
          is_atom(k) and k in @atomizable_keys ->
            k

          is_binary(k) and k in @atomizable_key_strings ->
            String.to_atom(k)

          true ->
            nil
        end

      if key_atom, do: Map.put(acc, key_atom, v), else: acc
    end)
  end

  defp validate_row(%{id: id, pos: pos})
       when is_binary(id) and is_binary(pos) do
    cond do
      String.contains?(id, "|unk|") or String.contains?(id, "|seed|") ->
        {:error, :placeholder_in_id}

      pos == "unk" ->
        {:error, :unknown_pos}

      pos not in @allowed_pos ->
        {:error, {:pos_not_allowed, pos}}

      not Regex.match?(@id_regex, id) ->
        {:error, :bad_id_shape}

      split_pos(id) != pos ->
        {:error, :id_pos_mismatch}

      true ->
        :ok
    end
  end

  defp validate_row(_), do: {:error, :incomplete_row}

  defp split_pos(id) do
    id
    |> String.split("|", trim: true)
    |> Enum.at(1)
  end
end

