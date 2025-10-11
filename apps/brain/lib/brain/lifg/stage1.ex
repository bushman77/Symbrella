defmodule Brain.LIFG.Stage1 do
  @moduledoc """
  Stage 1 — fast per-token disambiguation from `si.sense_candidates`.

  ## Guardrail highlights (P-202)
  • HARD REJECT: char-grams → emits [:brain,:lifg,:chargram_violation] and drops.
  • BOUNDARY GUARD: non word-boundary substrings dropped unless `mw: true`,
    emits [:brain,:lifg,:boundary_drop].
  • PUNCT SKIP: punctuation tokens (`kind: :punct` or `pos: "punct"`) are ignored for sense
    competition (no boundary or char-gram checks, no warnings).
  • Deterministic tie-breaks, safe spans, and clause-aware "what" nudges.
  • Optional MWE fallback injection, emits [:brain,:pmtg,:mwe_fallback_emitted].

  ### Options
    :weights          — %{lex_fit, rel_prior, activation, intent_bias}
    :scores           — :all | :top2 | :none (default from `:lifg_stage1_scores_mode`)
    :chargram_event   — telemetry (default [:brain, :lifg, :chargram_violation])
    :boundary_event   — telemetry (default [:brain, :lifg, :boundary_drop])
    :margin_threshold — default 0.15 (for alt_id emission)
    :mwe_fallback     — when true (or env `:lifg_stage1_mwe_fallback`), injects a
                        conservative fallback MWE candidate if an MWE token lacks
                        compatible MWE senses.
  """

  require Logger

  @type si :: map()
  @type choice ::
          %{
            required(:token_index) => non_neg_integer(),
            required(:chosen_id) => String.t(),
            required(:alt_ids) => [String.t()],
            required(:margin) => float(),
            optional(:scores) => map()
          }

  # ─────────────────────────────────────────────────────────────────────────────

  @spec run(si(), keyword()) ::
          {:ok, %{si: si(), choices: [choice()], audit: map()}} | {:error, term()}
  def run(si, opts \\ []) when is_map(si) and is_list(opts) do
    try do
      t0 = System.monotonic_time()

      # Optionally inject conservative MWE candidates
      si1 = ensure_mwe_candidates(si, opts)

      tokens = Map.get(si1, :tokens, [])

      slate =
        case Map.get(si1, :sense_candidates, Map.get(si1, :candidates_by_token, %{})) do
          %{} = s -> s
          _ -> %{}
        end

      {kept_tokens, dropped} = guard_tokens(tokens, si1, opts)

      scores_mode =
        Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

      margin_threshold = Keyword.get(opts, :margin_threshold, 0.15)
      w = weights(opts)

      choices =
        kept_tokens
        |> Enum.map(fn tok ->
          disambiguate_token(
            tok,
            Map.get(slate, Map.get(tok, :index, 0), []),
            scores: scores_mode,
            margin_threshold: margin_threshold,
            weights: w,
            si: si1
          )
        end)
        |> Enum.reject(&is_nil/1)

      audit = %{
        stage: :lifg_stage1,
        token_count: length(tokens),
        kept_tokens: length(kept_tokens),
        dropped_tokens: length(dropped),
        timing_ms: System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)
      }

      {:ok, %{si: si1, choices: choices, audit: audit}}
    rescue
      e ->
        Logger.error("LIFG Stage1 run failed: #{inspect(e)}")
        {:error, e}
    end
  end

  # Back-compat shim (si, ctx, opts)
  @spec run(si(), map(), keyword()) ::
          {:ok, %{si: si(), choices: [choice()], audit: map()}} | {:error, term()}
  def run(si, _ctx, opts), do: run(si, opts)

  # ── Guards ─────────────────────────────────────────────────────────

  defp guard_tokens(tokens, si, opts) when is_list(tokens) and is_map(si) do
    char_ev = Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation])
    bnd_ev = Keyword.get(opts, :boundary_event, [:brain, :lifg, :boundary_drop])

    Enum.reduce(tokens, {[], []}, fn tok, {kept, dropped} ->
      cond do
        # punctuation tokens are ignored for sense competition (quietly dropped)
        is_punct_token?(tok) ->
          {kept, [tok | dropped]}

        is_chargram?(tok, si) ->
          emit(char_ev, %{}, %{
            token_index: Map.get(tok, :index),
            phrase: Map.get(tok, :phrase),
            span: tok_span(tok)
          })

          {kept, [tok | dropped]}

        not boundary_ok?(tok, si) ->
          emit(bnd_ev, %{}, %{
            token_index: Map.get(tok, :index),
            phrase: Map.get(tok, :phrase),
            span: tok_span(tok),
            mw: Map.get(tok, :mw, false)
          })

          {kept, [tok | dropped]}

        true ->
          {[tok | kept], dropped}
      end
    end)
    |> then(fn {k, d} -> {Enum.reverse(k), Enum.reverse(d)} end)
  end

  defp guard_tokens(_tokens, _si, _opts), do: {[], []}

  # Treat as punctuation if explicitly marked or pos=='punct'
  defp is_punct_token?(tok) do
    kind = Map.get(tok, :kind) || Map.get(tok, "kind")
    pos = Map.get(tok, :pos) || Map.get(tok, "pos")
    kind in [:punct, "punct"] or pos in [:punct, "punct"]
  end

  # explicit flags OR (short, unigram, misaligned fragment) ⇒ char-gram
  defp is_chargram?(tok, si) when is_map(tok) and is_map(si) do
    flagged? =
      Map.get(tok, :source) in [:chargram, "chargram"] or
        Map.get(tok, :kind) in [:chargram, "chargram"] or
        Map.get(tok, :chargram) in [true, "true"]

    if flagged? do
      true
    else
      phrase = (Map.get(tok, :phrase) || "") |> to_string() |> String.trim()
      n = Map.get(tok, :n, 1)
      mw? = Map.get(tok, :mw, false)

      short? = String.length(phrase) <= 2
      unigramish? = n == 1 and not String.contains?(phrase, " ")

      # treat as char-gram when short, unigram, misaligned (and not mw)
      unigramish? and short? and not boundary_ok?(Map.put(tok, :mw, false), si) and not mw?
    end
  end

  defp is_chargram?(_, _), do: false

  defp boundary_ok?(tok, si) when is_map(tok) and is_map(si) do
    # MWEs bypass strict boundary enforcement
    mw? = Map.get(tok, :mw) || Map.get(tok, "mw") || false
    if mw?, do: true, else: do_boundary_check(tok, si)
  end

  defp boundary_ok?(_tok, _si), do: false

  defp do_boundary_check(tok, si) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence") || ""

    cond do
      not is_binary(sentence) ->
        true

      true ->
        {s, e} = tok_span(tok)
        len = String.length(sentence)

        left_ok? = s == 0 or not letter_or_number?(String.at(sentence, s - 1))
        right_ok? = e >= len or not letter_or_number?(String.at(sentence, e))
        left_ok? and right_ok?
    end
  end

  # Normalize span safely; tolerate missing / alternate shapes.
  defp tok_span(%{span: {s, e}}) when is_integer(s) and is_integer(e), do: {s, e}
  defp tok_span(%{start: s, stop: e}) when is_integer(s) and is_integer(e), do: {s, e}
  defp tok_span(%{start: s, length: len}) when is_integer(s) and is_integer(len), do: {s, s + len}

  defp tok_span(%{index: i} = tok) when is_integer(i) do
    phrase_len =
      (Map.get(tok, :phrase) || Map.get(tok, :word) || Map.get(tok, :lemma) || "")
      |> to_string()
      |> String.length()

    if phrase_len > 0 do
      {max(i, 0), max(i, 0) + phrase_len}
    else
      emit([:brain, :lifg, :missing_span], %{count: 1}, %{index: i})
      {max(i, 0), max(i, 0) + 1}
    end
  end

  defp tok_span(_tok) do
    emit([:brain, :lifg, :missing_span], %{count: 1}, %{})
    {0, 0}
  end

  defp letter_or_number?(nil), do: false
  defp letter_or_number?(ch), do: String.match?(ch, ~r/[\p{L}\p{N}]/u)

  # ── Per-token disambiguation ────────────────────────────────────────

  defp disambiguate_token(%{} = tok, cand_list, opts) when is_list(cand_list) do
    idx = Map.get(tok, :index, 0)
    thr = Keyword.get(opts, :margin_threshold, 0.15)
    scores_mode = Keyword.get(opts, :scores, :all)
    w = Map.new(Keyword.get(opts, :weights, []), fn {k, v} -> {k, v} end)
    si = Keyword.get(opts, :si, %{})

    cand_list =
      cand_list
      |> Enum.reject(&is_nil/1)
      |> Enum.map(&mapify/1)
      |> Enum.reject(&(is_nil(&1.id) or not is_binary(&1.id)))
      |> Enum.uniq_by(& &1.id)
      |> prefer_salutation_interjection(tok)
      |> prefer_pronoun_for_I(tok)
      |> prefer_interrogative_what(tok, si)
      |> compat_filter_for_token(tok)

    raw_scored =
      Enum.map(cand_list, fn c ->
        f = extract_features(c, tok, si)

        lex = as_float(f[:lex_fit])
        rel = as_float(f[:rel_prior])
        act = as_float(f[:activation])
        ib = as_float(f[:intent_bias])

        # base + micro nudges
        bias = context_bias(c, tok, si) + dialect_penalty(c, si)

        base =
          (w[:lex_fit] || 0.4) * lex +
            (w[:rel_prior] || 0.3) * rel +
            (w[:activation] || 0.2) * act +
            (w[:intent_bias] || 0.1) * ib +
            bias

        # deterministic micro-nudge to break ties without randomness
        eps = :erlang.phash2(c.id, 1000) / 1_000_000.0

        %{
          id: c.id,
          raw: base + eps,
          veto?: Map.get(c, :veto?, false)
        }
      end)

    if raw_scored == [] do
      nil
    else
      probs =
        raw_scored
        |> Enum.map(& &1.raw)
        |> Brain.LIFG.normalize_scores()

      scored =
        Enum.zip(raw_scored, probs)
        |> Enum.map(fn {%{id: id, raw: r, veto?: veto?}, p} ->
          %{id: id, score: p, raw: r, veto?: veto?}
        end)
        |> Enum.sort_by(&(-&1.score))

      [%{id: top_id, score: p1} | rest] = scored

      second_p =
        case rest |> Enum.reject(&(&1.id == top_id)) do
          [%{score: s2} | _] -> s2
          _ -> 0.0
        end

      alt_ids =
        if max(p1 - second_p, 0.0) < thr and rest != [] do
          [hd(rest).id]
        else
          []
        end

      choice0 = %{
        token_index: idx,
        chosen_id: top_id,
        alt_ids: alt_ids,
        margin: max(p1 - second_p, 0.0),
        scores: build_scores_map(scored, scores_mode),
        ranked: scored,
        audit: %{guards: :ok}
      }

      choice = Brain.LIFG.Reanalysis.maybe_promote(choice0)
      Map.drop(choice, [:ranked, :audit])
    end
  end

  defp disambiguate_token(_tok, _cand_list, _opts), do: nil

  # ── Candidate list tweaks ───────────────────────────────────────────

  # Prefer interjection "greeting" senses for salutations (“hello/hi/hey”), esp. at start
  defp prefer_salutation_interjection(list, tok) when is_list(list) and is_map(tok) do
    phrase = (Map.get(tok, :phrase) || "") |> to_string()

    at_start =
      case tok_span(tok) do
        {0, _} -> true
        _ -> Map.get(tok, :index, 0) == 0
      end

    sal? = Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) or at_start

    if sal? do
      greet =
        Enum.filter(list, fn c when is_map(c) ->
          pos = extract_pos(c)

          defn =
            (Map.get(c, :definition) || get_in(c, [:features, :definition]) || "")
            |> to_string()
            |> String.downcase()

          syns =
            (Map.get(c, :synonyms) || get_in(c, [:features, :synonyms]) || [])
            |> Enum.map(&String.downcase/1)

          String.contains?(pos, "interjection") and
            (String.contains?(defn, "greet") or Enum.any?(syns, &(&1 == "greeting")))
        end)

      if greet != [], do: greet, else: list
    else
      list
    end
  end

  defp prefer_salutation_interjection(list, _tok), do: list

  # Pronoun preference for "I", plus contractions + missing apostrophes
  defp prefer_pronoun_for_I(list, tok) when is_list(list) and is_map(tok) do
    phrase =
      (Map.get(tok, :phrase) || Map.get(tok, "phrase") || "")
      |> to_string()

    norm = String.downcase(phrase)

    if phrase == "I" or norm in ["i'm", "i’m", "im"] do
      pronounish =
        Enum.filter(list, fn c when is_map(c) -> pos_pronoun?(c) or id_pronounish?(c) end)

      if pronounish == [], do: list, else: pronounish
    else
      list
    end
  end

  defp prefer_pronoun_for_I(list, _tok), do: list

  # Prefer interrogative/determiner/relative "what" for clause openings
  defp prefer_interrogative_what(list, tok, si)
       when is_list(list) and is_map(tok) and is_map(si) do
    norm =
      (Map.get(tok, :phrase) || Map.get(tok, "phrase") || "")
      |> to_string()
      |> String.downcase()

    if norm != "what" do
      list
    else
      nx = next_token_phrase(tok, si) |> normalize_apostrophe() |> String.downcase()
      nx2 = next2_token_phrase(tok, si) |> normalize_apostrophe() |> String.downcase()

      pronouny_follow? =
        nx in ~w(im i'm i’m i we you he she they it you're we're)

      verbish_follow? =
        nx2 in ~w(trying try doing do mean saying think thinking going gonna want wanta wanna)

      interrogativeish =
        Enum.filter(list, fn c when is_map(c) ->
          pos = extract_pos(c)
          id = (c[:id] || "") |> to_string() |> String.downcase()

          String.contains?(pos, "pron") or
            String.contains?(pos, "det") or
            String.contains?(pos, "interrog") or
            String.contains?(id, "pron") or
            String.contains?(id, "det")
        end)

      cond do
        (pronouny_follow? or verbish_follow?) and interrogativeish != [] ->
          interrogativeish

        locale(si) not in ["en-sg", "sg", "en_sg"] ->
          prefer_non_indefinite_what(list)

        true ->
          list
      end
    end
  end

  defp prefer_interrogative_what(list, _tok, _si), do: list

  defp prefer_non_indefinite_what(list) do
    {bad, good} = Enum.split_with(list, &what_indefinite?/1)
    if good == [], do: list, else: good ++ bad
  end

  defp what_indefinite?(c) do
    gloss =
      (Map.get(c, :definition) || get_in(c, [:features, :definition]) || "")
      |> to_string()
      |> String.downcase()

    syns =
      (Map.get(c, :synonyms) || get_in(c, [:features, :synonyms]) || [])
      |> Enum.map(&to_string/1)
      |> Enum.map(&String.downcase/1)

    String.contains?(gloss, "something") or
      Enum.any?(syns, &(&1 in ~w(thing things stuff something)))
  end

  # ---- lookahead helpers --------------------------------------------

  defp next_token_phrase(tok, si), do: lookahead_phrases(tok, si) |> elem(0)
  defp next2_token_phrase(tok, si), do: lookahead_phrases(tok, si) |> elem(1)

  # Returns {nx1, nx2} of the next two non-chargram, non-punct phrases ("" if missing)
  defp lookahead_phrases(tok, si) do
    tokens = Map.get(si, :tokens, [])

    idx =
      case Map.get(tok, :index) do
        i when is_integer(i) ->
          i

        _ ->
          Enum.find_index(tokens, fn t ->
            t === tok or
              (Map.get(t, :id) == Map.get(tok, :id) and
                 (tok_span(t) == tok_span(tok) or
                    (Map.get(t, :phrase) || "") == (Map.get(tok, :phrase) || "")))
          end) || -1
      end

    words =
      tokens
      |> Enum.drop(idx + 1)
      |> Enum.filter(fn t ->
        kind = Map.get(t, :kind) || Map.get(t, "kind")
        src = Map.get(t, :source)
        not (kind in [:chargram, "chargram", :punct, "punct"] or src in [:chargram, "chargram"])
      end)
      |> Enum.flat_map(fn t ->
        v =
          Map.get(t, :phrase) || Map.get(t, "phrase") ||
            Map.get(t, :norm) || Map.get(t, "norm") ||
            Map.get(t, :text) || Map.get(t, "text")

        cond do
          is_nil(v) -> []
          is_binary(v) -> [v]
          true -> [to_string(v)]
        end
      end)

    case words do
      [a, b | _] -> {a, b}
      [a] -> {a, ""}
      _ -> {"", ""}
    end
  end

  defp normalize_apostrophe(s) when is_binary(s), do: String.replace(s, "’", "'")
  defp normalize_apostrophe(s), do: to_string(s)

  # ── POS / forms helpers ────────────────────────────────────────────

  defp pos_pronoun?(c) when is_map(c) do
    p =
      Map.get(c, :pos) ||
        get_in(c, [:features, :pos]) ||
        get_in(c, ["features", "pos"])

    cond do
      is_nil(p) -> false
      is_atom(p) -> p |> Atom.to_string() |> String.downcase() |> String.contains?("pron")
      is_binary(p) -> p |> String.downcase() |> String.contains?("pron")
      true -> false
    end
  end

  defp pos_pronoun?(_), do: false

  defp id_pronounish?(c) when is_map(c) do
    id = Map.get(c, :id) |> to_string()
    String.contains?(String.downcase(id), "pron")
  end

  defp id_pronounish?(_), do: false

  # Prefer MWE senses for MWE tokens (and vice versa), with safe fallback
  defp compat_filter_for_token(list, %{n: n} = tok)
       when is_list(list) and is_map(tok) and is_integer(n) do
    mwe? = n > 1 or (Map.get(tok, :mw) || Map.get(tok, "mw") || false)
    have_lemma? = Enum.any?(list, &has_readable_lemma?/1)

    kept =
      Enum.filter(list, fn c when is_map(c) ->
        case sense_lemma(c) do
          nil ->
            true

          lemma ->
            has_space = String.contains?(lemma, " ")
            if mwe?, do: has_space, else: not has_space
        end
      end)

    if have_lemma? and kept == [], do: list, else: kept
  end

  defp compat_filter_for_token(list, _tok), do: list

  defp has_readable_lemma?(c), do: not is_nil(sense_lemma(c))

  defp sense_lemma(c) when is_map(c) do
    (Map.get(c, :lemma) ||
       get_in(c, [:features, :lemma]) ||
       get_in(c, ["features", "lemma"]) ||
       Map.get(c, :word))
    |> case do
      nil -> nil
      "" -> nil
      v -> to_string(v)
    end
  end

  defp sense_lemma(_), do: nil

  # Build score map per requested mode (probabilities)
  defp build_scores_map(scored, :all) when is_list(scored),
    do: Map.new(scored, fn %{id: i, score: p} -> {i, p} end)

  defp build_scores_map(scored, :top2) when is_list(scored) do
    case scored do
      [] -> %{}
      [a] -> %{a.id => a.score}
      [a, b | _] -> %{a.id => a.score, b.id => b.score}
    end
  end

  defp build_scores_map(_scored, _), do: %{}

  # ── Feature extraction + nudges ─────────────────────────────────────

  defp extract_features(c, tok, si) do
    base =
      case Map.get(c, :features, %{}) do
        %{} = f when map_size(f) > 0 -> f
        _ -> derive_features(tok, c)
      end

    pos = (base[:pos] || extract_pos(c) || "") |> to_string()

    %{
      lex_fit: as_float(base[:lex_fit] || lexical_hint(c, tok, si)),
      rel_prior:
        as_float(
          base[:rel_prior] || prior_from_id_rank(to_string(c[:id] || "")) || prior_from_pos(pos)
        ),
      activation: as_float(base[:activation] || c[:activation] || 0.0),
      intent_bias: as_float(base[:intent_bias] || salutation_bias(tok, pos) || 0.0),
      pos: pos
    }
  end

  # Derive a reasonably informative feature set when none exist.
  defp derive_features(tok, c) do
    phrase = to_string(Map.get(tok, :phrase, ""))
    id = to_string(Map.get(c, :id, ""))
    lemma = sense_lemma(c) || to_string(Map.get(c, :word, ""))
    pos = extract_pos(c)

    n = Map.get(tok, :n, 1)
    mwe? = n > 1 or (Map.get(tok, :mw) || false)
    space = String.contains?(lemma, " ")

    lex_fit =
      cond do
        String.downcase(lemma) == String.downcase(phrase) -> 1.00
        String.downcase(Map.get(c, :word, "")) == String.downcase(phrase) -> 0.90
        String.downcase(lemma) == String.downcase(single_word(tok)) -> 0.85
        mwe? == space -> 0.70
        true -> 0.50
      end

    %{
      lex_fit: lex_fit,
      rel_prior: prior_from_id_rank(id) || prior_from_pos(pos),
      activation: (Map.get(c, :activation) || 0.0) * 1.0,
      intent_bias: salutation_bias(tok, pos),
      pos: pos,
      lemma: lemma
    }
  end

  defp single_word(tok),
    do: to_string(Map.get(tok, :word) || Map.get(tok, :lemma) || Map.get(tok, :phrase) || "")

  defp prior_from_id_rank(id) do
    # Favor lower sense indices: "...|N" → ~[0.95 .. 0.635] (cap at 9)
    case Regex.run(~r/\|(\d+)$/, id) do
      [_, nstr] ->
        n = String.to_integer(nstr)
        0.95 - min(n, 9) * 0.035

      _ ->
        nil
    end
  end

  defp salutation_bias(tok, pos) do
    phrase = to_string(Map.get(tok, :phrase, ""))

    at_start =
      case tok_span(tok) do
        {0, _} -> true
        _ -> Map.get(tok, :index, 0) == 0
      end

    is_sal = Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) or at_start
    is_interj = String.contains?(String.downcase(to_string(pos)), "interjection")
    if is_sal and is_interj, do: 0.30, else: 0.0
  end

  defp mapify(bin) when is_binary(bin), do: %{id: bin}
  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = m), do: m

  defp extract_pos(c) do
    (Map.get(c, :pos) || get_in(c, [:features, :pos]) || "")
    |> case do
      "" ->
        Map.get(c, :id)
        |> to_string()
        |> String.split("|")
        |> case do
          [_w, p | _] -> String.downcase(p)
          _ -> ""
        end

      p when is_binary(p) ->
        String.downcase(p)

      p when is_atom(p) ->
        p |> Atom.to_string() |> String.downcase()

      _ ->
        ""
    end
  end

  defp prior_from_pos(pos) do
    # super light priors; just to break ties deterministically
    cond do
      pos == "" -> 0.00
      String.contains?(pos, "interjection") -> 0.06
      String.contains?(pos, "noun") -> 0.04
      String.contains?(pos, "verb") -> 0.03
      String.contains?(pos, "pron") -> 0.03
      String.contains?(pos, "adv") -> 0.02
      true -> 0.01
    end
  end

  defp lexical_hint(c, tok, _si) do
    phrase =
      (Map.get(tok, :phrase) || Map.get(tok, "phrase") || "") |> to_string() |> String.downcase()

    lemma =
      (Map.get(c, :lemma) || Map.get(c, :word) || get_in(c, [:features, :lemma]) || "")
      |> to_string()
      |> String.downcase()

    if lemma != "" and phrase != "" and String.starts_with?(phrase, lemma), do: 0.05, else: 0.0
  end

  # Add small context nudges; also add clause-aware "what" bias
  defp context_bias(c, tok, si) do
    phrase = (Map.get(tok, :phrase) || "") |> to_string()
    pos = extract_pos(c)

    base =
      0.0
      |> Kernel.+(
        if Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) and String.contains?(pos, "interjection"),
          do: 0.08,
          else: 0.0
      )
      |> Kernel.+(
        if String.downcase(phrase) == "there" and String.contains?(pos, "noun"),
          do: 0.03,
          else: 0.0
      )

    base + what_clause_bias(c, tok, si)
  end

  defp what_clause_bias(c, tok, si) do
    norm =
      (Map.get(tok, :phrase) || Map.get(tok, "phrase") || "")
      |> to_string()
      |> String.downcase()

    if norm != "what" do
      0.0
    else
      nx = next_token_phrase(tok, si) |> normalize_apostrophe() |> String.downcase()
      nx2 = next2_token_phrase(tok, si) |> normalize_apostrophe() |> String.downcase()
      lang = locale(si)

      pronouny = nx in ~w(im i'm i’m i we you you're we're he she they it)

      verbish =
        nx2 in ~w(try trying mean meant want thinking think doing do say saying going gonna)

      if pronouny or verbish do
        pos = extract_pos(c)
        id = (c[:id] || "") |> to_string() |> String.downcase()

        det_pron_interrog =
          String.contains?(pos, "pron") or
            String.contains?(pos, "det") or
            String.contains?(pos, "interrog")

        particleish =
          String.contains?(pos, "part") or
            String.contains?(id, "part") or
            String.contains?(pos, "intj")

        cond do
          det_pron_interrog -> 0.15
          particleish and lang not in ["en-sg", "sg", "en_sg"] -> -0.20
          what_indefinite?(c) and lang not in ["en-sg", "sg", "en_sg"] -> -0.12
          true -> 0.0
        end
      else
        0.0
      end
    end
  end

  # ── Locale & dialect handling ───────────────────────────────────────

  defp locale(si) do
    (Map.get(si, :locale) || Map.get(si, :lang) || "en")
    |> to_string()
    |> String.downcase()
  end

  defp regional_marker(c) do
    (Map.get(c, :region) ||
       get_in(c, [:features, :region]) ||
       get_in(c, [:features, :dialect]) ||
       Map.get(c, :dialect) ||
       Map.get(c, :definition) || get_in(c, [:features, :definition]) || "")
    |> to_string()
    |> String.downcase()
  end

  # Penalize dialectal/marked senses unless locale matches
  defp dialect_penalty(c, si) do
    reg = regional_marker(c)
    lang = locale(si)

    cond do
      reg == "" -> 0.0
      String.contains?(reg, "singlish") and not String.contains?(lang, "sg") -> -0.08
      true -> 0.0
    end
  end

  # ── Utils ───────────────────────────────────────────────────────────

  defp weights(opts) when is_list(opts) do
    env = Application.get_env(:brain, :lifg_stage1_weights, %{})
    base = %{lex_fit: 0.4, rel_prior: 0.3, activation: 0.2, intent_bias: 0.1}

    base
    |> Map.merge(env || %{})
    |> Map.merge(Map.new(Keyword.get(opts, :weights, [])))
  end

  defp emit(ev, meas, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(ev, meas, meta)
    else
      :ok
    end
  end

  defp as_float(nil), do: 0.0
  defp as_float(n) when is_number(n), do: n * 1.0

  defp as_float(b) when is_binary(b) do
    case Float.parse(b) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  # ── NEW: MWE fallback injector (brain-local, no Core deps) ──────────

  # If a multiword token (n>1 or mw: true) has no compatible MWE senses,
  # synthesize a conservative phrase candidate so Stage-1 can score it.
  defp ensure_mwe_candidates(%{tokens: tokens} = si, opts) when is_list(tokens) do
    enable? =
      Keyword.get(
        opts,
        :mwe_fallback,
        Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
      )

    if not enable?, do: si, else: do_mwe_injection(si)
  end

  defp ensure_mwe_candidates(si, _opts), do: si

  defp do_mwe_injection(%{tokens: tokens} = si) do
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
            score_guess =
              [idx - 1, idx + 1]
              |> Enum.filter(&(&1 >= 0))
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

            emit([:brain, :pmtg, :mwe_fallback_emitted], %{count: 1}, %{
              token_index: idx,
              phrase: phrase,
              score: candidate.score
            })

            {updated, n + 1}
        end
      end)

    if emitted > 0, do: Map.put(si, :sense_candidates, sc), else: si
  end

  defp has_compatible_mwe?(sc, idx) do
    sc
    |> Map.get(idx, [])
    |> Enum.any?(fn c ->
      norm = c[:norm] || c["norm"] || c[:lemma] || c["lemma"] || ""
      String.contains?(norm, " ")
    end)
  end
end

