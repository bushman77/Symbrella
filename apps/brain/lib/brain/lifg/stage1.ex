defmodule Brain.LIFG.Stage1 do
  @moduledoc """
  Stage 1 — fast per-token disambiguation from `si.sense_candidates`.

  Options:
    :weights          — %{lex_fit, rel_prior, activation, intent_bias}
    :scores           — :all | :top2 | :none (default from `:lifg_stage1_scores_mode`)
    :chargram_event   — telemetry (default [:brain, :lifg, :chargram_violation])
    :boundary_event   — telemetry (default [:brain, :lifg, :boundary_drop])
    :margin_threshold — default 0.15 (for alt_id emission)
  """

  require Logger

  @type si :: map()
  @type choice :: %{
          required(:token_index) => non_neg_integer(),
          required(:chosen_id)   => String.t(),
          required(:alt_ids)     => [String.t()],
          required(:margin)      => float(),
          optional(:scores)      => map()
        }

  @spec run(si(), keyword()) :: {:ok, %{si: si(), choices: [choice()], audit: map()}} | {:error, term()}
  def run(si, opts \\ []) when is_map(si) and is_list(opts) do
    try do
      t0 = System.monotonic_time()

      tokens = Map.get(si, :tokens, [])
      slate =
        case Map.get(si, :sense_candidates, Map.get(si, :candidates_by_token, %{})) do
          %{} = s -> s
          _ -> %{}
        end

      {kept_tokens, dropped} = guard_tokens(tokens, si, opts)
      scores_mode = Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))
      margin_threshold = Keyword.get(opts, :margin_threshold, 0.15)
      w = weights(opts)

      choices =
        kept_tokens
        |> Enum.map(fn tok ->
          disambiguate_token(
            tok,
            Map.get(slate, tok.index, []),
            scores: scores_mode,
            margin_threshold: margin_threshold,
            weights: w
          )
        end)
        |> Enum.reject(&is_nil/1)

      timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

      audit = %{
        stage: :lifg_stage1,
        token_count: length(tokens),
        kept_tokens: length(kept_tokens),
        dropped_tokens: length(dropped),
        timing_ms: timing_ms
      }

      {:ok, %{si: si, choices: choices, audit: audit}}
    rescue
      e ->
        Logger.error("LIFG Stage1 run failed: #{inspect(e)}")
        {:error, e}
    end
  end

  # Back-compat shim (si, ctx, opts)
  @spec run(si(), map(), keyword()) :: {:ok, %{si: si(), choices: [choice()], audit: map()}} | {:error, term()}
  def run(si, _ctx, opts), do: run(si, opts)

  # ── Guards ─────────────────────────────────────────────────────────

  defp guard_tokens(tokens, si, opts) when is_list(tokens) and is_map(si) do
    char_ev = Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation])
    bnd_ev  = Keyword.get(opts, :boundary_event,  [:brain, :lifg, :boundary_drop])

    Enum.reduce(tokens, {[], []}, fn tok, {kept, dropped} ->
      cond do
        is_chargram?(tok) ->
          emit(char_ev, %{token_index: tok[:index], phrase: tok[:phrase]}, %{})
          {kept, [tok | dropped]}

        not boundary_ok?(tok, si) ->
          emit(bnd_ev, %{token_index: tok[:index], phrase: tok[:phrase]}, %{})
          {kept, [tok | dropped]}

        true ->
          {[tok | kept], dropped}
      end
    end)
    |> (fn {k, d} -> {Enum.reverse(k), Enum.reverse(d)} end).()
  end
  defp guard_tokens(_tokens, _si, _opts), do: {[], []}

  defp is_chargram?(tok) when is_map(tok) do
    (Map.get(tok, :source) in [:chargram, "chargram"]) or
      (Map.get(tok, :kind)   in [:chargram, "chargram"]) or
      (Map.get(tok, :chargram) in [true, "true"])
  end
  defp is_chargram?(_), do: false

  defp boundary_ok?(tok, si) when is_map(tok) and is_map(si) do
    # MWEs bypass strictness
    mw? = Map.get(tok, :mw) || Map.get(tok, "mw") || false
    if mw?, do: true, else: do_boundary_check(tok, si)
  end
  defp boundary_ok?(_tok, _si), do: false

  defp do_boundary_check(tok, si) when is_map(tok) and is_map(si) do
    sent = Map.get(si, :sentence)
    span = Map.get(tok, :span) || Map.get(tok, "span")

    cond do
      not is_binary(sent) -> true
      not (is_tuple(span) and tuple_size(span) == 2) -> true
      true ->
        {start, len} = span
        start = as_int(start, 0)
        len   = as_int(len, 0)
        stop  = start + len
        s_len = byte_size(sent)

        left_ok?  = start <= 0 or not letter?(String.at(sent, start - 1))
        right_ok? = stop >= s_len or not letter?(String.at(sent, stop))

        left_ok? and right_ok?
    end
  end
  defp do_boundary_check(_tok, _si), do: true

  defp letter?(<<c::utf8>>), do: unicode_letter?(c)
  defp unicode_letter?(cp),
    do: (cp >= ?A and cp <= ?Z) or (cp >= ?a and cp <= ?z) or (cp >= ?À and cp <= 0x024F)

  # ── Per-token disambiguation ────────────────────────────────────────

  defp disambiguate_token(%{} = tok, cand_list, opts) when is_list(cand_list) do
    idx         = tok.index || 0
    thr         = Keyword.get(opts, :margin_threshold, 0.15)
    scores_mode = Keyword.get(opts, :scores, :all)
    w           = Map.new(Keyword.get(opts, :weights, []), fn {k,v} -> {k, v} end)

    cand_list =
      cand_list
      |> Enum.reject(&is_nil/1)
      |> Enum.reject(&(is_nil(&1.id) or not is_binary(&1.id)))
      |> Enum.uniq_by(& &1.id)
      |> prefer_salutation_interjection(tok)
      |> prefer_pronoun_for_I(tok)
      |> compat_filter_for_token(tok)

    score_cand = fn c when is_map(c) ->
      f = Map.get(c, :features, %{})
      %{
        id: c.id,
        score:
          (w[:lex_fit]     || 0.4) * as_float(f[:lex_fit]     || f["lex_fit"]) +
          (w[:rel_prior]   || 0.3) * as_float(f[:rel_prior]   || f["rel_prior"]) +
          (w[:activation]  || 0.2) * as_float(f[:activation]  || f["activation"]) +
          (w[:intent_bias] || 0.1) * as_float(f[:intent_bias] || f["intent_bias"])
      }
    end

    scored = Enum.map(cand_list, score_cand)

    if scored == [] do
      nil
    else
      sorted_scored = Enum.sort_by(scored, &(-&1.score))

      [%{id: top_id, score: top_s} | rest] = sorted_scored
      rest_unique = rest |> Enum.reject(&(&1.id == top_id)) |> Enum.uniq_by(& &1.id)

      second_s =
        case rest_unique do
          [%{score: s2} | _] -> s2
          _ -> 0.0
        end

      alt_ids =
        if max(top_s - second_s, 0.0) < thr and rest_unique != [] do
          [hd(rest_unique).id]
        else
          []
        end

      %{
        token_index: idx,
        chosen_id: top_id,
        alt_ids: alt_ids,
        margin: max(top_s - second_s, 0.0),
        scores: build_scores_map(sorted_scored, scores_mode)
      }
    end
  end
  defp disambiguate_token(_tok, _cand_list, _opts), do: nil

  # Prefer interjection "greeting" senses for salutations (“hello/hi/hey”), esp. at start
  defp prefer_salutation_interjection(list, tok) when is_list(list) and is_map(tok) do
    phrase = (tok[:phrase] || "") |> to_string()
    at_start =
      case tok[:span] do
        {0, _} -> true
        _ -> (tok[:index] || 0) == 0
      end

    sal? = Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) or at_start

    if sal? do
      greet =
        Enum.filter(list, fn c when is_map(c) ->
          pos =
            (c[:pos] || get_in(c, [:features, :pos]) || "")
            |> to_string() |> String.downcase()

          defn =
            (c[:definition] || get_in(c, [:features, :definition]) || "")
            |> to_string() |> String.downcase()

          syns =
            (c[:synonyms] || get_in(c, [:features, :synonyms]) || [])
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

  # Pronoun preference for the token exactly "I"
  defp prefer_pronoun_for_I(list, tok) when is_list(list) and is_map(tok) do
    phrase = (Map.get(tok, :phrase) || Map.get(tok, "phrase") || "") |> to_string()
    if phrase == "I" do
      pronounish = Enum.filter(list, fn c when is_map(c) -> pos_pronoun?(c) or id_pronounish?(c) end)
      if pronounish == [], do: list, else: pronounish
    else
      list
    end
  end
  defp prefer_pronoun_for_I(list, _tok), do: list

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
  defp compat_filter_for_token(list, %{n: n} = tok) when is_list(list) and is_map(tok) and is_integer(n) do
    mwe? = n > 1 or (Map.get(tok, :mw) || Map.get(tok, "mw") || false)
    have_lemma? = Enum.any?(list, &has_readable_lemma?/1)

    kept =
      Enum.filter(list, fn c when is_map(c) ->
        case sense_lemma(c) do
          nil -> true
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
      ""  -> nil
      v   -> to_string(v)
    end
  end
  defp sense_lemma(_), do: nil

  # Build score map per requested mode; default :all for back-compat
  defp build_scores_map(scored, :all) when is_list(scored),
    do: Map.new(scored, fn %{id: i, score: s} -> {i, s} end)

  defp build_scores_map(scored, :top2) when is_list(scored) do
    case scored do
      [] -> %{}
      [a] -> %{a.id => a.score}
      [a, b | _] -> %{a.id => a.score, b.id => b.score}
    end
  end

  defp build_scores_map(_scored, _), do: %{}

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

  defp as_int(nil, d), do: d
  defp as_int(i, _d) when is_integer(i), do: i
  defp as_int(b, d) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} -> n
      _ -> d
    end
  end
  defp as_int(_, d), do: d

  defp as_float(nil), do: 0.0
  defp as_float(n) when is_number(n), do: n * 1.0
  defp as_float(b) when is_binary(b) do
    case Float.parse(b) do
      {f, _} -> f
      _ -> 0.0
    end
  end
  defp as_float(_), do: 0.0
end

