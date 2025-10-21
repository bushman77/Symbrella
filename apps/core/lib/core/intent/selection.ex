defmodule Core.Intent.Selection do
  @moduledoc """
  Lightweight intent selector.

  - Returns the incoming SemanticInput (map) with `:intent`, `:keyword`, `:confidence`.
  - Appends `{:intent, %{keyword, intent, confidence}}` to `si.trace`.
  - Emits telemetry so BrainLive (and others) can display it:

      * [:core,  :intent, :selected]
      * [:brain, :intent, :selected]

    measurements: %{confidence: float()}
    metadata:     %{intent: atom(), keyword: binary(), text: binary(), source: atom()}
  """

  @type si :: map()
  @type intent :: :greet | :translate | :abuse | :insult | :ask | :unknown

  @spec select(si(), Keyword.t()) :: si()
  def select(si, opts \\ [])

  # Primary clause: we expect a sentence-bearing SI
  def select(%{sentence: _} = si, _opts) do
    kw = extract_keyword(si)
    {intent, conf} = infer_intent(kw, si)
    text = text_from_si(si, kw)

    si2 =
      si
      |> Map.put(:intent, intent)
      |> Map.put(:keyword, kw)
      |> Map.put(:confidence, conf)
      |> Map.update(:trace, [], &[{:intent, %{keyword: kw, intent: intent, confidence: conf}} | &1])

    emit(intent, kw, conf, text)
    si2
  end

  # Fallback: leave SI unchanged if we don't recognize the shape
  def select(si, _opts), do: si

  # ───────────────────────────── private ─────────────────────────────

  # Emit both events and update Brain's latest intent (best-effort, never crash)
  defp emit(intent, kw, conf, text)
       when is_atom(intent) and is_binary(kw) and is_number(conf) and is_binary(text) do
    meas = %{confidence: conf}
    meta = %{intent: intent, keyword: kw, text: text, source: :core}

    # 1) Update Brain snapshot (ignore failures to avoid coupling)
    _ =
      try do
        Brain.set_latest_intent(%{intent: intent, keyword: kw, confidence: conf})
      catch
        _, _ -> :ok
      end

    # 2) Telemetry (best-effort)
    if function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute([:core, :intent, :selected],  meas, meta)
      :telemetry.execute([:brain, :intent, :selected], meas, meta)
    end

    :ok
  end

  defp emit(_, _, _, _), do: :ok

# Pull a clean, human-ish keyword to display (no duplicates)
defp extract_keyword(%{keyword: kw}) when is_binary(kw) and kw != "" do
  kw |> String.trim() |> squish() |> String.downcase()
end

defp extract_keyword(%{tokens: tokens}) when is_list(tokens) do
  tokens
  |> Enum.map(fn t -> t[:phrase] || t["phrase"] || "" end)
  |> Enum.map(&(&1 |> String.trim()))
  |> Enum.reject(&(&1 == ""))
  |> Enum.map(&squish/1)
  |> Enum.uniq()
  |> prefer_multiword_keyword()
end

defp extract_keyword(%{sentence: s}) when is_binary(s) do
  s |> String.trim() |> squish() |> String.downcase()
end

defp extract_keyword(_), do: ""

# --- helpers used above ---

defp squish(s) when is_binary(s) do
  s
  |> String.replace(~r/\s+/u, " ")
  |> String.trim()
end

defp prefer_multiword_keyword([]), do: ""

defp prefer_multiword_keyword(phrases) do
  # score: more words first, then longer text
  phrases
  |> Enum.sort_by(fn p -> {word_count(p), String.length(p)} end, :desc)
  |> List.first()
  |> String.downcase()
end

defp word_count(p) when is_binary(p), do: length(String.split(p, ~r/\s+/, trim: true))

  # Prefer the full sentence for telemetry; fall back to kw if missing
  defp text_from_si(%{sentence: s}, _kw) when is_binary(s) and s != "", do: s
  defp text_from_si(%{text: s}, _kw) when is_binary(s) and s != "", do: s
  defp text_from_si(%{tokens: tokens}, kw) when is_list(tokens) do
    joined =
      tokens
      |> Enum.map(fn t -> t[:phrase] || t["phrase"] || "" end)
      |> Enum.reject(&(&1 == ""))
      |> Enum.join(" ")
      |> String.trim()

    if joined == "", do: kw || "", else: joined
  end
  defp text_from_si(_si, kw), do: kw || ""

  # ───────────────────────────── heuristics ─────────────────────────────

  # Precedence: greet -> translate -> abuse/insult -> ask -> unknown
  defp infer_intent(kw, _si) when kw in ["", nil], do: {:unknown, 0.0}

  defp infer_intent(kw, _si) do
    cond do
      greeting?(kw) ->
        {:greet, 0.70}

      translate_like?(kw) ->
        {:translate, 0.65}

      matches_abuse?(kw) ->
        {:abuse, 0.92}

      matches_insult?(kw) ->
        {:insult, 0.80}

      looks_like_question?(kw) ->
        {:ask, 0.70}

      true ->
        {:unknown, 0.45}
    end
  end

  # --- greet / translate ---

  defp greeting?(s) do
    Regex.match?(~r/^(hi|hello|hey|yo|gm|good\s+(morning|afternoon|evening))\b/i, s || "")
  end

  defp translate_like?(s) do
    Regex.match?(~r/\b(translate|what(?:'s| is)\s+.+?\s+in)\b/i, s || "")
  end

  # --- abuse / insult detection ---

  # Return regexes at runtime (not in a module attribute) to avoid injecting NIF refs
  defp abuse_phrase_regexes do
    [
      ~r/\b(fuck\s+you|f\W*\s*u)\b/i,
      ~r/\b(fuck\s*off)\b/i,
      ~r/\b(screw\s+you)\b/i,
      ~r/\b(piss\s*off)\b/i,
      ~r/\b(go\s+to\s+hell)\b/i
    ]
  end

  # Word lists as functions (avoid @injection)
  defp abuse_words,  do: ~w(asshole bitch bastard dickhead motherfucker shithead cocksucker retard retarded)
  defp insult_words, do: ~w(idiot stupid dumb moron loser pathetic jerk clown trash garbage worthless useless brainless)

  defp matches_abuse?(s) when is_binary(s) do
    down = String.downcase(s)

    phrase_hit? =
      Enum.any?(abuse_phrase_regexes(), fn rx -> Regex.match?(rx, down) end)

    word_hit? =
      Regex.match?(compiled_word_regex(abuse_words()), down) ||
        (case env_abuse_regex() do
           nil -> false
           rx  -> Regex.match?(rx, down)
         end)

    phrase_hit? or word_hit?
  end
  defp matches_abuse?(_), do: false

  defp matches_insult?(s) when is_binary(s) do
    down = String.downcase(s)

    # "you are/you're <insult>"
    pattern_hit? =
      Regex.match?(
        ~r/\b(you\s+are|you're|ur)\s+(a\s+)?(#{words_alt(insult_words())})\b/i,
        down
      )

    word_hit? =
      Regex.match?(compiled_word_regex(insult_words()), down) ||
        (case env_insult_regex() do
           nil -> false
           rx  -> Regex.match?(rx, down)
         end)

    pattern_hit? or word_hit?
  end
  defp matches_insult?(_), do: false

  # --- ask ---

  defp looks_like_question?(s) when is_binary(s) do
    String.contains?(s, "?") or
      Regex.match?(
        ~r/^\s*(who|what|when|where|why|how|do|does|did|can|could|will|would|should|is|are|am|have|has|had|may|might|was|were)\b/i,
        s
      )
  end
  defp looks_like_question?(_), do: false

  # --- helpers for word lists / config ---

  defp compiled_word_regex(words) when is_list(words) and words != [] do
    # \b(word1|word2|...)\b (case-insensitive)
    Regex.compile!("\\b(" <> Enum.map_join(words, "|", &Regex.escape/1) <> ")\\b", "i")
  end
  defp compiled_word_regex(_), do: ~r/(?!)/ # never matches

  defp words_alt(words), do: Enum.map_join(words, "|", &Regex.escape/1)

  # Allow env to extend/override word lists without code changes:
  # config :core, Core.Intent.Selection, abuse_terms: [...], insult_terms: [...]
  defp env_abuse_regex() do
    terms = Application.get_env(:core, __MODULE__, [])[:abuse_terms] || []
    if terms == [] do
      nil
    else
      compiled_word_regex(terms)
    end
  end

  defp env_insult_regex() do
    terms = Application.get_env(:core, __MODULE__, [])[:insult_terms] || []
    if terms == [] do
      nil
    else
      compiled_word_regex(terms)
    end
  end
end

