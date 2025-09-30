defmodule Core do
  @moduledoc "Tokenize -> rebuild word n-grams -> STM -> LTM(owns lexicon) -> LIFG Stage-1 -> notify Brain."

  alias Brain
  alias Core.Token
  alias Core.Lexicon
  alias Core.SemanticInput
  alias Db

  @type si :: map()
  @type opts :: keyword()

  @spec resolve_input(String.t(), opts()) :: SemanticInput.t()
  def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
    mode = Keyword.get(opts, :mode, :test)
    max_n = Keyword.get(opts, :max_wordgram_n, 3)

    si0 =
      phrase
      |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n)
      # <<< ensure struct here
      |> wrap_si(phrase)
      |> rebuild_word_ngrams(max_n)
      |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
      |> Map.put_new(:trace, [])

    case mode do
      :prod ->
        si0
        |> Brain.stm()
        |> keep_only_word_boundary_tokens()
        # LTM may enrich lexicon; returns SI
        |> Db.ltm(opts)
        |> keep_only_word_boundary_tokens()
        # merges :active_cells into SI
        |> Lexicon.all()
        |> keep_only_word_boundary_tokens()
        |> run_lifg_and_attach(opts)
        |> notify_brain_activation(opts)

      _ ->
        si0
    end
  end

  # ─────────────────────── Brain notify ───────────────────────

  defp notify_brain_activation(si, opts) do
    payload = %{
      delta: Keyword.get(opts, :delta, 0.1),
      decay: Keyword.get(opts, :decay, 0.98),
      via: :core
    }

    rows = Map.get(si, :active_cells, []) || []
    lifg_count = si |> Map.get(:lifg_choices, []) |> length()

    if rows != [] and Process.whereis(Brain) do
      Brain.activate_cells(rows, payload)
    end

    Map.update(si, :trace, [], fn tr ->
      [{:activated, %{rows: length(rows), lifg_choices: lifg_count, shape: :activate_cells}} | tr]
    end)
  end

  # ─────────────────────── LIFG stage-1 ───────────────────────

  defp run_lifg_and_attach(si, lifg_opts \\ []) do
    groups =
      si.tokens
      |> Enum.reduce(%{}, fn t, acc ->
        nrm = norm(Map.get(t, :phrase))

        senses =
          si.active_cells
          |> Enum.filter(fn s -> (Map.get(s, :norm) || Map.get(s, "norm")) == nrm end)

        if senses == [], do: acc, else: Map.put(acc, Map.get(t, :index), senses)
      end)

    ctx = [1.0]

    case Brain.lifg_stage1(
           %{candidates_by_token: groups, tokens: si.tokens, sentence: si.sentence},
           ctx,
           lifg_opts
         ) do
      {:ok, out} ->
        ev =
          out.audit
          |> Map.merge(%{
            stage: :lifg_stage1,
            choices: out.choices,
            boosts: out.boosts,
            inhibitions: out.inhibitions
          })

        lifg_choices =
          Enum.map(out.choices, fn ch ->
            chosen_id = Map.get(ch, :chosen_id)
            scores = Map.get(ch, :scores) || %{}
            feats = Map.get(ch, :features) || %{}

            score =
              if is_binary(chosen_id) and is_map(scores),
                do: Map.get(scores, chosen_id, 0.0),
                else: Map.get(feats, :score_norm, 0.0)

            %{
              token_index: Map.get(ch, :token_index),
              lemma: (Map.get(ch, :lemma) || "") |> String.downcase(),
              id: chosen_id,
              alt_ids: Map.get(ch, :alt_ids, []),
              score: score
            }
          end)

        si
        |> Map.put(:lifg_choices, lifg_choices)
        |> Map.update(:trace, [], &[ev | &1])

      {:error, _} ->
        si
    end
  end

  # ─────────────────────── Tokens / n-grams ───────────────────────

  # Build contiguous word n-grams with character spans against a normalized sentence.
  defp rebuild_word_ngrams(%{sentence: s} = si, max_n)
       when is_binary(s) and is_integer(max_n) and max_n > 0 do
    s_norm = s |> String.trim() |> String.replace(~r/\s+/u, " ")
    words = if s_norm == "", do: [], else: String.split(s_norm, " ")

    # char start offsets for each word
    starts =
      words
      |> Enum.reduce({[], 0}, fn w, {acc, pos} -> {[pos | acc], pos + String.length(w) + 1} end)
      |> elem(0)
      |> Enum.reverse()

    wlen = length(words)

    tokens =
      for i <- 0..(wlen - 1), n <- min(max_n, wlen - i)..1//-1 do
        phrase = words |> Enum.slice(i, n) |> Enum.join(" ")
        start = Enum.at(starts, i)
        len = String.length(phrase)

        %{index: nil, span: {start, len}, n: n, phrase: phrase, mw: n > 1, instances: []}
      end
      |> Enum.with_index()
      |> Enum.map(fn {t, idx} -> Map.put(t, :index, idx) end)

    si |> Map.put(:sentence, s_norm) |> Map.put(:tokens, tokens)
  end

  defp rebuild_word_ngrams(si, _), do: si

  # Keep tokens where sentence slice matches token.phrase (normalized)
  # Supports char-span {start,len} and word-window {i,j} (j exclusive)
  defp keep_only_word_boundary_tokens(%{sentence: s, tokens: toks} = si)
       when is_binary(s) and is_list(toks) do
    s_norm = s |> String.trim() |> String.replace(~r/\s+/u, " ")
    words = if s_norm == "", do: [], else: String.split(s_norm, " ")
    wcount = length(words)

    kept =
      Enum.filter(toks, fn t ->
        phrase = Map.get(t, :phrase) |> norm()

        case Map.get(t, :span) do
          {a, b} when is_integer(a) and is_integer(b) and a >= 0 ->
            # First, treat as char-span {start,len}
            char_match =
              b > 0 and
                norm(String.slice(s_norm, a, b) || "") == phrase

            if char_match do
              true
            else
              # Fallback: interpret as word-window {i,j} with j exclusive
              j = b
              window_ok = a < j and a < wcount and j <= wcount

              if window_ok do
                joined = words |> Enum.slice(a, j - a) |> Enum.join(" ") |> norm()
                joined == phrase
              else
                false
              end
            end

          _ ->
            false
        end
      end)

    %{si | tokens: kept}
  end

  defp keep_only_word_boundary_tokens(si), do: si

  # ───────────────────────── Helpers ─────────────────────────

  # Ensure we have a %SemanticInput{} struct after tokenize.
  defp wrap_si(%SemanticInput{} = si, _orig_sentence), do: si

  defp wrap_si(tokens, sentence) when is_list(tokens),
    do: %SemanticInput{sentence: sentence, tokens: tokens, source: :test, trace: []}

  defp wrap_si(other, sentence) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp wrap_si(_other, sentence),
    do: %SemanticInput{sentence: sentence, tokens: [], source: :test, trace: []}

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()
end
