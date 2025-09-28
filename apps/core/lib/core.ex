defmodule Core do
  @moduledoc "Tokenize -> rebuild word n-grams -> STM -> LTM(owns lexicon) -> LIFG Stage-1 -> notify Brain."

  alias Brain
  alias Core.Token
  alias Core.Lexicon
  alias Db

  @type si :: map()
  @type opts :: keyword()

  @spec resolve_input(String.t(), opts()) :: si()
  def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
    mode  = Keyword.get(opts, :mode, :test)
    max_n = Keyword.get(opts, :max_wordgram_n, 3)

    si0 =
      phrase
      |> Token.tokenize(max_wordgram_n: max_n)
      |> rebuild_word_ngrams(max_n)                # char spans {start,len} over normalized sentence
      |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
      |> Map.put_new(:trace, [])

    case mode do
      :prod ->
        si0
        |> Brain.stm()                              # <— was Brain.STM.run/2
        |> keep_only_word_boundary_tokens()
        |> Db.ltm(opts)                             # LTM also triggers Lexicon enrichment if missing
        |> keep_only_word_boundary_tokens()
        |> Lexicon.all()                            # ensure lexicon rows exist; merges into :active_cells
        |> keep_only_word_boundary_tokens()
        |> run_lifg_and_attach(opts)                # attach lifg_choices + audit event
        |> notify_brain_activation(opts)            # bump cells & add {:activated, ...} to trace

      _ ->
        si0
    end
  end

  # ─────────────────────── Brain notify ───────────────────────

  defp notify_brain_activation(si, opts) do
    payload = %{delta: Keyword.get(opts, :delta, 0.1), decay: Keyword.get(opts, :decay, 0.98), via: :core}
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

    case Brain.lifg_stage1(%{candidates_by_token: groups, tokens: si.tokens, sentence: si.sentence}, ctx, lifg_opts) do
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
            scores    = Map.get(ch, :scores)   || %{}
            feats     = Map.get(ch, :features) || %{}

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
    words  = if s_norm == "", do: [], else: String.split(s_norm, " ")

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
        start  = Enum.at(starts, i)
        len    = String.length(phrase)

        %{index: nil, span: {start, len}, n: n, phrase: phrase, mw: n > 1, instances: []}
      end
      |> Enum.with_index()
      |> Enum.map(fn {t, idx} -> Map.put(t, :index, idx) end)

    si |> Map.put(:sentence, s_norm) |> Map.put(:tokens, tokens)
  end

  defp rebuild_word_ngrams(si, _), do: si

  # Keep tokens where sentence slice matches token.phrase (normalized)
  defp keep_only_word_boundary_tokens(%{sentence: s, tokens: toks} = si)
       when is_binary(s) and is_list(toks) do
    kept =
      Enum.filter(toks, fn t ->
        case Map.get(t, :span) do
          {start, len} when is_integer(start) and is_integer(len) and start >= 0 and len > 0 ->
            from_span = String.slice(s, start, len) || ""
            norm(from_span) == norm(Map.get(t, :phrase))
          _ -> false
        end
      end)

    %{si | tokens: kept}
  end

  defp keep_only_word_boundary_tokens(si), do: si

  # ───────────────────────── Helpers ─────────────────────────

  defp norm(nil), do: ""
  defp norm(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
  defp norm(v),
    do: v |> Kernel.to_string() |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
end

