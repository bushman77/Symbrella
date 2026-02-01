defmodule Brain.SelfName do
  @moduledoc """
  SelfName — lightweight "own-name" detector.

  This models the brain's "cocktail party effect" at a tiny scale:
  when your name appears in the token stream, we emit a salience-like signal.

  Output is a small map you can stash in `state.attention[:self_name]` and/or
  into a pipeline audit trail.
  """

  @event [:brain, :self_name, :detected]

  @type match_t :: %{
          name: binary(),
          span: {non_neg_integer(), non_neg_integer()},
          token_indexes: [non_neg_integer()]
        }

  @type result_t :: %{
          hit?: boolean(),
          score: float(),
          matches: [match_t()],
          token_indexes: [non_neg_integer()],
          reason: atom()
        }

  @doc """
  Detect whether a configured "self name" appears in `tokens`.

  Options:
    * `:names` - list of names/phrases to match (case-insensitive). If omitted,
      uses `Application.get_env(:brain, :self_names, [])`.

  `tokens` may be a list of maps (e.g. `%{index: 1, phrase: "Bradley"}`) or strings.
  """
  @spec detect(list(), binary() | nil, keyword()) :: result_t()
  def detect(tokens, _sentence \\ nil, opts \\ []) when is_list(tokens) and is_list(opts) do
    names =
      opts
      |> Keyword.get(:names, Application.get_env(:brain, :self_names, []))
      |> normalize_names()

    tok_stream = normalize_tokens(tokens)

    matches =
      names
      |> Enum.flat_map(fn %{name: name, words: want} ->
        find_phrase_matches(tok_stream, want, name)
      end)

    token_indexes =
      matches
      |> Enum.flat_map(& &1.token_indexes)
      |> Enum.uniq()
      |> Enum.sort()

    hit? = matches != []
    score = if hit?, do: 1.0, else: 0.0

    res = %{
      hit?: hit?,
      score: score,
      matches: matches,
      token_indexes: token_indexes,
      reason: if(hit?, do: :self_name_match, else: :no_match)
    }

    :telemetry.execute(
      @event,
      %{score: score},
      %{hit?: hit?, matches: length(matches), names: length(names)}
    )

    res
  end

  # ───────────────────────── Internals ─────────────────────────

  defp normalize_names(names) when is_list(names) do
    names
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn n ->
      words =
        n
        |> String.downcase()
        |> String.split(~r/\s+/u, trim: true)
        |> Enum.map(&clean_word/1)
        |> Enum.reject(&(&1 == ""))

      %{name: n, words: words}
    end)
    |> Enum.reject(fn %{words: ws} -> ws == [] end)
  end

  defp normalize_tokens(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.flat_map(fn {tok, pos} ->
      {idx, raw} =
        cond do
          is_map(tok) ->
            idx =
              tok[:index] || tok["index"] || tok[:token_index] || tok["token_index"] || pos

            raw =
              tok[:lemma] || tok["lemma"] ||
                tok[:phrase] || tok["phrase"] ||
                tok[:text] || tok["text"] ||
                tok[:word] || tok["word"] ||
                ""

            {idx, raw}

          is_binary(tok) ->
            {pos, tok}

          true ->
            {pos, ""}
        end

      w = raw |> to_string() |> clean_word()

      if w == "" do
        []
      else
        [%{idx: idx, word: w}]
      end
    end)
  end

  defp find_phrase_matches(tok_stream, want_words, name) when is_list(tok_stream) do
    wlen = length(want_words)
    tlen = length(tok_stream)

    if wlen == 0 or tlen < wlen do
      []
    else
      0..(tlen - wlen)
      |> Enum.reduce([], fn start, acc ->
        slice = Enum.slice(tok_stream, start, wlen)

        if Enum.map(slice, & &1.word) == want_words do
          idxs = Enum.map(slice, & &1.idx)

          match = %{
            name: name,
            span: {start, start + wlen},
            token_indexes: idxs
          }

          [match | acc]
        else
          acc
        end
      end)
      |> Enum.reverse()
    end
  end

  defp clean_word(w) when is_binary(w) do
    w
    |> String.downcase()
    # strip leading/trailing non-letter/number (keeps internal hyphens/apostrophes)
    |> String.replace(~r/^[^\p{L}\p{N}]+|[^\p{L}\p{N}]+$/u, "")
  end
end
