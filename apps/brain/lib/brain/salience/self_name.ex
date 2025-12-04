# apps/brain/lib/brain/salience/self_name.ex
defmodule Brain.Salience.SelfName do
  @moduledoc """
  Detect mentions of Symbrella's "self name" in incoming text.

  Purpose:
    - model the human "name-called" orienting reflex as a salience trigger
    - provide a reusable, pure-ish function usable from pipelines/regions

  Input:
    - a sentence (binary), OR
    - tokens as a list of maps/structs with any of:
        :phrase | :lemma | :word | :text and optionally :index

  Config:
    config :brain, :self_name_terms, ["symbrella", "bradley"]
  """

  @event [:brain, :salience, :self_name]

  @type match :: %{
          term: String.t(),
          at: non_neg_integer() | nil,
          surface: String.t()
        }

  @type result :: %{
          hit?: boolean(),
          score: float(),
          matches: [match()],
          reason: :hit | :miss | :no_terms,
          terms: [String.t()]
        }

  @spec detect(binary() | list(), keyword()) :: result()
  def detect(input, opts \\ []) do
    terms =
      opts[:terms] ||
        Application.get_env(:brain, :self_name_terms, [])

    terms_norm =
      terms
      |> List.wrap()
      |> Enum.map(&norm_term/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    if terms_norm == [] do
      %{hit?: false, score: 0.0, matches: [], reason: :no_terms, terms: []}
    else
      parts = extract_parts(input)
      {matches, score} = find_matches(parts, terms_norm)
      hit? = matches != []

      if hit? and Keyword.get(opts, :telemetry, true) do
        :telemetry.execute(@event, %{score: score, count: length(matches)}, %{terms: terms_norm})
      end

      %{
        hit?: hit?,
        score: score,
        matches: matches,
        reason: if(hit?, do: :hit, else: :miss),
        terms: terms_norm
      }
    end
  end

  # ───────────────────────── internals ─────────────────────────

  # parts are normalized words with an optional index
  #   [%{w: "good", i: 0, surface: "Good"}, ...]
  defp extract_parts(sentence) when is_binary(sentence) do
    sentence
    |> String.split(~r/\s+/, trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {raw, i} ->
      %{w: norm_word(raw), i: i, surface: raw}
    end)
    |> Enum.reject(&(&1.w == ""))
  end

  defp extract_parts(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.map(fn {t, fallback_i} ->
      surface =
        cond do
          is_map(t) -> t[:phrase] || t[:lemma] || t[:word] || t[:text] || ""
          true -> ""
        end

      i =
        cond do
          is_map(t) and is_integer(t[:index]) -> t[:index]
          true -> fallback_i
        end

      %{w: norm_word(to_string(surface)), i: i, surface: to_string(surface)}
    end)
    |> Enum.reject(&(&1.w == ""))
  end

  defp extract_parts(_), do: []

  defp find_matches(parts, terms_norm) do
    words = Enum.map(parts, & &1.w)

    {matches, best_score} =
      terms_norm
      |> Enum.reduce({[], 0.0}, fn term, {acc, best} ->
        t_words = String.split(term, " ", trim: true)

        found =
          case t_words do
            [one] -> find_unigram(parts, one)
            _ -> find_ngram(parts, t_words)
          end

        if found == [] do
          {acc, best}
        else
          # scoring: multiword is stronger; repeats add small boost
          base = if length(t_words) > 1, do: 0.95, else: 0.85
          bump = min(0.15, 0.05 * max(length(found) - 1, 0))
          score = clamp01(base + bump)

          {acc ++ Enum.map(found, fn m -> Map.put(m, :term, term) end), max(best, score)}
        end
      end)

    # guard against substring-y false positives:
    # we only match whole normalized words or exact ngram sequences above.
    _ = words
    {matches, best_score}
  end

  defp find_unigram(parts, term_word) do
    parts
    |> Enum.filter(&(&1.w == term_word))
    |> Enum.map(fn p -> %{term: term_word, at: p.i, surface: p.surface} end)
  end

  defp find_ngram(parts, t_words) do
    n = length(t_words)
    max_i = length(parts) - n

    if max_i < 0 do
      []
    else
      0..max_i
      |> Enum.flat_map(fn i ->
        slice = Enum.slice(parts, i, n)

        if Enum.map(slice, & &1.w) == t_words do
          [
            %{
              term: Enum.join(t_words, " "),
              at: hd(slice).i,
              surface: Enum.map_join(slice, " ", & &1.surface)
            }
          ]
        else
          []
        end
      end)
    end
  end

  defp norm_term(x) do
    x
    |> to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
    # keep letters/numbers/space/'/-
    |> String.replace(~r/[^\p{L}\p{N}\s'-]+/u, "")
    |> String.trim()
  end

  defp norm_word(x) do
    x
    |> to_string()
    |> String.downcase()
    |> String.replace(~r/^[^\p{L}\p{N}]+|[^\p{L}\p{N}]+$/u, "")
    |> String.replace(~r/[^\p{L}\p{N}'-]+/u, "")
    |> String.trim()
  end

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0
end
