defmodule Brain.WM.Recall do
  @moduledoc """
  Best-effort Hippocampus recall integration and WM candidate normalization.
  """

  @doc """
  Build WM candidates from recall results or fallback cues.
  """
  @spec candidates_for_focus(map() | list(), keyword()) :: [map()]
  def candidates_for_focus(si_or_cues, recall_opts \\ []) do
    sentence = extract_sentence(si_or_cues)
    results = safe_hippo_recall(si_or_cues, recall_opts)

    case results do
      list when is_list(list) and list != [] ->
        Enum.map(list, &recall_result_to_wm_candidate/1)

      _ ->
        si_or_cues
        |> cues_to_candidates(sentence)
        |> Enum.map(&cue_to_wm_candidate/1)
    end
  end

  defp safe_hippo_recall(si_or_cues, recall_opts) do
    if Code.ensure_loaded?(Brain.Hippocampus) and
         function_exported?(Brain.Hippocampus, :recall, 2) do
    else
      []
    end
  end

  defp recall_result_to_wm_candidate(r) when is_map(r) do
    slate = get_in(r, [:episode, :slate]) || get_in(r, ["episode", "slate"])

    lemma =
      Map.get(r, :lemma) ||
        Map.get(r, "lemma") ||
        (slate && first_lemma_from_slate(slate)) ||
        ""

    lemma_s = to_string(lemma)

    id =
      Map.get(r, :id) ||
        Map.get(r, "id") ||
        (lemma_s != "" && "#{lemma_s}|ltm") ||
        "ltm"

    %{
      token_index: as_nonneg_int(Map.get(r, :token_index) || Map.get(r, "token_index") || 0),
      id: to_string(id),
      lemma: lemma_s,
      score: as_float(Map.get(r, :score) || Map.get(r, "score") || 1.0),
      source: :ltm,
      reason: :hippocampus,
      payload: r
    }
  end

  defp recall_result_to_wm_candidate(other) do
    %{
      token_index: 0,
      id: "ltm",
      lemma: "ltm",
      score: 0.0,
      source: :ltm,
      reason: :hippocampus,
      payload: other
    }
  end

  defp cue_to_wm_candidate(c) when is_map(c) do
    lemma = to_string(c[:lemma] || c["lemma"] || "")
    id = c[:id] || c["id"] || (lemma != "" && "#{lemma}|ltm") || "ltm"

    %{
      token_index: as_nonneg_int(c[:token_index] || c["token_index"] || 0),
      id: to_string(id),
      lemma: lemma,
      score: as_float(c[:score] || c["score"] || 1.0),
      source: :ltm,
      reason: :hippocampus_fallback
    }
  end

  defp cue_to_wm_candidate(other) do
    %{
      token_index: 0,
      id: "ltm",
      lemma: "ltm",
      score: 0.0,
      source: :ltm,
      reason: :hippocampus_fallback,
      payload: other
    }
  end

  defp extract_sentence(%{} = m), do: Map.get(m, :sentence) || Map.get(m, "sentence")
  defp extract_sentence(_), do: nil

  defp as_nonneg_int(n) when is_integer(n) and n >= 0, do: n

  defp as_nonneg_int(n) when is_binary(n) do
    case Integer.parse(String.trim(n)) do
      {i, _} when i >= 0 -> i
      _ -> 0
    end
  end

  defp as_nonneg_int(_), do: 0

  defp as_float(n) when is_number(n), do: n * 1.0

  defp as_float(n) when is_binary(n) do
    case Float.parse(String.trim(n)) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  defp cues_to_candidates(cues, sentence) do
    winners =
      case cues do
        %{winners: ws} when is_list(ws) -> ws
        %{"winners" => ws} when is_list(ws) -> ws
        l when is_list(l) -> l
        nil -> []
        x -> [x]
      end

    sent = normalize_sentence(sentence)

    Enum.flat_map(winners, fn w ->
      cond do
        is_map(w) and (w[:id] || w["id"]) ->
          id = w[:id] || w["id"]

          [
            %{
              token_index: w[:token_index] || w["token_index"] || 0,
              id: to_string(id),
              score: w[:score] || w["score"] || 1.0
            }
          ]

        is_map(w) and (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) ->
          lemma = (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) |> to_string()

          [
            %{
              token_index: w[:token_index] || w["token_index"] || 0,
              lemma: lemma,
              score: w[:score] || w["score"] || 1.0
            }
          ]

        is_binary(w) and String.contains?(w, "|") ->
          [%{token_index: 0, id: w, score: 1.0}]

        is_binary(w) ->
          l =
            w
            |> String.downcase()
            |> String.replace(~r/\s+/u, " ")
            |> String.trim()

          cond do
            l == "" ->
              []

            sent != "" and not String.contains?(sent, l) ->
              []

            true ->
              [
                %{
                  token_index: 0,
                  lemma: l,
                  score: 0.30,
                  source: :ltm,
                  reason: :hippocampus_fallback
                }
              ]
          end

        true ->
          []
      end
    end)
  end

  defp normalize_sentence(s) when is_binary(s),
    do: s |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp normalize_sentence(_), do: ""

  defp first_lemma_from_slate(%{winners: winners}) when is_list(winners) do
    winners
    |> Enum.find_value(fn w ->
      w[:lemma] || w["lemma"] || parse_id_word(w[:id] || w["id"]) || w[:word] || w["word"]
    end) || "ltm"
  end

  defp first_lemma_from_slate(_), do: "ltm"

  defp parse_id_word(nil), do: nil

  defp parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end
end
