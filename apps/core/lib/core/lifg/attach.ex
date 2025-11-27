defmodule Core.LIFG.Attach do
  @moduledoc """
  Core-side LIFG runner + attachment helpers.

  Extracted from Core.ex to keep the main pipeline slim.
  Responsibilities:
    • Call Brain.LIFG (safely) and attach :lifg_choices / :atl_slate / trace
    • Apply tiny tie-break priors and MWE-friendly bumps
    • MWE shadowing (suppress overlapping unigrams when an MWE wins)
  """

  # ── knobs ─────────────────────────────────────────────────────────────
  @lifg_tie_epsilon 0.01
  @lifg_prob_epsilon 0.01

  # Bias knobs for multi-word expressions (config-driven):
  #   :mwe_greet_phrase_bump — extra score for greeting phrases
  #   :mwe_general_bump      — extra score for non-greeting MWEs
  @greet_phrase_bump Application.compile_env(:core, :mwe_greet_phrase_bump, 0.01)
  @mwe_general_bump Application.compile_env(:core, :mwe_general_bump, 0.008)

  alias Brain

  @spec run_and_attach(map(), keyword()) :: map()
  def run_and_attach(%{tokens: tokens} = si, lifg_opts) when is_list(tokens) do
    # Pass intent bias downstream (non-breaking; ignored if unused)
    lifg_opts2 = Keyword.put(lifg_opts, :intent_bias, Map.get(si, :intent_bias, %{}))

    case safe_lifg_run(si, lifg_opts2) do
      {:ok, %{si: si_after, choices: raw_choices, slate: slate, flips: flips}}
      when is_list(raw_choices) and is_map(slate) ->
        tokens_for_choices = Map.get(si_after, :tokens, tokens)
        intent_fallback = Map.get(si, :intent)

        lifg_choices =
          raw_choices
          |> Enum.map(fn ch ->
            token_index = Map.get(ch, :token_index, 0)
            t = Enum.at(tokens_for_choices, token_index, %{})
            token_norm = t |> Map.get(:phrase, "") |> norm()

            chosen_id = Map.get(ch, :chosen_id)
            scores = Map.get(ch, :scores) || %{}
            feats = Map.get(ch, :features) || %{}
            alt_ids = Map.get(ch, :alt_ids, [])
            margin = Map.get(ch, :margin, 0.0)
            prob_margin = Map.get(ch, :prob_margin, 0.0)

            phrase_bump_for = fn id, thin? ->
              cond do
                Map.get(t, :n, 1) > 1 and id_norm(id) == token_norm ->
                  base = if thin?, do: @mwe_general_bump, else: 0.0

                  greet =
                    if Map.get(si_after, :intent, intent_fallback) == :greet,
                      do: @greet_phrase_bump,
                      else: 0.0

                  base + greet

                true ->
                  0.0
              end
            end

            base_score =
              if is_binary(chosen_id) and is_map(scores),
                do: Map.get(scores, chosen_id, 0.0),
                else: Map.get(feats, :score_norm, 0.0)

            candidates =
              [chosen_id | alt_ids]
              |> Enum.uniq()
              |> Enum.reject(&is_nil/1)

            matching = Enum.filter(candidates, &(id_norm(&1) == token_norm))

            # Tiny prior only for razor-thin cases
            pos_prior = %{
              "phrase" => 0.03,
              "noun" => 0.01,
              "verb" => 0.0,
              "adjective" => 0.0
            }

            thin? = margin < @lifg_tie_epsilon or prob_margin < @lifg_prob_epsilon
            score_of = fn id -> Map.get(scores, id, base_score) + phrase_bump_for.(id, thin?) end

            pick_with_prior = fn ids ->
              Enum.max_by(
                ids,
                fn id -> score_of.(id) + Map.get(pos_prior, id_pos(id), 0.0) end,
                fn -> chosen_id end
              )
            end

            {chosen_id2, score2} =
              cond do
                matching != [] and thin? ->
                  best = pick_with_prior.(matching)
                  {best, Map.get(scores, best, base_score)}

                matching != [] ->
                  best = Enum.max_by(matching, &score_of.(&1), fn -> chosen_id end)
                  {best, Map.get(scores, best, base_score)}

                thin? ->
                  best = pick_with_prior.(candidates)
                  {best, Map.get(scores, best, base_score)}

                true ->
                  {chosen_id, base_score}
              end

            %{
              token_index: token_index,
              lemma: token_norm,
              id: chosen_id2,
              alt_ids: alt_ids,
              score: score2
            }
          end)
          |> mwe_shadow(tokens_for_choices)

        ev = %{
          stage: :lifg_run,
          ts_ms: System.system_time(:millisecond),
          choice_count: length(raw_choices),
          flips: flips
        }

        si_after
        |> Map.put(:atl_slate, slate)
        |> Map.put(:lifg_choices, lifg_choices)
        |> Map.put(:acc_conflict, Map.get(si_after, :acc_conflict, 0.0))
        |> Map.update(:trace, [], &[ev | &1])

      _ ->
        si
    end
  end

  def run_and_attach(si, _lifg_opts), do: si

  # ───────────────────────── Brain call (safe) ─────────────────────────

  defp safe_lifg_run(si, opts) do
    cond do
      Code.ensure_loaded?(Brain) and function_exported?(Brain, :lifg_run, 2) ->
        try do
          apply(Brain, :lifg_run, [si, opts])
        rescue
          _ -> apply(Brain.LIFG, :run, [si, opts])
        end

      Code.ensure_loaded?(Brain.LIFG) and function_exported?(Brain.LIFG, :run, 2) ->
        apply(Brain.LIFG, :run, [si, opts])

      true ->
        {:error, :lifg_unavailable}
    end
  end

  # ───────────────────────── MWE shadowing ─────────────────────────

  defp mwe_shadow(lifg_choices, tokens) when is_list(lifg_choices) and is_list(tokens) do
    mwe_spans =
      lifg_choices
      |> Enum.map(& &1.token_index)
      |> Enum.map(&Enum.at(tokens, &1, %{}))
      |> Enum.filter(fn t -> Map.get(t, :n, 1) > 1 end)
      |> Enum.map(&Map.get(&1, :span))

    if mwe_spans == [] do
      lifg_choices
    else
      Enum.reject(lifg_choices, fn ch ->
        t = Enum.at(tokens, ch.token_index, %{})

        if Map.get(t, :n, 1) == 1 do
          covered_by_any_mwe?(Map.get(t, :span), mwe_spans)
        else
          false
        end
      end)
    end
  end

  defp covered_by_any_mwe?({a, l}, spans) when is_integer(a) and is_integer(l) do
    b = a + l

    Enum.any?(spans, fn
      {s, sl} when is_integer(s) and is_integer(sl) ->
        e = s + sl
        a >= s and b <= e

      _ ->
        false
    end)
  end

  defp covered_by_any_mwe?(_, _), do: false

  # ───────────────────────── id helpers ─────────────────────────

  defp id_norm(nil), do: nil

  defp id_norm(id) when is_binary(id) do
    id
    |> String.split("|")
    |> List.first()
  end

  defp id_pos(nil), do: nil

  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, p | _] -> p
      _ -> nil
    end
  end

  # ───────────────────────── norm ─────────────────────────

  defp norm(nil), do: ""

  defp norm(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(v) do
    v
    |> Kernel.to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end
end

