defmodule Core.Brain.WM do
  @moduledoc """
  Core-side working-memory bridge.

  This stage admits a small set of prompt-facing topics through `Brain.focus/2`.
  It does not mutate Brain state directly; Brain's WM gate remains the admission
  path.
  """

  alias Core.Brain.Runtime

  @brain_topics [
    {"working memory", "working memory|phrase|core"},
    {"wm", "working memory|phrase|core"},
    {"lifg", "lifg|region|core"},
    {"pmtg", "pmtg|region|core"},
    {"hippocampus", "hippocampus|region|core"},
    {"amygdala", "amygdala|region|core"},
    {"thalamus", "thalamus|region|core"},
    {"acc", "acc|region|core"},
    {"ofc", "ofc|region|core"},
    {"dlpfc", "dlpfc|region|core"},
    {"symbrella", "symbrella|system|core"}
  ]

  @spec focus_prompt_topics(map(), keyword()) :: map()
  def focus_prompt_topics(%{} = si, opts) when is_list(opts) do
    if Keyword.get(opts, :focus_prompt_topics?, true) == false do
      trace_focus(si, 0, :disabled)
    else
      candidates = topic_candidates(si)

      if candidates == [] do
        # Most ordinary questions do not mention one of @brain_topics.
        # No Brain.focus/2 call occurs, so an empty WM is *not* proof that
        # BasalGanglia rejected lexical candidates on their scores.
        trace_focus(si, 0, :none)
      else
        result = Runtime.apply_if_exported(Brain, :focus, [candidates, []], :unavailable)

        case result do
          wm when is_list(wm) -> trace_focus(si, length(candidates), :focused, length(wm))
          _ -> trace_focus(si, length(candidates), :unavailable)
        end
      end
    end
  end

  def focus_prompt_topics(si, _opts), do: si

  defp topic_candidates(si) do
    text =
      [
        Map.get(si, :sentence),
        Map.get(si, "sentence"),
        Map.get(si, :keyword),
        Map.get(si, "keyword")
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.map_join(" ", &to_string/1)
      |> normalize_text()

    @brain_topics
    |> Enum.filter(fn {phrase, _id} -> topic_present?(text, phrase) end)
    |> Enum.map(fn {phrase, id} ->
      %{
        id: id,
        lemma: phrase_label(phrase),
        score: 0.92,
        source: :intent
      }
    end)
    |> Enum.uniq_by(& &1.id)
  end

  defp topic_present?(text, phrase) do
    Regex.match?(~r/(^|\b)#{Regex.escape(phrase)}(\b|$)/iu, text)
  end

  defp phrase_label("wm"), do: "working memory"
  defp phrase_label(phrase), do: phrase

  defp normalize_text(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}\s]/u, " ")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp trace_focus(si, count, decision, wm_size_after \\ nil) do
    choices = Map.get(si, :lifg_choices) || Map.get(si, "lifg_choices") || []
    lifg_count = if is_list(choices), do: length(choices), else: 0

    reason =
      case decision do
        :none -> :no_configured_prompt_topic
        :disabled -> :disabled_by_option
        :unavailable -> :brain_focus_unavailable
        :focused -> :prompt_topic_focus
      end

    # Diagnostics describe *attempts*, not candidate admission outcomes.
    # LIFG interpretation uses Brain.lifg_run, not this Brain.focus/2 path.
    :telemetry.execute(
      [:core, :wm, :focus],
      %{topic_candidates: count, lifg_choices: lifg_count},
      %{decision: decision, reason: reason, admission_path: :prompt_topics_only}
    )

    Core.Pipeline.Trace.append(
      si,
      :wm_focus,
      decision: decision,
      reason: reason,
      scores: %{},
      meta: %{
        candidate_count: count,
        lifg_choices_available: lifg_count,
        lifg_winners_submitted?: false,
        focus_attempted?: count > 0,
        admission_path: :prompt_topics_only,
        wm_size_after: wm_size_after
      }
    )
  end
end
