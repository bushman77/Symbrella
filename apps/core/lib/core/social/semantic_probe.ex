defmodule Core.Social.SemanticProbe do
  @moduledoc """
  Bounded, observation-only lexical and grammatical analysis of external text.

  Does not call Core.resolve_input(mode: :prod), because that path can write
  Hippocampus, focus WM, notify neurons, and record synaptic experiences.
  Instead we reuse Core's tokenizer, read-only LTM lookup, syntax evidence and
  LIFG Stage1 scoring with learning and normal Stage1 telemetry disabled.
  A selected lexical candidate is NOT a verified meaning or a factual claim.
  """

  @max_input_chars 420
  @max_tokens 32
  @max_choices 12
  @low_margin 0.15

  @doc """
  Analyze a short excerpt. Returns only a bounded diagnostic, never the
  underlying SemanticInput, candidate slates, or external instructions.
  """
  def run(text, opts \\ [])

  def run(text, opts) when is_binary(text) and is_list(opts) do
    excerpt = text |> String.trim() |> String.slice(0, @max_input_chars)

    if excerpt == "" do
      {:error, :empty_social_text}
    else
      analyze(excerpt, opts)
    end
  end

  def run(_, _), do: {:error, :invalid_social_text}

  defp analyze(excerpt, opts) do
    tokens =
      excerpt
      |> Core.LIFG.Input.tokenize(fuzzy: false, max_wordgram_n: 1)
      |> Enum.filter(&is_map/1)
      |> Enum.take(@max_tokens)

    si = %{
      sentence: excerpt,
      tokens: tokens,
      source: :colony_external,
      experience_origin: :colony_external,
      active_cells: [],
      sense_candidates: %{},
      trace: []
    }

    # Explicitly bounded *read* of the existing lexicon. No candidate writes.
    si =
      if Keyword.get(opts, :ltm?, true) do
        Core.Pipeline.LTM.run(si, limit_per_norm: 8)
      else
        si
      end

    si =
      si
      |> Core.Syntax.Signatures.attach()
      |> Core.Syntax.RoleCompetition.attach()
      |> Core.Syntax.SentenceCompetition.attach()

    signatures =
      si
      |> Map.get(:sentence_signatures, [])
      |> Enum.take(12)
      |> Enum.map(fn sig ->
        Map.take(sig, [:id, :span, :roles])
      end)

    {lifg, choices} = score_stage1(si, opts)

    summary =
      si
      |> Map.put(:lifg_choices, project_choices(choices))
      |> Core.Comprehension.Summary.build()

    {:ok,
     %{
       stage: :social_semantic_shadow,
       input_chars: String.length(excerpt),
       token_count: length(tokens),
       syntax: %{status: :observed, signatures: signatures},
       lifg: lifg,
       comprehension: %{
         recognized_topics: Enum.take(summary.recognized_topics, 6),
         selected_lexical_candidates: Enum.take(summary.selected_lexical_candidates, 6),
         understood: Enum.take(summary.understood, 6),
         uncertain: Enum.take(summary.uncertain, 6),
         degraded?: summary.degraded?,
         reasons: Enum.take(summary.reasons, 6)
       },
       acc_shadow: conflict_shadow(lifg),
       sentence_meaning: %{status: :not_verified, reason: :external_assertion_without_proof}
     }}
  end

  defp score_stage1(si, opts) do
    # A caller-injected evaluator is useful in tests; the normal implementation
    # always uses the real Brain.LIFG.Stage1 engine with isolated telemetry.
    scorer = Keyword.get(opts, :scorer, &Brain.LIFG.Stage1.run/2)

    shadow_opts = [
      cerebellum_learn?: false,
      reanalysis: false,
      reanalysis?: false,
      mwe_fallback: false,
      stage1_local_mwe_fallback?: false,
      sense_focus?: false,
      semantic_control?: false,
      stage1_stop_event: [:core, :social, :shadow_stage1_stop],
      chargram_event: [:core, :social, :shadow_chargram],
      boundary_event: [:core, :social, :shadow_boundary],
      mwe_event: [:core, :social, :shadow_mwe]
    ]

    case scorer.(si, shadow_opts) do
      {:ok, %{choices: choices, audit: audit}} when is_list(choices) and is_map(audit) ->
        selected =
          choices
          |> Enum.filter(&is_map/1)
          |> Enum.take(@max_choices)

        {%{
           status: :scored,
           chosen_count: length(choices),
           missing_candidates: Map.get(audit, :missing_candidates, 0),
           choices: Enum.map(selected, &compact_choice/1)
         }, selected}

      {:error, _} ->
        {%{status: :unavailable, chosen_count: 0, missing_candidates: nil, choices: []}, []}

      _ ->
        {%{status: :unavailable, chosen_count: 0, missing_candidates: nil, choices: []}, []}
    end
  end

  defp compact_choice(choice) do
    %{
      token_index: Map.get(choice, :token_index),
      chosen_id: limited_id(Map.get(choice, :chosen_id)),
      margin: bounded_number(Map.get(choice, :margin)),
      probability_margin: bounded_number(Map.get(choice, :prob_margin))
    }
  end

  defp project_choices(choices) do
    Enum.map(choices, fn choice ->
      id = Map.get(choice, :chosen_id)

      %{
        id: limited_id(id),
        lemma: lemma(id),
        token_index: Map.get(choice, :token_index),
        score: bounded_number(Map.get(choice, :prob_margin))
      }
    end)
  end

  defp lemma(id) when is_binary(id) do
    id
    |> String.split("|", parts: 2)
    |> List.first()
    |> String.slice(0, 80)
  end

  defp lemma(_), do: nil

  defp limited_id(id) when is_binary(id), do: String.slice(id, 0, 128)
  defp limited_id(_), do: nil
  defp bounded_number(n) when is_number(n), do: n * 1.0
  defp bounded_number(_), do: 0.0

  defp conflict_shadow(%{status: :scored, choices: choices, missing_candidates: missing}) do
    low_margin =
      Enum.count(choices, fn %{margin: margin} -> margin < @low_margin end)

    %{
      status: :shadow_only,
      low_margin_choices: low_margin,
      missing_candidates: missing,
      uncertain?: low_margin > 0 or (is_integer(missing) and missing > 0),
      note: :lexical_ambiguity_not_factual_contradiction
    }
  end

  defp conflict_shadow(_) do
    %{status: :unavailable, uncertain?: true, note: :lexical_evaluation_missing}
  end
end
