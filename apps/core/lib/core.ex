defmodule Core do
  @moduledoc """
  ...
  """

  alias Core.Intent.Selection
  alias Core.LIFG.Attach, as: LifgAttach
  alias Core.Response.Attach, as: ResponseAttach
  alias Core.SemanticInput
  alias Core.TokenFilters
  alias Core.Brain.Introspection, as: BrainIntrospection
  @type opts :: keyword()

  @si_template %SemanticInput{}
  @si_fields @si_template |> Map.from_struct() |> Map.keys() |> MapSet.new()

  defp coerce_si(%SemanticInput{} = si), do: si

  defp coerce_si(%{} = m) do
    filtered =
      m
      |> Enum.filter(fn {k, _} -> is_atom(k) and MapSet.member?(@si_fields, k) end)
      |> Map.new()

    struct(SemanticInput, filtered)
  end

  defp coerce_si(other) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp coerce_si(_), do: %SemanticInput{}

  @spec resolve_input(String.t(), opts()) :: SemanticInput.t()
  def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
    mode = Keyword.get(opts, :mode, :prod)
    max_n = Keyword.get(opts, :max_wordgram_n, 3)
    lifg_opts = build_lifg_opts(opts)

    si0 =
      phrase
      |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n)
      |> wrap_si(phrase)
      |> TokenFilters.rebuild_word_ngrams(max_n)
      |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
      |> Map.put(:session_id, Keyword.get(opts, :session_id))
      |> Map.put_new(:trace, [])
      |> Map.put(:lifg_opts, lifg_opts)

    si1 = apply_intent_selection(si0, opts)

    out =
      case mode do
        :prod -> run_prod_pipeline(si1, opts, lifg_opts)
        _ -> si1
      end

    coerce_si(out)
  end

  defp build_lifg_opts(opts) do
    lifg_defaults = Application.get_env(:brain, :lifg_defaults, [])
    Keyword.merge(lifg_defaults, Keyword.get(opts, :lifg_opts, []))
  end

  defp apply_intent_selection(%{} = si, opts) do
    case Selection.select(si, opts) do
      {si2, _res} -> coerce_si(si2)
      si2 -> coerce_si(si2)
    end
  end

  defp run_prod_pipeline(%SemanticInput{} = si, opts, lifg_opts),
    do: run_prod_pipeline(Map.from_struct(si), opts, lifg_opts)

  defp run_prod_pipeline(%{} = si, opts, lifg_opts) when is_list(opts) and is_list(lifg_opts) do
    atl_opts = Keyword.get(opts, :atl_opts, [])

    si
    |> Core.Brain.STM.run()
    |> Core.Pipeline.Candidates.reset(:post_stm)
    |> TokenFilters.keep_only_word_boundary_tokens()
    |> Core.MWE.Stage.run(:early, opts)
    |> Core.Pipeline.LTM.run(opts)
    |> TokenFilters.keep_only_word_boundary_tokens()
    |> Core.MWE.Stage.run(:late, opts)
    |> Core.Relations.attach_edges()
    |> Core.Pipeline.Evidence.drop_empty()
    |> Core.Brain.Episodes.attach(opts)
    |> Core.Pipeline.Perception.run(opts)
    |> Core.Brain.Amygdala.react(opts)
    |> LifgAttach.run_and_attach(lifg_opts)
    |> BrainIntrospection.update_self_model(opts)
    |> Core.Brain.ATL.ingest(atl_opts)
    |> Core.Brain.ATL.attach_lifg_pairs(atl_opts)
    |> Core.Comprehension.Summary.attach(opts)
    |> Core.Brain.Prefrontal.attach(opts)
    |> Core.Brain.Hippocampus.encode()
    |> Core.Brain.Hippocampus.persist(opts)
    |> Core.Brain.WM.focus_prompt_topics(opts)
    |> maybe_build_response_plan(opts)
    |> Core.Brain.Activation.notify(opts)
  end

  defp run_prod_pipeline(other, _opts, _lifg_opts), do: other

  @brain_merge_blocklist MapSet.new([
                           :sentence,
                           :source,
                           :tokens,
                           :token_structs,
                           :pos_list,
                           :cells,
                           :pattern_roles,
                           :phrase_matches,
                           :sense_candidates,
                           :candidates_by_token
                         ])

  def __merge_brain_out__(%SemanticInput{} = si, %{} = brain_out) do
    allowed =
      brain_out
      |> Enum.filter(fn {key, _value} ->
        is_atom(key) and MapSet.member?(@si_fields, key) and
          not MapSet.member?(@brain_merge_blocklist, key) and key != :trace
      end)
      |> Map.new()

    trace =
      List.wrap(Map.get(brain_out, :trace)) ++
        List.wrap(Map.get(si, :trace))

    si
    |> Map.from_struct()
    |> Map.merge(allowed)
    |> Map.put(:trace, trace)
    |> then(&struct(SemanticInput, &1))
  end

  def __merge_brain_out__(si, brain_out) when is_map(si) and is_map(brain_out) do
    si
    |> coerce_si()
    |> __merge_brain_out__(brain_out)
  end

  defp maybe_build_response_plan(%{} = si, opts),
    do: ResponseAttach.maybe_build_response_plan(si, opts)

  defp maybe_build_response_plan(si, _opts), do: si

  defp wrap_si(%SemanticInput{} = si, _orig_sentence), do: si

  defp wrap_si(tokens, sentence) when is_list(tokens),
    do: %SemanticInput{sentence: sentence, tokens: tokens, source: :test, trace: []}

  defp wrap_si(other, _sentence) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp wrap_si(_other, sentence),
    do: %SemanticInput{sentence: sentence, tokens: [], source: :test, trace: []}
end
