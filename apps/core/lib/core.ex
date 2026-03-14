# apps/core/lib/core.ex
defmodule Core do
  @moduledoc """
  Pipeline:
  Tokenize → Rebuild word n-grams → STM → LTM (DB read)
  → MWE Signatures (early/late bias) → Relations → Episodes attach → Amygdala react
  → LIFG Stage-1 (via Brain GenServer for WM updates) → ATL → Hippocampus encode
  → (Optional) Persist episode → Notify Brain → Response plan attach.

  Notes:
  • `resolve_input/2` defaults to :prod and runs the full pipeline.
  • LTM reads from DB (Db.ltm/2). No Lexicon seeding/enrichment.
  • Brain roundtrips are guarded so tests can run without Brain processes.
  • Response planning is delegated to `Core.Response.Attach`.

  ## Examples

      iex> si = Core.resolve_input("good afternoon", mode: :test)
      iex> {si.sentence, si.source, is_list(si.tokens)}
      {"good afternoon", :test, true}
  """

  alias Core.BrainAdapter
  alias Core.Intent.Selection
  alias Core.MWE.Signatures
  alias Core.Response.Attach, as: ResponseAttach
  alias Core.SemanticInput
  alias Core.TokenFilters
  alias Db
  alias Db.BrainCell

  @type opts :: keyword()

  # ───────────────────────── Struct/merge helpers ─────────────────────────

  @si_template %SemanticInput{}
  @si_fields @si_template |> Map.from_struct() |> Map.keys() |> MapSet.new()

  # Brain roundtrips may return stale / cross-turn fields (especially candidates/tokens).
  # Core treats these as write-owned by the current turn and refuses to merge them back in.
  @brain_merge_blocklist MapSet.new([
                           :sentence,
                           :source,
                           :lifg_opts,

                           # per-turn derived token structures
                           :tokens,
                           :token_structs,
                           :pos_list,
                           :cells,
                           :pattern_roles,
                           :phrase_matches,

                           # per-turn candidates can leak across turns
                           :sense_candidates
                         ])

  @doc false
  @spec __merge_brain_out__(SemanticInput.t() | map() | String.t(), map()) :: SemanticInput.t()
  def __merge_brain_out__(si_or_map, %{} = brain_out) do
    si = coerce_si(si_or_map)
    merge_into_si(si, brain_out)
  end

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

  defp merge_into_si(%SemanticInput{} = si, %{} = brain_out) do
    Enum.reduce(brain_out, si, fn {k, v}, acc ->
      cond do
        not is_atom(k) ->
          acc

        not MapSet.member?(@si_fields, k) ->
          acc

        MapSet.member?(@brain_merge_blocklist, k) ->
          acc

        k == :trace and is_list(v) ->
          merge_trace(acc, v)

        true ->
          Map.put(acc, k, v)
      end
    end)
  end

  defp merge_trace(%SemanticInput{} = si, brain_trace) when is_list(brain_trace) do
    Map.update(si, :trace, brain_trace, fn core_trace ->
      # Both traces are treated as newest-first lists. Keep Brain events first.
      brain_trace ++ core_trace
    end)
  end

  defp brain_roundtrip(si_or_map, fun) when is_function(fun, 1) do
    si = coerce_si(si_or_map)
    brain_in = Map.from_struct(si)

    case fun.(brain_in) do
      %{} = out -> merge_into_si(si, out)
      _ -> si
    end
  end

  # ───────────────────────── Public API ────────────────────────────────

  @doc """
  Resolve a raw input string into a `Core.SemanticInput` by running Core’s semantic pipeline.

  Options:
    • mode: :prod | :test (default :prod)
    • max_wordgram_n: integer (default 3)
    • lifg_opts: keyword (merged with :brain, :lifg_defaults)

  See module docs for the end-to-end pipeline description.
  """
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

  # ───────────────────────── Front-half helpers ─────────────────────────

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

  # ───────────────────────── Production pipeline ─────────────────────────

  defp run_prod_pipeline(%SemanticInput{} = si, opts, lifg_opts),
    do: run_prod_pipeline(Map.from_struct(si), opts, lifg_opts)

  defp run_prod_pipeline(%{} = si, opts, lifg_opts) when is_list(opts) and is_list(lifg_opts) do
    atl_opts = Keyword.get(opts, :atl_opts, [])

    si
    |> brain_roundtrip(&BrainAdapter.stm/1)
    |> reset_sense_candidates(:post_stm)
    |> TokenFilters.keep_only_word_boundary_tokens()
    |> maybe_run_mwe(:early, opts)
    |> ltm_stage(opts)
    |> TokenFilters.keep_only_word_boundary_tokens()
    |> maybe_run_mwe(:late, opts)
    |> Core.Relations.attach_edges()
    |> maybe_drop_empty_evidence()
    |> BrainAdapter.maybe_attach_episodes(opts)
    |> BrainAdapter.maybe_amygdala_react(opts)
    |> run_lifg_and_attach(lifg_opts)
    |> BrainAdapter.maybe_ingest_atl(atl_opts)
    |> BrainAdapter.atl_attach_lifg_pairs(atl_opts)
    |> BrainAdapter.maybe_encode_hippocampus()
    |> BrainAdapter.maybe_persist_episode(opts)
    |> maybe_build_response_plan(opts)
    |> BrainAdapter.notify_activation(opts)
  end

  defp run_prod_pipeline(other, _opts, _lifg_opts), do: other

  # ─────────────────────── Response planner hook ───────────────────────

  defp maybe_build_response_plan(%{} = si, opts),
    do: ResponseAttach.maybe_build_response_plan(si, opts)

  defp maybe_build_response_plan(si, _opts), do: si

  # ─────────────────────── LIFG attach (WM-aware) ───────────────────────
  #
  # IMPORTANT:
  # If you want Working Memory (WM) updated, you must run LIFG through the Brain GenServer.
  # The Brain server can apply gating and update its internal WM/attention state.
  #
  # Message shape (Brain side):
  #   GenServer.call(Brain, {:lifg_stage1, si_map, ctx_vec, lifg_opts}, :infinity)
  #
  # We attach:
  #   • si.lifg_choices (normalized cover/choices list, best-effort)
  #   • si.trace merge (best-effort)
  #
  defp run_lifg_and_attach(%{} = si, lifg_opts) when is_list(lifg_opts) do
    if Code.ensure_loaded?(Brain) and is_pid(Process.whereis(Brain)) do
      ctx_vec = Keyword.get(lifg_opts, :ctx_vec, [])

      reply =
        try do
          GenServer.call(Brain, {:lifg_stage1, si, ctx_vec, lifg_opts}, :infinity)
        rescue
          _ -> {:error, :lifg_call_failed}
        catch
          :exit, _ -> {:error, :lifg_call_exit}
        end

      attach_lifg_reply(si, reply)
    else
      si
    end
  end

  defp run_lifg_and_attach(si, _lifg_opts), do: si

  defp attach_lifg_reply(%{} = si, {:ok, %{} = out}) do
    lifg_choices =
      cond do
        Code.ensure_loaded?(Brain.Pipeline.LIFGStage1) and
            function_exported?(Brain.Pipeline.LIFGStage1, :extract_lifg_choices, 1) ->
          Brain.Pipeline.LIFGStage1.extract_lifg_choices(out)

        is_list(Map.get(out, :cover)) ->
          Map.get(out, :cover)

        is_list(get_in(out, [:si, :lifg_choices])) ->
          get_in(out, [:si, :lifg_choices])

        true ->
          []
      end

    si2 =
      if is_list(lifg_choices) do
        Map.put(si, :lifg_choices, lifg_choices)
      else
        si
      end

    merge_brain_trace(si2, out)
  end

  # Some Brain paths may return a tuple or embed out under {:ok, out}
  defp attach_lifg_reply(%{} = si, {{:ok, %{} = out}, _state}),
    do: attach_lifg_reply(si, {:ok, out})

  defp attach_lifg_reply(%{} = si, {:error, _reason}), do: si
  defp attach_lifg_reply(%{} = si, _other), do: si

  defp merge_brain_trace(%{} = si, %{} = out) do
    brain_trace =
      cond do
        is_list(Map.get(out, :trace)) -> Map.get(out, :trace)
        is_list(get_in(out, [:si, :trace])) -> get_in(out, [:si, :trace])
        true -> []
      end

    if is_list(brain_trace) and brain_trace != [] do
      Map.update(si, :trace, brain_trace, fn core_trace ->
        brain_trace ++ core_trace
      end)
    else
      si
    end
  end

  # ─────────────────────── LTM orchestrator ────────────────────────────

  defp ltm_stage(%{} = si, opts) when is_list(opts) do
    case Db.ltm(si, opts) do
      {:ok, %{rows: rows, db_hits: db_hits}} ->
        existing =
          case Map.get(si, :active_cells, []) do
            l when is_list(l) -> l
            _ -> []
          end

        active_cells =
          (existing ++ rows)
          |> Enum.flat_map(&sanitize_cell/1)
          |> Enum.reject(&(cell_id(&1) == nil))
          |> Enum.uniq_by(&cell_id/1)

        db_hits_ms =
          case db_hits do
            %MapSet{} = ms -> ms
            other -> MapSet.new(List.wrap(other))
          end

        activation_summary0 =
          case Map.get(si, :activation_summary) do
            %{} = m -> m
            _ -> %{}
          end

        activation_summary =
          Map.update(activation_summary0, :db_hits, db_hits_ms, fn acc ->
            acc_ms =
              cond do
                match?(%MapSet{}, acc) -> acc
                is_list(acc) -> MapSet.new(acc)
                is_nil(acc) -> MapSet.new()
                true -> MapSet.new(List.wrap(acc))
              end

            MapSet.union(acc_ms, db_hits_ms)
          end)

        si
        |> Map.put(:active_cells, active_cells)
        |> Map.put(:activation_summary, activation_summary)

      _other ->
        si
    end
  end

  defp ltm_stage(si, _opts), do: si

  # ─────────────────────── Evidence hygiene ────────────────────────────

  defp maybe_drop_empty_evidence(%{} = si) do
    case Map.get(si, :evidence) do
      %{relations: []} = ev when map_size(ev) == 1 ->
        Map.delete(si, :evidence)

      _ ->
        si
    end
  end

  defp maybe_drop_empty_evidence(other), do: other

  # ─────────────────────── MWE Signatures ──────────────────────────────

  defp maybe_run_mwe(%{} = si, stage, opts) when is_atom(stage) and is_list(opts) do
    env_on = Application.get_env(:core, :mwe_signatures, :on) != :off
    opt_val = Keyword.get(opts, :mwe_signatures, :inherit)

    enabled =
      case opt_val do
        :off -> false
        false -> false
        :inherit -> env_on
        _ -> true
      end

    if enabled do
      Signatures.run(si,
        stage: stage,
        extra_lex: Keyword.get(opts, :mwe_extra_lex, []),
        multiplier: Keyword.get(opts, :mwe_multiplier, 1.0),
        demote_funcs?: Keyword.get(opts, :mwe_demote_funcs?, true)
      )
    else
      si
    end
  end

  defp maybe_run_mwe(si, _stage, _opts), do: si

  # ───────────────────────── Sense-candidate hygiene ────────────────────

  defp reset_sense_candidates(%{} = si, tag) do
    sc = Map.get(si, :sense_candidates, %{})
    dropped = if is_map(sc), do: map_size(sc), else: 0

    si
    |> Map.put(:sense_candidates, %{})
    |> Map.update(:trace, [], fn tr ->
      [
        %{
          stage: :reset_sense_candidates,
          where: tag,
          dropped: dropped,
          ts_ms: System.system_time(:millisecond)
        }
        | tr
      ]
    end)
  end

  defp reset_sense_candidates(si, _tag), do: si

  # ───────────────────────── Helpers ────────────────────────────────

  defp wrap_si(%SemanticInput{} = si, _orig_sentence), do: si

  defp wrap_si(tokens, sentence) when is_list(tokens),
    do: %SemanticInput{sentence: sentence, tokens: tokens, source: :test, trace: []}

  defp wrap_si(other, _sentence) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp wrap_si(_other, sentence),
    do: %SemanticInput{sentence: sentence, tokens: [], source: :test, trace: []}

  defp sanitize_cell(%BrainCell{} = s), do: [s]
  defp sanitize_cell(%{id: _} = m), do: [m]
  defp sanitize_cell(%{"id" => _} = m), do: [m]
  defp sanitize_cell(id) when is_binary(id), do: [%{id: id}]
  defp sanitize_cell(_), do: []

  defp cell_id(%BrainCell{id: id}), do: id
  defp cell_id(%{id: id}), do: id
  defp cell_id(%{"id" => id}), do: id
  defp cell_id(id) when is_binary(id), do: id
  defp cell_id(_), do: nil
end
