defmodule Core do
  @moduledoc """
  Pipeline:
  Tokenize → Rebuild word n-grams → STM → LTM (DB read/enrich)
  → MWE Signatures (late bias) → (Episodes attach) → LIFG Stage-1 → ATL → Hippocampus encode
  → (Optional) Persist episode → Notify Brain → Response plan attach.

  Notes:
  • `resolve_input/2` defaults to :prod mode and runs the full pipeline.
  • LTM no longer seeds or refetches via Lexicon — DB is the source of truth.
  • MWE Signatures are optional early and required late (unless disabled).
  • All Brain interactions are guarded so tests without Brain processes still pass.
  • Response planning is delegated to `Core.Response.Attach`.

  ## Examples

      iex> si = Core.resolve_input("good afternoon", mode: :test)
      iex> {si.sentence, si.source, is_list(si.tokens)}
      {"good afternoon", :test, true}

  """

  alias Core.BrainAdapter
  alias Core.Intent.Selection
  alias Core.LIFG.Attach, as: LIFGAttach
  alias Core.MWE.Signatures
  alias Core.Response.Attach, as: ResponseAttach
  alias Core.SemanticInput
  alias Core.TokenFilters
  alias Db
  alias Db.BrainCell

  @type opts :: keyword()

  # ───────────────────────── Struct/merge helpers ─────────────────────────────

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

  defp merge_into_si(%SemanticInput{} = si, %{} = brain_out) do
    Enum.reduce(brain_out, si, fn {k, v}, acc ->
      if MapSet.member?(@si_fields, k), do: Map.put(acc, k, v), else: acc
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

  # ───────────────────────── Public API ───────────────────────────────────────

  @doc """
  Resolve a raw input string into a `Core.SemanticInput` by running Core’s semantic pipeline.

  This is the primary entry point for transforming user text into structured state that can be
  consumed by the downstream “brain regions”. It has two operational modes:

  ## 1) Front-half shaping (always runs)

  These steps run regardless of mode so you always get a coherent `%SemanticInput{}`:

  - **Tokenize**
    Calls `Core.LIFG.Input.tokenize/2` to turn the raw string into initial tokens.

  - **Wrap into `SemanticInput`**
    Calls `wrap_si/2` to guarantee the output is a `%SemanticInput{}` with at least:
    `:sentence`, `:tokens`, `:source`, and `:trace`.

  - **Rebuild word n-grams**
    Calls `TokenFilters.rebuild_word_ngrams/2` to synthesize word-level n-gram tokens
    up to `opts[:max_wordgram_n]` (default `3`). These n-grams support multiword matching
    and later sense competition.

  - **Attach execution metadata**
    - `:source` is set to `:prod` or `:test` based on `opts[:mode]`
    - `:trace` is ensured to exist
    - `:lifg_opts` is attached after merging app defaults
      (`Application.get_env(:brain, :lifg_defaults, [])`) with `opts[:lifg_opts]`

  - **Intent selection**
    Calls `Core.Intent.Selection.select/2` to attach intent/keyword/confidence signals.
    This call may return either:
    - an SI-like map/struct, or
    - `{si, result}`

    Both forms are normalized into a `%SemanticInput{}` via `coerce_si/1`.

  ## 2) Production pipeline (runs only when `opts[:mode] == :prod`)

  When in `:prod` mode, the function continues with the full cognitive pipeline:

  - **STM (short-term memory)**
    Uses `brain_roundtrip(&BrainAdapter.stm/1)` to safely call Brain and merge back only
    known `SemanticInput` fields.

  - **Word-boundary filtering**
    `TokenFilters.keep_only_word_boundary_tokens/1` removes tokens that do not align to
    word boundaries (guardrail against substrings leaking into sense selection).

  - **MWE Signatures (early bias)**
    `maybe_run_mwe(:early, ...)` optionally applies early multiword bias to improve downstream
    selection before LTM is loaded.

  - **LTM (long-term memory via DB lookup)**
    `ltm_stage/2` calls `Db.ltm/2`, merges returned rows into `:active_cells`, dedupes by `id`,
    and updates `:activation_summary.db_hits`.

  - **Word-boundary filtering again**
    Re-applies boundary filtering after LTM augmentation.

  - **MWE Signatures (late bias)**
    `maybe_run_mwe(:late, ...)` runs again late when evidence is strongest (tokens + LTM + intent).

  - **Relations**
    `Core.Relations.attach_edges/1` attaches relational edges among tokens/cells.

  - **Episode + affect hooks**
    `BrainAdapter.maybe_attach_episodes/2` and `BrainAdapter.maybe_amygdala_react/2`
    optionally enrich with episodic recall and affective modulation.

  - **LIFG Stage-1**
    `run_lifg_and_attach/2` runs competitive sense selection and attaches winners/margins/guards.

  - **ATL integration and pairing**
    `BrainAdapter.maybe_ingest_atl/2` and `BrainAdapter.atl_attach_lifg_pairs/2` attach ATL outputs
    and connect LIFG winners into ATL pair structures.

  - **Hippocampus**
    `BrainAdapter.maybe_encode_hippocampus/1` encodes an episode candidate; optional persistence
    occurs via `BrainAdapter.maybe_persist_episode/2`.

  - **Response planning**
    `maybe_build_response_plan/2` delegates to `Core.Response.Attach` to attach response policy outputs.

  - **Notify Brain**
    `BrainAdapter.notify_activation/2` emits/records an activation event for observers/telemetry/UI.

  ## Options

  - `:mode` — `:prod` (default) runs the full pipeline; any other value stops after intent selection.
  - `:max_wordgram_n` — maximum size for word n-grams used in the n-gram rebuild step (default `3`).
  - `:lifg_opts` — overrides merged into `Application.get_env(:brain, :lifg_defaults, [])`.
  - `:mwe_signatures` — `:inherit` (default), `:off`/`false`, or any truthy value to enable.
  - `:mwe_extra_lex`, `:mwe_multiplier`, `:mwe_demote_funcs?` — forwarded into MWE signature passes.

  ## Examples

      iex> si = Core.resolve_input("good afternoon", mode: :test)
      iex> si.sentence
      "good afternoon"
      iex> si.source
      :test
      iex> is_list(si.tokens)
      true

  """
  @spec resolve_input(String.t(), opts()) :: SemanticInput.t()
  def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
    mode = Keyword.get(opts, :mode, :prod)
    max_n = Keyword.get(opts, :max_wordgram_n, 3)

    lifg_defaults = Application.get_env(:brain, :lifg_defaults, [])
    lifg_opts = Keyword.merge(lifg_defaults, Keyword.get(opts, :lifg_opts, []))

    si0 =
      phrase
      |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n)
      |> wrap_si(phrase)
      |> TokenFilters.rebuild_word_ngrams(max_n)
      |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
      |> Map.put_new(:trace, [])
      |> Map.put(:lifg_opts, lifg_opts)

    sel_out = Selection.select(si0, opts)

    si1 =
      case sel_out do
        {si, _res} -> coerce_si(si)
        si -> coerce_si(si)
      end

    case mode do
      :prod ->
        si1
        |> brain_roundtrip(&BrainAdapter.stm/1)
        |> TokenFilters.keep_only_word_boundary_tokens()
        |> maybe_run_mwe(:early, opts)
        |> ltm_stage(opts)
        |> TokenFilters.keep_only_word_boundary_tokens()
        |> maybe_run_mwe(:late, opts)
        |> Core.Relations.attach_edges()
        |> BrainAdapter.maybe_attach_episodes(opts)
        |> BrainAdapter.maybe_amygdala_react(opts)
        |> run_lifg_and_attach(lifg_opts)
        |> BrainAdapter.maybe_ingest_atl(opts)
        |> brain_roundtrip(&BrainAdapter.atl_attach_lifg_pairs(&1, opts))
        |> BrainAdapter.maybe_encode_hippocampus()
        |> BrainAdapter.maybe_persist_episode(opts)
        |> maybe_build_response_plan(opts)
        |> BrainAdapter.notify_activation(opts)

      _ ->
        si1
    end
  end

  # ─────────────────────── Response planner hook ───────────────────────

  defp maybe_build_response_plan(%{} = si, opts),
    do: ResponseAttach.maybe_build_response_plan(si, opts)

  defp maybe_build_response_plan(si, _opts), do: si

  # ─────────────────────── LIFG attach (extracted) ───────────────────────

  defp run_lifg_and_attach(si, lifg_opts) when is_map(si) and is_list(lifg_opts),
    do: LIFGAttach.run_and_attach(si, lifg_opts)

  defp run_lifg_and_attach(si, _lifg_opts), do: si

  # ─────────────────────── LTM orchestrator ───────────────────────

  defp ltm_stage(si, opts) when is_map(si) do
    case Db.ltm(si, opts) do
      {:ok, %{rows: rows, db_hits: db_hits}} ->
        existing =
          case Map.get(si, :active_cells, []) do
            l when is_list(l) -> l
            _ -> []
          end

        active_cells =
          existing
          |> Kernel.++(rows)
          |> Enum.flat_map(&sanitize_cell/1)
          |> Enum.reject(&(cell_id(&1) == nil))
          |> Enum.uniq_by(&cell_id/1)

        activation_summary =
          si
          |> Map.get(:activation_summary, %{})
          |> Map.update(:db_hits, db_hits, fn acc -> MapSet.union(acc, db_hits) end)

        si
        |> Map.put(:active_cells, active_cells)
        |> Map.put(:activation_summary, activation_summary)

      _ ->
        si
    end
  end

  # ───────────────────────── Helpers ─────────────────────────

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

  # ─────────────────────── MWE Signatures ───────────────────────

  defp maybe_run_mwe(si, stage, opts) do
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
end

