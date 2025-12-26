# apps/core/lib/core/semantic_input.ex
defmodule Core.SemanticInput do
  @moduledoc """
  Core pipeline carrier (SI). Minimal, unambiguous, but **struct-stable**.

  The Core pipeline frequently attaches additional keys (intent, evidence, LIFG/ATL
  products, activation summaries, etc.). Those keys should exist on the struct so
  Core can return `%Core.SemanticInput{}` without losing fields during coercion.

  Required fields (core invariants):
    • sentence      — original input sentence (single source of truth)
    • source        — origin (:prod | :test | :user | etc.)
    • tokens        — token list (maps/structs)
    • active_cells  — lexicon/DB rows attached by downstream stages
    • trace         — ordered list of stage events (maps/tuples)

  Frequently-attached fields (kept on the struct):
    • intent / keyword / confidence
    • intent_bias
    • sense_candidates
    • lifg_opts / lifg_choices / acc_conflict
    • atl_slate
    • activation_summary
    • evidence
    • episode
    • response_text / response_tone / response_meta
    • emotion / appraisal
    • frame / frame_ts_ms / frame_seq / frame_run_id
  """

  @type token :: term()
  @type cell :: term()

  @type sense_candidate :: %{
          required(:id) => String.t(),
          optional(:score) => number(),
          optional(:lemma) => String.t(),
          optional(:norm) => String.t(),
          optional(:pos) => atom(),
          optional(:rel_prior) => number(),
          optional(:mw) => boolean(),
          optional(:source) => atom()
        }

  @type t :: %__MODULE__{
          # invariants
          sentence: String.t() | nil,
          source: atom() | nil,
          tokens: [token()],
          active_cells: [cell()],
          trace: list(),

          # intent surface
          intent: atom() | nil,
          keyword: String.t() | nil,
          confidence: number() | nil,
          intent_bias: map(),

          # candidates/winners
          sense_candidates: %{optional(non_neg_integer()) => [sense_candidate()]},
          lifg_opts: keyword() | nil,
          lifg_choices: list() | nil,
          acc_conflict: number() | nil,

          # integration products
          atl_slate: map() | nil,
          activation_summary: map() | nil,
          evidence: map() | nil,
          episode: map() | nil,

          # planner / UI surface
          response_text: String.t() | nil,
          response_tone: atom() | nil,
          response_meta: map() | nil,

          # affect
          emotion: map() | nil,
          appraisal: map() | nil,

          # misc products some stages attach
          mwe_matches: list() | nil,

          # execution/frame metadata
          frame: map() | nil,
          frame_ts_ms: integer() | nil,
          frame_seq: integer() | nil,
          frame_run_id: integer() | nil
        }

  defstruct sentence: nil,
            source: nil,
            tokens: [],
            active_cells: [],
            trace: [],
            intent: nil,
            keyword: nil,
            confidence: nil,
            intent_bias: %{},
            sense_candidates: %{},
            lifg_opts: nil,
            lifg_choices: nil,
            acc_conflict: nil,
            atl_slate: nil,
            activation_summary: %{db_hits: MapSet.new()},
            evidence: nil,
            episode: nil,
            response_text: nil,
            response_tone: nil,
            response_meta: nil,
            emotion: nil,
            appraisal: nil,
            mwe_matches: nil,
            frame: nil,
            frame_ts_ms: nil,
            frame_seq: nil,
            frame_run_id: nil

  @doc """
  Record scored sense candidates for a token into `si.sense_candidates`.

  - `token_index` — index of the token in `si.tokens`.
  - `scored` — list of `{id, score}` or `%{id: id, score: score}` or plain `id`.
  - `lemma` — token lemma (or downcased surface if you don’t have a lemma).

  Options:
    * `:margin`     — include near-winners within (max_score - margin). Default 0.15
    * `:top_k`      — keep at most K per token after merge. Default 4
    * `:min_score`  — hard floor; drop anything below. Default nil (no floor)
  """
  @spec emit_sense_candidates(map(), non_neg_integer(), list(), String.t(), keyword()) :: map()
  def emit_sense_candidates(%{} = si, token_index, scored, lemma, opts \\ []) do
    margin = Keyword.get(opts, :margin, 0.15)
    top_k = Keyword.get(opts, :top_k, 4)
    min_score = Keyword.get(opts, :min_score, nil)

    list =
      scored
      |> Enum.map(fn
        {id, score} -> %{id: id, score: score, lemma: lemma}
        %{id: id, score: s} -> %{id: id, score: s, lemma: lemma}
        id when is_binary(id) -> %{id: id, score: 0.0, lemma: lemma}
      end)

    max_score =
      case Enum.max_by(list, & &1.score, fn -> %{score: -1.0e9} end) do
        %{score: s} -> s
      end

    filtered =
      list
      |> Enum.filter(fn %{score: s} ->
        (is_nil(min_score) or s >= min_score) and s >= max_score - margin
      end)
      |> Enum.sort_by(& &1.score, :desc)
      |> Enum.take(top_k)
      |> Enum.reduce(%{}, fn %{id: id} = cand, acc ->
        case acc do
          %{^id => existing} ->
            if cand.score > existing.score, do: Map.put(acc, id, cand), else: acc

          _ ->
            Map.put(acc, id, cand)
        end
      end)
      |> Map.values()
      |> Enum.sort_by(& &1.score, :desc)

    merged_per_idx =
      si
      |> Map.get(:sense_candidates, %{})
      |> Map.update(token_index, filtered, fn existing ->
        (existing ++ filtered)
        |> Enum.reduce(%{}, fn %{id: id} = cand, acc ->
          case acc do
            %{^id => e} -> if cand.score > e.score, do: Map.put(acc, id, cand), else: acc
            _ -> Map.put(acc, id, cand)
          end
        end)
        |> Map.values()
        |> Enum.sort_by(& &1.score, :desc)
        |> Enum.take(top_k)
      end)

    Map.put(si, :sense_candidates, merged_per_idx)
  end

  @doc """
  Get all sense candidates, or only those for a single token index.
  """
  @spec get_sense_candidates(map(), non_neg_integer() | :all) :: list() | map()
  def get_sense_candidates(%{} = si, idx \\ :all) do
    sc = Map.get(si, :sense_candidates, %{})

    case idx do
      :all -> sc
      _ -> Map.get(sc, idx, [])
    end
  end
end
