defmodule Core.SemanticInput do
  @moduledoc """
  Core pipeline carrier (SI). Minimal, unambiguous.

  Required fields:
    • sentence      — the original input sentence (single source of truth)
    • source        — origin (e.g., :user, :test)
    • tokens        — tokens produced by Token.tokenize/1
    • active_cells  — lexicon/DB rows attached by downstream stages
    • trace         — ordered list of stage events (maps), including LIFG output

  Enriched / optional fields (filled by later stages):
    • sense_candidates — per-token sense slates (from PMTG / lexicon, etc.)
    • response_text    — text chosen by Core.Response planner
    • response_tone    — planner tone label (e.g. :warm, :neutral, :deescalate)
    • response_meta    — planner metadata/explanation map
    • emotion          — amygdala snapshot / affective appraisal map

  Notes:
    • We intentionally do NOT include :original_sentence (avoid ambiguity).
    • Stages may still read additional ad-hoc fields via Map.get/2.
    • LIFG can keep writing rich results into trace; these extra fields are a
      small, stable surface for UI / planner integration.
  """

  @type sense_candidate :: %{
          id: String.t(),
          score: number(),
          lemma: String.t()
        }

  @type t :: %__MODULE__{
          sentence: String.t() | nil,
          source: atom() | nil,
          tokens: [Core.Token.t()],
          active_cells: [map()],
          trace: [map()],
          # sense_candidates[token_index] = [sense_candidate, ...]
          sense_candidates: %{optional(non_neg_integer()) => [sense_candidate()]},
          # planner / UI surface
          response_text: String.t() | nil,
          response_tone: atom() | nil,
          response_meta: map() | nil,
          # fast affective appraisal from Amygdala (if present)
          emotion: map() | nil
        }

  defstruct sentence: nil,
            source: nil,
            tokens: [],
            active_cells: [],
            trace: [],
            sense_candidates: %{},
            response_text: nil,
            response_tone: nil,
            response_meta: nil,
            emotion: nil

  @doc """
  Record scored sense candidates for a token into `si.sense_candidates`.

  - `token_index` — index of the token in `si.tokens`.
  - `scored` — list of `{id, score}` or `%{id: id, score: score}`.
  - `lemma` — the token’s lemma (or downcased surface if you don’t have a lemma).

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
      # Dedup within this batch by id, keep highest score
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
