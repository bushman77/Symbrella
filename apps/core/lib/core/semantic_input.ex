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
    • phrase_candidates — unconfirmed phrase windows derived from base tokens
    • active_cells  — lexicon/DB rows attached by downstream stages
    • trace         — ordered list of stage events (maps/tuples)

  Frequently-attached fields (kept on the struct):
    • fuzzy_corrections / fuzzy_aliases / fuzzy_confidence
    • selected_action / action_candidates / action_meta
    • agency_decision / agency_commands / agency_command_results
    • intent / keyword / confidence
    • intent_bias
    • token_cover / resolved_tokens
    • sense_candidates
    • lifg_opts / lifg_choices / acc_conflict
    • atl_slate
    • comprehension
    • prefrontal / control_signals
    • perception
    • activation_summary
    • evidence
    • episode
    • response_text / response_tone / response_meta
    • symbolic_frame
    • emotion / appraisal
    • self_model / self_monitor / self_memory_recall / self_continuity
    • frame / frame_ts_ms / frame_seq / frame_run_id
  """

  @type token :: term()
  @type cell :: term()
  @contract_v 1
  @list_fields MapSet.new([
                 :tokens,
                 :phrase_candidates,
                 :active_cells,
                 :trace,
                 :fuzzy_corrections,
                 :fuzzy_aliases,
                 :token_cover,
                 :resolved_tokens,
                 :lifg_choices,
                 :action_candidates,
                 :agency_commands,
                 :agency_command_results,
                 :mwe_matches
               ])
  @map_fields MapSet.new([
                :intent_bias,
                :sense_candidates,
                :perception,
                :atl_slate,
                :comprehension,
                :prefrontal,
                :control_signals,
                :activation_summary,
                :evidence,
                :episode,
                :response_meta,
                :symbolic_frame,
                :action_meta,
                :emotion,
                :appraisal,
                :mood,
                :self_monitor,
                :self_memory_recall,
                :self_continuity,
                :frame
              ])
  @atom_fields MapSet.new([:source, :intent, :response_tone, :selected_action])
  @number_fields MapSet.new([:fuzzy_confidence, :confidence, :acc_conflict])
  @integer_fields MapSet.new([:frame_ts_ms, :frame_seq, :frame_run_id])

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
          phrase_candidates: [map()],
          active_cells: [cell()],
          trace: list(),

          # fuzzy correction (attached at pipeline entry)
          fuzzy_corrections: [map()] | nil,
          fuzzy_aliases: [atom()] | nil,
          fuzzy_confidence: number() | nil,

          # intent surface
          intent: atom() | nil,
          keyword: String.t() | nil,
          confidence: number() | nil,
          intent_bias: map(),
          token_cover: list() | nil,
          resolved_tokens: list() | nil,

          # candidates/winners
          sense_candidates: %{optional(non_neg_integer()) => [sense_candidate()]},
          lifg_opts: keyword() | nil,
          lifg_choices: list() | nil,
          acc_conflict: number() | nil,
          perception: map() | nil,

          # integration products
          atl_slate: map() | nil,
          comprehension: map() | nil,
          prefrontal: map() | nil,
          control_signals: map() | nil,
          activation_summary: map() | nil,
          evidence: map() | nil,
          episode: map() | nil,

          # planner / UI surface
          response_text: String.t() | nil,
          response_tone: atom() | nil,
          response_meta: map() | nil,
          symbolic_frame: map() | nil,
          selected_action: atom() | nil,
          action_candidates: list() | nil,
          action_meta: map() | nil,
          agency_decision: term() | nil,
          agency_commands: list() | nil,
          agency_command_results: list() | nil,

          # affect
          emotion: map() | nil,
          appraisal: map() | nil,
          mood: map() | nil,
          self_model: term() | nil,
          self_monitor: map() | nil,
          self_memory_recall: map() | nil,
          self_continuity: map() | nil,

          # misc products some stages attach
          mwe_matches: list() | nil,

          # execution/frame metadata
          session_id: term() | nil,
          frame: map() | nil,
          frame_ts_ms: integer() | nil,
          frame_seq: integer() | nil,
          frame_run_id: integer() | nil
        }

  defstruct sentence: nil,
            source: nil,
            tokens: [],
            phrase_candidates: [],
            active_cells: [],
            trace: [],
            fuzzy_corrections: [],
            fuzzy_aliases: [],
            fuzzy_confidence: 0.0,
            intent: nil,
            keyword: nil,
            confidence: nil,
            intent_bias: %{},
            token_cover: nil,
            resolved_tokens: nil,
            sense_candidates: %{},
            lifg_opts: nil,
            lifg_choices: nil,
            acc_conflict: nil,
            perception: nil,
            atl_slate: nil,
            comprehension: nil,
            prefrontal: nil,
            control_signals: nil,
            activation_summary: %{db_hits: MapSet.new()},
            evidence: nil,
            episode: nil,
            response_text: nil,
            response_tone: nil,
            response_meta: nil,
            symbolic_frame: nil,
            selected_action: nil,
            action_candidates: nil,
            action_meta: nil,
            agency_decision: nil,
            agency_commands: nil,
            agency_command_results: nil,
            emotion: nil,
            appraisal: nil,
            mood: nil,
            self_model: nil,
            self_monitor: nil,
            self_memory_recall: nil,
            self_continuity: nil,
            mwe_matches: nil,
            session_id: nil,
            frame: nil,
            frame_ts_ms: nil,
            frame_seq: nil,
            frame_run_id: nil

  @doc """
  Version for the `SemanticInput` carrier contract.

  Increment this when field names or accepted shapes change in a way that
  downstream apps or persisted fixtures need to notice.
  """
  @spec contract_version() :: pos_integer()
  def contract_version, do: @contract_v

  @doc """
  Returns the known SemanticInput fields, excluding `:__struct__`.
  """
  @spec contract_fields() :: [atom()]
  def contract_fields do
    %__MODULE__{}
    |> Map.from_struct()
    |> Map.keys()
    |> Enum.sort()
  end

  @doc """
  Normalize a map or `%SemanticInput{}` into the current contract.

  This accepts existing atom keys and known string keys. It never converts
  arbitrary strings into atoms.
  """
  @spec normalize_contract(map() | t()) :: {:ok, t()} | {:error, [term()]}
  def normalize_contract(%__MODULE__{} = si) do
    si
    |> Map.from_struct()
    |> normalize_contract()
  end

  def normalize_contract(%{} = map) do
    defaults = %__MODULE__{} |> Map.from_struct()

    normalized =
      contract_fields()
      |> Enum.reduce(%{}, fn field, acc ->
        Map.put(acc, field, map_get(map, field, Map.fetch!(defaults, field)))
      end)

    si = struct(__MODULE__, normalized)

    case validate_contract(si) do
      :ok -> {:ok, si}
      {:error, problems} -> {:error, problems}
    end
  end

  def normalize_contract(_), do: {:error, [semantic_input: :expected_map]}

  @doc """
  Validate the current SemanticInput contract without raising.
  """
  @spec validate_contract(map() | t()) :: :ok | {:error, [term()]}
  def validate_contract(%__MODULE__{} = si), do: validate_contract(Map.from_struct(si))

  def validate_contract(%{} = map) do
    problems =
      contract_fields()
      |> Enum.reduce([], fn field, acc ->
        value = map_get(map, field)

        case field_problem(field, value) do
          nil -> acc
          problem -> [{field, problem} | acc]
        end
      end)
      |> Enum.reverse()

    case problems do
      [] -> :ok
      _ -> {:error, problems}
    end
  end

  def validate_contract(_), do: {:error, [semantic_input: :expected_map]}

  @doc """
  Record scored sense candidates for a token into `si.sense_candidates`.

  - `token_index` — index of the token in `si.tokens`.
  - `scored` — list of `{id, score}` or `%{id: id, score: score}` or plain `id`.
  - `lemma` — token lemma (or downcased surface if you don't have a lemma).

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

  defp field_problem(:sentence, value), do: string_or_nil_problem(value)
  defp field_problem(:keyword, value), do: string_or_nil_problem(value)
  defp field_problem(:response_text, value), do: string_or_nil_problem(value)
  defp field_problem(:session_id, _value), do: nil
  defp field_problem(:self_model, _value), do: nil
  defp field_problem(:agency_decision, _value), do: nil

  defp field_problem(field, value) do
    cond do
      MapSet.member?(@list_fields, field) -> list_or_nil_problem(value)
      MapSet.member?(@map_fields, field) -> map_or_nil_problem(value)
      MapSet.member?(@atom_fields, field) -> atom_or_nil_problem(value)
      MapSet.member?(@number_fields, field) -> number_or_nil_problem(value)
      MapSet.member?(@integer_fields, field) -> integer_or_nil_problem(value)
      true -> nil
    end
  end

  defp string_or_nil_problem(value) when is_binary(value) or is_nil(value), do: nil
  defp string_or_nil_problem(_), do: :expected_string_or_nil

  defp list_or_nil_problem(value) when is_list(value) or is_nil(value), do: nil
  defp list_or_nil_problem(_), do: :expected_list_or_nil

  defp map_or_nil_problem(value) when is_map(value) or is_nil(value), do: nil
  defp map_or_nil_problem(_), do: :expected_map_or_nil

  defp atom_or_nil_problem(value) when is_atom(value) or is_nil(value), do: nil
  defp atom_or_nil_problem(_), do: :expected_atom_or_nil

  defp number_or_nil_problem(value) when is_number(value) or is_nil(value), do: nil
  defp number_or_nil_problem(_), do: :expected_number_or_nil

  defp integer_or_nil_problem(value) when is_integer(value) or is_nil(value), do: nil
  defp integer_or_nil_problem(_), do: :expected_integer_or_nil

  defp map_get(map, key, default \\ nil)

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default
end
