# apps/core/lib/core/semantic_input.ex
defmodule Core.SemanticInput do
  @moduledoc """
  Core pipeline carrier (SI). Minimal, unambiguous, but struct-stable.

  The Core pipeline frequently attaches additional keys (intent, evidence,
  LIFG/ATL products, activation summaries, etc.). Those keys should exist
  on the struct so Core can return %Core.SemanticInput{} without losing
  fields during coercion.

  Required fields (core invariants):
    - sentence: original input sentence
    - source: origin (:prod | :test | :user | etc.)
    - tokens: token list (maps/structs)
    - phrase_candidates: unconfirmed phrase windows
    - active_cells: lexicon/DB rows attached by downstream stages
    - neuron_activation: separate, live id-to-activation snapshot from STM
    - semantic_pulses: ephemeral session-scoped verb-lemma signals (not senses)
    - trace: ordered list of stage events

  Sentence signatures:
    - Produced by Core's grammatical recognition layer.
    - Describe grammatical constructions rather than final word senses.
    - Preserve competing constructions when input is ambiguous.
    - Are available to Brain.LIFG as candidate-specific evidence.
    - Do not modify BrainCell activation or cognitive state.

  Other pipeline products include:
    - fuzzy correction and aliases
    - intent and contextual information
    - token cover and multiword matches
    - sense candidates and LIFG choices
    - perception and episodic evidence
    - comprehension and response planning
    - mood, emotion, appraisal and self-model
    - execution/frame metadata
  """

  @type token :: term()
  @type cell :: term()

  # Version 4 adds independently audited post-ATL LIFG feedback.
  @contract_v 4

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
                 :semantic_choices,
                 :action_candidates,
                 :agency_commands,
                 :agency_command_results,
                 :mwe_matches,
                 :mwe_lexicon,
                 :mwe_hypotheses,
                 :mwe_choices,
                 :lexical_constructions,
                 :episodic_candidates,
                 :sentence_signatures
               ])

  @map_fields MapSet.new([
                :intent_bias,
                :sense_candidates,
                :sentence_competition,
                :role_competition,
                :sentence_meaning,
                :lifg_feedback,
                :perception,
                :atl_slate,
                :comprehension,
                :prefrontal,
                :control_signals,
                :activation_summary,
                :neuron_activation,
                :semantic_pulses,
                :evidence,
                :episode,
                :response_meta,
                :symbolic_frame,
                :context_frame,
                :discourse_reference,
                :wm_context,
                :action_meta,
                :emotion,
                :appraisal,
                :mood,
                :self_monitor,
                :self_memory_recall,
                :self_continuity,
                :frame
              ])

  @atom_fields MapSet.new([
                 :source,
                 :intent,
                 :opener_intent,
                 :conversation_act,
                 :topic_domain,
                 :response_tone,
                 :selected_action
               ])

  @number_fields MapSet.new([
                   :fuzzy_confidence,
                   :confidence,
                   :acc_conflict
                 ])

  @integer_fields MapSet.new([
                    :frame_ts_ms,
                    :frame_seq,
                    :frame_run_id
                  ])

  @typedoc """
  A scored dictionary sense candidate associated with a token.
  """
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

  @typedoc """
  A grammatical construction recognized by Core.

  This is an evidence record, not a semantic winner.

  Fields:
    - id: grammatical construction identifier
    - span: inclusive zero-based token-index range
    - roles: grammatical roles mapped to token indices
    - required_features: morphological constraints for role matching
    - evidence: observations supporting the construction
    - source: component producing the evidence

  A single sentence may produce multiple competing signatures.

  Core.Syntax is responsible for validating individual signatures.
  SemanticInput validates the outer field as a list.
  """
  @type sentence_signature :: %{
          required(:id) => atom(),
          required(:span) => {
            non_neg_integer(),
            non_neg_integer()
          },
          required(:roles) => %{
            optional(atom()) => non_neg_integer()
          },
          optional(:required_features) => %{
            optional(atom()) => [atom()]
          },
          optional(:evidence) => [atom()],
          optional(:source) => atom()
        }

  @type t :: %__MODULE__{
          # Core invariants
          sentence: String.t() | nil,
          source: atom() | nil,
          tokens: [token()],
          phrase_candidates: [map()],
          active_cells: [cell()],
          neuron_activation: %{optional(String.t()) => number()},
          semantic_pulses: %{optional(String.t()) => number()},
          trace: list(),

          # Fuzzy correction
          fuzzy_corrections: [map()] | nil,
          fuzzy_aliases: [atom()] | nil,
          fuzzy_confidence: number() | nil,

          # Intent and context
          intent: atom() | nil,
          keyword: String.t() | nil,
          confidence: number() | nil,
          opener_intent: atom() | nil,
          opener_text: String.t() | nil,
          primary_text: String.t() | nil,
          conversation_act: atom() | nil,
          topic_domain: atom() | nil,
          context_frame: map() | nil,
          discourse_reference: map() | nil,
          wm_context: map() | nil,
          intent_bias: map(),
          token_cover: list() | nil,
          resolved_tokens: list() | nil,

          # Grammatical evidence and sense candidates
          sentence_signatures: [sentence_signature()] | nil,
          sentence_competition: map() | nil,
          role_competition: map() | nil,
          sentence_meaning: map() | nil,
          episodic_candidates: [map()],
          lifg_feedback: map() | nil,
          sense_candidates: %{
            optional(non_neg_integer()) => [sense_candidate()]
          },

          # LIFG
          lifg_opts: keyword() | nil,
          lifg_choices: list() | nil,
          semantic_choices: list() | nil,
          acc_conflict: number() | nil,
          perception: map() | nil,

          # Integration products
          atl_slate: map() | nil,
          comprehension: map() | nil,
          prefrontal: map() | nil,
          control_signals: map() | nil,
          activation_summary: map() | nil,
          evidence: map() | nil,
          episode: map() | nil,

          # Planner and response
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

          # Affect and self-state
          emotion: map() | nil,
          appraisal: map() | nil,
          mood: map() | nil,
          self_model: term() | nil,
          self_monitor: map() | nil,
          self_memory_recall: map() | nil,
          self_continuity: map() | nil,

          # Multiword expressions
          mwe_matches: list() | nil,
          mwe_lexicon: list(),
          mwe_hypotheses: list(),
          mwe_choices: list(),
          lexical_constructions: list(),

          # Execution/frame metadata
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
            neuron_activation: %{},
            semantic_pulses: %{},
            trace: [],
            fuzzy_corrections: [],
            fuzzy_aliases: [],
            fuzzy_confidence: 0.0,
            intent: nil,
            keyword: nil,
            confidence: nil,
            opener_intent: nil,
            opener_text: nil,
            primary_text: nil,
            conversation_act: nil,
            topic_domain: nil,
            context_frame: nil,
            discourse_reference: nil,
            wm_context: nil,
            intent_bias: %{},
            token_cover: nil,
            resolved_tokens: [],
            sentence_signatures: [],
            sentence_competition: nil,
            role_competition: nil,
            sentence_meaning: nil,
            episodic_candidates: [],
            lifg_feedback: nil,
            sense_candidates: %{},
            lifg_opts: nil,
            lifg_choices: nil,
            semantic_choices: nil,
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
            mwe_lexicon: [],
            mwe_hypotheses: [],
            mwe_choices: [],
            lexical_constructions: [],
            session_id: nil,
            frame: nil,
            frame_ts_ms: nil,
            frame_seq: nil,
            frame_run_id: nil

  @doc """
  Version of the SemanticInput carrier contract.

  Increment when field names or accepted shapes change in ways that
  downstream applications or persisted fixtures must recognize.
  """
  @spec contract_version() :: pos_integer()
  def contract_version, do: @contract_v

  @doc """
  Returns the known SemanticInput fields, excluding :__struct__.
  """
  @spec contract_fields() :: [atom()]
  def contract_fields do
    %__MODULE__{}
    |> Map.from_struct()
    |> Map.keys()
    |> Enum.sort()
  end

  @doc """
  Normalize a map or SemanticInput into the current contract.

  Accepts known atom keys and known string keys.
  Never converts arbitrary input strings into atoms.
  """
  @spec normalize_contract(map() | t()) ::
          {:ok, t()} | {:error, [term()]}

  def normalize_contract(%__MODULE__{} = si) do
    si
    |> Map.from_struct()
    |> normalize_contract()
  end

  def normalize_contract(%{} = map) do
    defaults =
      %__MODULE__{}
      |> Map.from_struct()

    normalized =
      contract_fields()
      |> Enum.reduce(%{}, fn field, acc ->
        value =
          map_get(
            map,
            field,
            Map.fetch!(defaults, field)
          )

        Map.put(acc, field, value)
      end)

    si = struct(__MODULE__, normalized)

    case validate_contract(si) do
      :ok ->
        {:ok, si}

      {:error, problems} ->
        {:error, problems}
    end
  end

  def normalize_contract(_) do
    {:error, [semantic_input: :expected_map]}
  end

  @doc """
  Validate the SemanticInput contract without raising.

  The sentence_signatures field is validated as a list.
  Detailed grammatical evidence validation belongs to Core.Syntax.
  """
  @spec validate_contract(map() | t()) ::
          :ok | {:error, [term()]}

  def validate_contract(%__MODULE__{} = si) do
    si
    |> Map.from_struct()
    |> validate_contract()
  end

  def validate_contract(%{} = map) do
    problems =
      contract_fields()
      |> Enum.reduce([], fn field, acc ->
        value = map_get(map, field)

        case field_problem(field, value) do
          nil ->
            acc

          problem ->
            [{field, problem} | acc]
        end
      end)
      |> Enum.reverse()

    case problems do
      [] -> :ok
      _ -> {:error, problems}
    end
  end

  def validate_contract(_) do
    {:error, [semantic_input: :expected_map]}
  end

  @doc """
  Record scored sense candidates for a token.

  Parameters:
    - token_index: index into si.tokens
    - scored: {id, score}, %{id, score}, or plain IDs
    - lemma: normalized token lemma

  Options:
    - margin: include near-winners (default 0.15)
    - top_k: maximum candidates per token (default 4)
    - min_score: optional minimum score

  This function preserves existing candidate-ranking behavior.
  """
  @spec emit_sense_candidates(
          map(),
          non_neg_integer(),
          list(),
          String.t(),
          keyword()
        ) :: map()

  def emit_sense_candidates(
        %{} = si,
        token_index,
        scored,
        lemma,
        opts \\ []
      ) do
    margin = Keyword.get(opts, :margin, 0.15)
    top_k = Keyword.get(opts, :top_k, 4)
    min_score = Keyword.get(opts, :min_score, nil)

    list =
      scored
      |> Enum.map(fn
        {id, score} ->
          %{id: id, score: score, lemma: lemma}

        %{id: id, score: score} ->
          %{id: id, score: score, lemma: lemma}

        id when is_binary(id) ->
          %{id: id, score: 0.0, lemma: lemma}
      end)

    max_score =
      case Enum.max_by(
             list,
             & &1.score,
             fn -> %{score: -1.0e9} end
           ) do
        %{score: score} -> score
      end

    filtered =
      list
      |> Enum.filter(fn %{score: score} ->
        (is_nil(min_score) or score >= min_score) and
          score >= max_score - margin
      end)
      |> Enum.sort_by(& &1.score, :desc)
      |> Enum.take(top_k)
      |> Enum.reduce(%{}, fn %{id: id} = candidate, acc ->
        case acc do
          %{^id => existing} ->
            if candidate.score > existing.score do
              Map.put(acc, id, candidate)
            else
              acc
            end

          _ ->
            Map.put(acc, id, candidate)
        end
      end)
      |> Map.values()
      |> Enum.sort_by(& &1.score, :desc)

    merged_per_idx =
      si
      |> Map.get(:sense_candidates, %{})
      |> Map.update(token_index, filtered, fn existing ->
        (existing ++ filtered)
        |> Enum.reduce(%{}, fn %{id: id} = candidate, acc ->
          case acc do
            %{^id => previous} ->
              if candidate.score > previous.score do
                Map.put(acc, id, candidate)
              else
                acc
              end

            _ ->
              Map.put(acc, id, candidate)
          end
        end)
        |> Map.values()
        |> Enum.sort_by(& &1.score, :desc)
        |> Enum.take(top_k)
      end)

    Map.put(si, :sense_candidates, merged_per_idx)
  end

  @doc """
  Return all sense candidates or the candidates for one token index.
  """
  @spec get_sense_candidates(
          map(),
          non_neg_integer() | :all
        ) :: list() | map()

  def get_sense_candidates(%{} = si, idx \\ :all) do
    candidates = Map.get(si, :sense_candidates, %{})

    case idx do
      :all ->
        candidates

      _ ->
        Map.get(candidates, idx, [])
    end
  end

  # ------------------------------------------------------------------
  # Contract validation
  # ------------------------------------------------------------------

  defp field_problem(:sentence, value),
    do: string_or_nil_problem(value)

  defp field_problem(:keyword, value),
    do: string_or_nil_problem(value)

  defp field_problem(:opener_text, value),
    do: string_or_nil_problem(value)

  defp field_problem(:primary_text, value),
    do: string_or_nil_problem(value)

  defp field_problem(:response_text, value),
    do: string_or_nil_problem(value)

  defp field_problem(:session_id, _value), do: nil
  defp field_problem(:self_model, _value), do: nil
  defp field_problem(:agency_decision, _value), do: nil

  defp field_problem(field, value) do
    cond do
      MapSet.member?(@list_fields, field) ->
        list_or_nil_problem(value)

      MapSet.member?(@map_fields, field) ->
        map_or_nil_problem(value)

      MapSet.member?(@atom_fields, field) ->
        atom_or_nil_problem(value)

      MapSet.member?(@number_fields, field) ->
        number_or_nil_problem(value)

      MapSet.member?(@integer_fields, field) ->
        integer_or_nil_problem(value)

      true ->
        nil
    end
  end

  defp string_or_nil_problem(value)
       when is_binary(value) or is_nil(value),
       do: nil

  defp string_or_nil_problem(_),
    do: :expected_string_or_nil

  defp list_or_nil_problem(value)
       when is_list(value) or is_nil(value),
       do: nil

  defp list_or_nil_problem(_),
    do: :expected_list_or_nil

  defp map_or_nil_problem(value)
       when is_map(value),
       do: nil

  defp map_or_nil_problem(nil), do: nil

  defp map_or_nil_problem(_),
    do: :expected_map_or_nil

  defp atom_or_nil_problem(value)
       when is_atom(value),
       do: nil

  defp atom_or_nil_problem(_),
    do: :expected_atom_or_nil

  defp number_or_nil_problem(value)
       when is_number(value) or is_nil(value),
       do: nil

  defp number_or_nil_problem(_),
    do: :expected_number_or_nil

  defp integer_or_nil_problem(value)
       when is_integer(value) or is_nil(value),
       do: nil

  defp integer_or_nil_problem(_),
    do: :expected_integer_or_nil

  # ------------------------------------------------------------------
  # Safe field access
  # ------------------------------------------------------------------

  defp map_get(map, key, default \\ nil)

  defp map_get(%{} = map, key, default)
       when is_atom(key) do
    Map.get(
      map,
      key,
      Map.get(map, Atom.to_string(key), default)
    )
  end

  defp map_get(_map, _key, default), do: default
end
