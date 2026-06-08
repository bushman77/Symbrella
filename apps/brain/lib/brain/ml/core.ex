defmodule Brain.ML.Core do
  @moduledoc """
  Pure functional layer for ML turn records, schemas, and feature extraction.

  This module owns only stable data contracts and deterministic transformations.
  It must not own processes, PubSub, persistence, Phoenix rendering, Axon training,
  or Nx execution.
  """

  @turn_schema_v 1
  @feature_schema_v 1

  @feature_names [
    :text_chars,
    :text_tokens,
    :intent_confidence,
    :mood_valence,
    :mood_arousal,
    :mood_vigilance,
    :mood_plasticity,
    :wm_items_count,
    :lifg_winners_count,
    :lifg_mean_score,
    :lifg_mean_margin,
    :lifg_guard_drop_count,
    :trigger_latency_ms,
    :response_chars
  ]

  @type turn_id :: integer() | binary()

  @type turn_record :: %{
          required(:v) => pos_integer(),
          required(:turn_id) => turn_id() | nil,
          required(:opened_at_ms) => integer() | nil,
          required(:at_ms) => integer() | nil,
          required(:text) => String.t() | nil,
          required(:intent) => map() | nil,
          required(:mood) => map() | nil,
          required(:wm) => term(),
          required(:lifg) => map(),
          required(:trigger) => map()
        }

  @type feature_name :: unquote(Enum.reduce(@feature_names, &{:|, [], [&1, &2]}))
  @type features :: %{optional(feature_name()) => number()}

  @doc "Version of the stable ML turn-record map schema."
  @spec turn_schema_v() :: pos_integer()
  def turn_schema_v, do: @turn_schema_v

  @doc "Version of the stable ML feature extraction schema."
  @spec feature_schema_v() :: pos_integer()
  def feature_schema_v, do: @feature_schema_v

  @doc "Stable feature order for consumers that later build tensors outside this module."
  @spec feature_names() :: [feature_name()]
  def feature_names, do: @feature_names

  @doc """
  Builds a normalized turn record from a map or keyword list.

  Callers provide runtime facts such as `:turn_id`, timestamps, and payloads.
  Missing optional fields are normalized to stable defaults.
  """
  @spec build_turn_record(map() | keyword()) :: turn_record()
  def build_turn_record(attrs) when is_list(attrs), do: attrs |> Map.new() |> build_turn_record()

  def build_turn_record(%{} = attrs) do
    lifg = normalize_lifg(mget(attrs, :lifg))

    %{
      v: positive_integer(mget(attrs, :v), @turn_schema_v),
      turn_id: mget(attrs, :turn_id),
      opened_at_ms: integer_or_nil(mget(attrs, :opened_at_ms)),
      at_ms: integer_or_nil(mget(attrs, :at_ms)),
      text: text_or_nil(mget(attrs, :text)),
      clock: mget(attrs, :clock),
      intent: map_or_nil(mget(attrs, :intent)),
      mood: map_or_nil(mget(attrs, :mood)),
      wm: mget(attrs, :wm),
      lifg: lifg,
      response: map_or_nil(mget(attrs, :response)),
      trigger: normalize_trigger(mget(attrs, :trigger))
    }
    |> drop_keys_when_nil([:response])
  end

  @doc "Extracts best-effort turn text from intent and blackboard-style payloads."
  @spec text_from(term(), term()) :: String.t() | nil
  def text_from(intent_payload, bb_payload) do
    text =
      mget(intent_payload || %{}, :text) ||
        mget(intent_payload || %{}, :sentence) ||
        mget(bb_payload || %{}, :text)

    text_or_nil(text)
  end

  @doc "Builds the normalized trigger block for a blackboard telemetry event."
  @spec trigger_from_blackboard(term(), integer()) :: map()
  def trigger_from_blackboard(%{} = bb_env, now_ms) when is_integer(now_ms) do
    normalize_trigger(%{
      kind: mget(bb_env, :kind),
      event: mget(bb_env, :event),
      measurements: mget(bb_env, :measurements) || %{},
      meta: mget(bb_env, :meta) || %{},
      at_ms: mget(bb_env, :at_ms) || now_ms
    })
  end

  def trigger_from_blackboard(_, now_ms) when is_integer(now_ms) do
    normalize_trigger(%{at_ms: now_ms})
  end

  @doc """
  Projects a LIFG Stage-1 pipeline stop blackboard envelope into LIFG state.

  The GenServer uses this instead of sampling latest LIFG state so turn records
  stay tied to the stop event that closed the turn.
  """
  @spec lifg_state_from_pipeline_stop(term()) :: map() | nil
  def lifg_state_from_pipeline_stop(%{} = bb_env) do
    meta = mget(bb_env, :meta) || %{}

    %{
      region: :lifg,
      last: %{
        meta: meta,
        tokens: mget(meta, :tokens) || [],
        source: mget(meta, :source),
        guards: %{
          chargram_violation: mget(meta, :chargram_violation),
          missing_candidate_tokens: mget(meta, :missing_candidate_tokens) || [],
          missing_candidates: mget(meta, :missing_candidates),
          rejected_by_boundary: mget(meta, :rejected_by_boundary) || []
        },
        intent: mget(meta, :intent),
        confidence: mget(meta, :confidence),
        feature_mix: :lifg_stage1,
        ts_ms: mget(meta, :ts_ms),
        choices: mget(meta, :choices) || [],
        audit: %{
          boundary_drops: mget(meta, :boundary_drops),
          chargram_violation: mget(meta, :chargram_violation),
          dropped_tokens: mget(meta, :dropped_tokens),
          kept_tokens: mget(meta, :kept_tokens),
          missing_candidate_tokens: mget(meta, :missing_candidate_tokens) || [],
          missing_candidates: mget(meta, :missing_candidates),
          rejected_by_boundary: mget(meta, :rejected_by_boundary) || [],
          weak_decisions: mget(meta, :weak_decisions),
          guard_drops: mget(meta, :guard_drops),
          mwe_fallbacks: mget(meta, :mwe_fallbacks)
        },
        finalists: mget(meta, :finalists) || [],
        si_sentence: mget(meta, :sentence)
      }
    }
  end

  def lifg_state_from_pipeline_stop(_), do: nil

  @doc """
  Validates the minimum contract for a turn record.

  Returns `:ok` or `{:error, reasons}` where reasons are atoms suitable for tests
  and caller-side logging.
  """
  @spec validate_turn_record(term()) :: :ok | {:error, [atom()]}
  def validate_turn_record(%{} = turn) do
    reasons =
      []
      |> require_key(turn, :v)
      |> require_key(turn, :turn_id)
      |> require_key(turn, :opened_at_ms)
      |> require_key(turn, :at_ms)
      |> require_key(turn, :lifg)
      |> require_key(turn, :trigger)
      |> require_version(turn)
      |> require_non_negative_timestamp(turn, :opened_at_ms)
      |> require_non_negative_timestamp(turn, :at_ms)

    case reasons do
      [] -> :ok
      _ -> {:error, Enum.reverse(reasons)}
    end
  end

  def validate_turn_record(_), do: {:error, [:not_a_map]}

  @doc "Predicate wrapper around `validate_turn_record/1`."
  @spec valid_turn_record?(term()) :: boolean()
  def valid_turn_record?(turn), do: validate_turn_record(turn) == :ok

  @doc """
  Extracts a stable feature map from a normalized or raw turn record.

  Extraction is total and deterministic: unknown or malformed values become
  neutral numeric defaults.
  """
  @spec extract_features(map() | keyword()) :: features()
  def extract_features(turn) when is_list(turn), do: turn |> Map.new() |> extract_features()

  def extract_features(%{} = turn) do
    record = build_turn_record(turn)
    lifg = mget(record, :lifg) || %{}
    trigger = mget(record, :trigger) || %{}
    response = mget(record, :response) || %{}

    %{
      text_chars: text_chars(mget(record, :text)),
      text_tokens: text_tokens(mget(record, :text)),
      intent_confidence: number(mget(record.intent || %{}, :confidence)),
      mood_valence: mood_value(record.mood, :valence),
      mood_arousal: mood_value(record.mood, :arousal),
      mood_vigilance: mood_value(record.mood, :vigilance),
      mood_plasticity: mood_value(record.mood, :plasticity),
      wm_items_count: wm_items_count(record.wm),
      lifg_winners_count: length(mget(lifg, :winners) || []),
      lifg_mean_score: mean_numeric(mget(lifg, :winners), :score),
      lifg_mean_margin: mean_numeric(mget(lifg, :winners), :margin),
      lifg_guard_drop_count: lifg_guard_drop_count(lifg),
      trigger_latency_ms:
        trigger_latency(record.opened_at_ms, mget(trigger, :at_ms) || record.at_ms),
      response_chars: text_chars(mget(response, :assistant_text))
    }
  end

  def extract_features(_), do: empty_features()

  @doc "Returns a feature row in `feature_names/0` order."
  @spec feature_row(map() | keyword()) :: [number()]
  def feature_row(turn) do
    features = extract_features(turn)
    Enum.map(@feature_names, &Map.get(features, &1, 0.0))
  end

  @doc """
  Builds a symbolic feature batch without constructing Nx tensors.

  Tensor creation and execution belong outside this pure core layer.
  """
  @spec feature_batch([map() | keyword()]) :: %{
          feature_schema_v: pos_integer(),
          feature_names: [feature_name()],
          x: [[number()]]
        }
  def feature_batch(turns) when is_list(turns) do
    %{
      feature_schema_v: @feature_schema_v,
      feature_names: @feature_names,
      x: Enum.map(turns, &feature_row/1)
    }
  end

  @doc """
  Normalizes LIFG state into the explainability block expected by turn records.

  This function is deliberately data-only. It does not hydrate lexicon rows from
  persistence.
  """
  @spec normalize_lifg(term()) :: map()
  def normalize_lifg(nil), do: %{last_update: nil, winners: []}

  def normalize_lifg(%{} = lifg) when is_map_key(lifg, :last_update) do
    %{
      last_update: mget(lifg, :last_update),
      winners: lifg |> mget(:winners) |> List.wrap() |> Enum.filter(&is_map/1)
    }
  end

  def normalize_lifg(%{} = lifg) do
    {choices, token_meta} = extract_choices_and_tokens(lifg)

    %{
      last_update: lifg,
      winners: Enum.map(choices, &normalize_choice(&1, token_meta))
    }
  end

  def normalize_lifg(_), do: %{last_update: nil, winners: []}

  @doc "Builds the normalized response block for a response-complete blackboard event."
  @spec response_from_complete(term(), integer()) :: map() | nil
  def response_from_complete(%{} = bb_env, now_ms) when is_integer(now_ms) do
    meta = mget(bb_env, :meta) || %{}
    measurements = mget(bb_env, :measurements) || %{}

    normalize_response(%{
      assistant_text: mget(meta, :assistant_text),
      assistant_chars: mget(measurements, :assistant_chars),
      user_chars: mget(measurements, :user_chars),
      tone: mget(meta, :tone),
      mode: mget(meta, :mode),
      response_profile: mget(meta, :response_profile),
      prompt_response_profile: mget(meta, :prompt_response_profile),
      simulated_affect: mget(meta, :simulated_affect),
      personality_state: mget(meta, :personality_state),
      reflection: mget(meta, :reflection),
      system_sha256: mget(meta, :system_sha256),
      symbolic_frame: mget(meta, :symbolic_frame),
      metadata: meta,
      at_ms: mget(bb_env, :at_ms) || now_ms
    })
  end

  def response_from_complete(_, _), do: nil

  @doc "Normalizes response-complete metadata into the turn-record response block."
  @spec normalize_response(term()) :: map() | nil
  def normalize_response(%{} = response) do
    response
    |> take_known_or_original([
      :assistant_text,
      :assistant_chars,
      :user_chars,
      :tone,
      :mode,
      :response_profile,
      :prompt_response_profile,
      :simulated_affect,
      :personality_state,
      :reflection,
      :system_sha256,
      :symbolic_frame,
      :metadata,
      :at_ms
    ])
    |> drop_nil_values()
  end

  def normalize_response(_), do: nil

  @doc "Replaces an existing turn with the same turn id, or prepends the new record."
  @spec replace_or_prepend(term(), map(), pos_integer()) :: [map()]
  def replace_or_prepend(turns, %{turn_id: turn_id} = rec, keep) when is_list(turns) do
    keep = max(positive_integer(keep, 1), 1)

    {found?, reversed} =
      Enum.reduce(turns, {false, []}, fn turn, {found?, acc} ->
        if is_map(turn) and mget(turn, :turn_id) == turn_id do
          {true, [rec | acc]}
        else
          {found?, [turn | acc]}
        end
      end)

    updated = Enum.reverse(reversed)

    if found?, do: Enum.take(updated, keep), else: [rec | turns] |> Enum.take(keep)
  end

  def replace_or_prepend(_turns, rec, keep) do
    [rec] |> Enum.take(max(positive_integer(keep, 1), 1))
  end

  defp normalize_trigger(%{} = trigger) do
    %{
      kind: mget(trigger, :kind),
      event: mget(trigger, :event),
      measurements: mget(trigger, :measurements) || %{},
      meta: mget(trigger, :meta) || %{},
      at_ms: integer_or_nil(mget(trigger, :at_ms))
    }
  end

  defp normalize_trigger(_),
    do: %{kind: nil, event: nil, measurements: %{}, meta: %{}, at_ms: nil}

  defp extract_choices_and_tokens(%{} = lifg) do
    last =
      mget(lifg, :last) ||
        mget_in(lifg, [:state, :last]) ||
        mget_in(lifg, [:lifg, :last]) ||
        mget_in(lifg, [:out, :last])

    tokens =
      (mget(lifg, :tokens) ||
         mget_in(lifg, [:state, :tokens]) ||
         (is_map(last) && mget(last, :tokens)) ||
         [])
      |> List.wrap()
      |> Enum.filter(&is_map/1)

    token_meta =
      Enum.reduce(tokens, %{}, fn token, acc ->
        idx = mget(token, :index) || mget(token, :token_index)
        if is_integer(idx), do: Map.put(acc, idx, token), else: acc
      end)

    choices =
      (mget(lifg, :choices) ||
         mget(lifg, :winners) ||
         mget_in(lifg, [:out, :choices]) ||
         mget_in(lifg, [:lifg, :choices]) ||
         (is_map(last) && mget(last, :choices)) ||
         [])
      |> List.wrap()
      |> Enum.filter(&is_map/1)

    {choices, token_meta}
  end

  defp normalize_choice(%{} = choice, token_meta) do
    id = mget(choice, :chosen_id) || mget(choice, :id)
    token_index = mget(choice, :token_index)
    token = if is_integer(token_index), do: Map.get(token_meta, token_index), else: nil
    {chosen_word, chosen_pos, chosen_sense} = parse_cell_id(id)

    %{
      token_index: token_index,
      phrase: token && (mget(token, :phrase) || mget(token, :raw) || mget(token, :text)),
      span: token && (mget(token, :span) || mget(token, :range)),
      mw: token && (mget(token, :mw) || mget(token, :multiword)),
      lemma: mget(choice, :lemma),
      chosen_id: id,
      chosen_word: chosen_word,
      chosen_pos: chosen_pos,
      chosen_sense: chosen_sense,
      margin: number_or_nil(mget(choice, :margin)),
      score: number_or_nil(mget(choice, :score) || mget(choice, :prob)),
      scores: mget(choice, :scores),
      alt_ids: choice |> mget(:alt_ids) |> List.wrap() |> Enum.take(12)
    }
    |> drop_nil_values()
  end

  defp parse_cell_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [word, pos, sense] -> {word, pos, sense}
      [word, pos] -> {word, pos, nil}
      [word] -> {word, nil, nil}
    end
  end

  defp parse_cell_id(_), do: {nil, nil, nil}

  defp mood_value(%{} = mood, key) do
    derived = mget(mood, :derived) || %{}
    nested = mget(mood, :mood) || %{}

    number(
      mget(derived, key) ||
        mget(nested, key) ||
        mget(mood, key)
    )
  end

  defp mood_value(_, _), do: 0.0

  defp wm_items_count(%{} = wm) do
    candidates = [
      mget(wm, :items),
      mget(wm, :entries),
      mget(wm, :focus),
      mget(wm, :slots)
    ]

    case Enum.find(candidates, &is_list/1) do
      nil -> 0
      items -> length(items)
    end
  end

  defp wm_items_count(items) when is_list(items), do: length(items)
  defp wm_items_count(_), do: 0

  defp lifg_guard_drop_count(%{} = lifg) do
    last = mget(lifg, :last_update) || %{}
    guards = mget(last, :guards) || mget_in(last, [:last, :guards]) || %{}
    audit = mget(last, :audit) || mget_in(last, [:last, :audit]) || %{}

    [
      mget(guards, :missing_candidate_tokens),
      mget(guards, :rejected_by_boundary),
      mget(audit, :boundary_drops),
      mget(audit, :dropped_tokens),
      mget(audit, :guard_drops)
    ]
    |> Enum.map(&count_any/1)
    |> Enum.sum()
  end

  defp lifg_guard_drop_count(_), do: 0

  defp mean_numeric(items, key) when is_list(items) do
    nums =
      items
      |> Enum.map(fn item -> if is_map(item), do: number_or_nil(mget(item, key)), else: nil end)
      |> Enum.reject(&is_nil/1)

    case nums do
      [] -> 0.0
      _ -> Enum.sum(nums) / length(nums)
    end
  end

  defp mean_numeric(_, _), do: 0.0

  defp trigger_latency(opened_at_ms, at_ms) when is_integer(opened_at_ms) and is_integer(at_ms) do
    max(at_ms - opened_at_ms, 0) * 1.0
  end

  defp trigger_latency(_, _), do: 0.0

  defp text_chars(text) when is_binary(text), do: String.length(text)
  defp text_chars(_), do: 0

  defp text_tokens(text) when is_binary(text) do
    text
    |> String.split(~r/\s+/, trim: true)
    |> length()
  end

  defp text_tokens(_), do: 0

  defp count_any(value) when is_list(value), do: length(value)
  defp count_any(value) when is_integer(value), do: max(value, 0)
  defp count_any(%{} = value), do: map_size(value)
  defp count_any(true), do: 1
  defp count_any(_), do: 0

  defp empty_features do
    @feature_names
    |> Enum.map(&{&1, 0.0})
    |> Enum.into(%{})
  end

  defp take_known_or_original(map, keys) do
    known =
      keys
      |> Enum.reduce(%{}, fn key, acc ->
        Map.put(acc, key, mget(map, key))
      end)
      |> drop_nil_values()

    if map_size(known) == 0, do: map, else: known
  end

  defp require_key(reasons, map, key) do
    if Map.has_key?(map, key) or Map.has_key?(map, to_string(key)) do
      reasons
    else
      [missing_reason(key) | reasons]
    end
  end

  defp require_version(reasons, map) do
    if positive_integer(mget(map, :v), nil) == @turn_schema_v do
      reasons
    else
      [:unsupported_turn_schema_v | reasons]
    end
  end

  defp require_non_negative_timestamp(reasons, map, key) do
    value = mget(map, key)

    if is_integer(value) and value >= 0 do
      reasons
    else
      [invalid_timestamp_reason(key) | reasons]
    end
  end

  defp missing_reason(:v), do: :missing_v
  defp missing_reason(:turn_id), do: :missing_turn_id
  defp missing_reason(:opened_at_ms), do: :missing_opened_at_ms
  defp missing_reason(:at_ms), do: :missing_at_ms
  defp missing_reason(:lifg), do: :missing_lifg
  defp missing_reason(:trigger), do: :missing_trigger
  defp missing_reason(_), do: :missing_required_key

  defp invalid_timestamp_reason(:opened_at_ms), do: :invalid_opened_at_ms
  defp invalid_timestamp_reason(:at_ms), do: :invalid_at_ms
  defp invalid_timestamp_reason(_), do: :invalid_timestamp

  defp positive_integer(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_integer(_, default), do: default

  defp integer_or_nil(value) when is_integer(value), do: value
  defp integer_or_nil(_), do: nil

  defp map_or_nil(%{} = value), do: value
  defp map_or_nil(_), do: nil

  defp text_or_nil(value) when is_binary(value) and value != "", do: value
  defp text_or_nil(_), do: nil

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp number_or_nil(value) when is_integer(value), do: value * 1.0
  defp number_or_nil(value) when is_float(value), do: value
  defp number_or_nil(_), do: nil

  defp drop_nil_values(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Enum.into(%{})
  end

  defp drop_keys_when_nil(map, keys) when is_list(keys) do
    map
    |> Enum.reject(fn {key, value} -> is_nil(value) and key in keys end)
    |> Enum.into(%{})
  end

  defp mget(%{} = map, key) do
    cond do
      Map.has_key?(map, key) -> Map.get(map, key)
      is_atom(key) and Map.has_key?(map, Atom.to_string(key)) -> Map.get(map, Atom.to_string(key))
      true -> nil
    end
  end

  defp mget(_, _), do: nil

  defp mget_in(value, []) when is_map(value), do: value

  defp mget_in(value, [key | rest]) when is_map(value) do
    case mget(value, key) do
      %{} = next -> mget_in(next, rest)
      other when rest == [] -> other
      _ -> nil
    end
  end

  defp mget_in(_, _), do: nil
end
