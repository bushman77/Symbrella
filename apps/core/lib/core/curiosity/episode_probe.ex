defmodule Core.Curiosity.EpisodeProbe do
  @moduledoc """
  Builds occasional clarification questions from recent hippocampal episodes.

  This module is deliberately stateless from OTP's perspective. It keeps only a
  tiny per-session throttle in `:persistent_term`, and derives each question from
  the current SI evidence or, as a fallback, the live Hippocampus window.
  """

  @state_key {__MODULE__, :state}

  @defaults [
    enabled?: true,
    every_turns: 2,
    min_gap_ms: 15_000,
    max_vigilance: 0.75,
    min_uncertainty: 0.2,
    max_recent: 40,
    asked_keep: 40
  ]

  @spec reset() :: :ok
  def reset do
    :persistent_term.erase(@state_key)
    :ok
  rescue
    _ -> :ok
  end

  @spec maybe_question(map(), map(), keyword()) :: {:ok, String.t(), map()} | :none
  def maybe_question(si, mood \\ %{}, opts \\ [])

  def maybe_question(%{} = si, mood, opts) when is_map(mood) and is_list(opts) do
    cfg = config(opts)
    session_id = session_id(si)
    now_ms = System.system_time(:millisecond)

    {session_state, all_state} = next_session_state(session_id)

    result =
      cond do
        not Keyword.get(cfg, :enabled?, true) ->
          :none

        not turn_due?(session_state, cfg) ->
          :none

        not gap_due?(session_state, now_ms, cfg) ->
          :none

        unsafe_pressure?(si, mood, cfg) ->
          :none

        true ->
          si
          |> recent_episode_results(cfg)
          |> choose_candidate(session_state, cfg)
          |> question_result()
      end

    session_state = update_session_state(session_state, result, now_ms, cfg)
    put_state(Map.put(all_state, session_id, session_state))
    result
  end

  def maybe_question(_si, _mood, _opts), do: :none

  @spec build_question(map()) :: {:ok, String.t(), map()} | :none
  def build_question(%{} = candidate), do: question_result(candidate)
  def build_question(_), do: :none

  defp config(opts) do
    app_cfg = Application.get_env(:core, __MODULE__, [])

    @defaults
    |> Keyword.merge(if(is_list(app_cfg), do: app_cfg, else: []))
    |> Keyword.merge(opts)
  end

  defp next_session_state(session_id) do
    all_state = :persistent_term.get(@state_key, %{})

    session_state =
      all_state
      |> Map.get(session_id, %{})
      |> Map.update(:turns, 1, &(&1 + 1))
      |> Map.put_new(:last_asked_ms, 0)
      |> Map.put_new(:asked, [])

    {session_state, all_state}
  end

  defp put_state(state) when is_map(state) do
    :persistent_term.put(@state_key, state)
  end

  defp turn_due?(state, cfg) do
    every = cfg |> Keyword.get(:every_turns, 6) |> positive_int_or(6)
    rem(Map.get(state, :turns, 1), every) == 0
  end

  defp gap_due?(state, now_ms, cfg) do
    min_gap = cfg |> Keyword.get(:min_gap_ms, 120_000) |> nonneg_int_or(120_000)
    now_ms - Map.get(state, :last_asked_ms, 0) >= min_gap
  end

  defp unsafe_pressure?(si, mood, cfg) do
    text = text_from_si(si) |> String.downcase()
    max_vigilance = cfg |> Keyword.get(:max_vigilance, 0.75) |> number_or(0.75)
    vigilance = mood_value(mood, :vigilance)
    tone_hint = Map.get(mood, :tone_hint)

    vigilance > max_vigilance or
      tone_hint in [:deescalate, :panic, :emergency] or
      Regex.match?(
        ~r/\b(hurt myself|kill myself|suicide|emergency|dangerous|crashing|failing)\b/u,
        text
      )
  end

  defp recent_episode_results(si, cfg) do
    max_recent = cfg |> Keyword.get(:max_recent, 40) |> positive_int_or(40)

    si
    |> evidence_episodes()
    |> case do
      [] -> hippocampus_window(max_recent)
      list -> list
    end
    |> Enum.take(max_recent)
  end

  defp evidence_episodes(si) do
    case Map.get(si, :evidence) || %{} do
      %{episodes: eps} when is_list(eps) -> eps
      %{"episodes" => eps} when is_list(eps) -> eps
      _ -> []
    end
  end

  defp hippocampus_window(limit) do
    cond do
      not module_loaded?(Brain.Hippocampus) ->
        []

      not function_exported?(Brain.Hippocampus, :snapshot, 0) ->
        []

      true ->
        case Brain.Hippocampus.snapshot() do
          %{window: window} when is_list(window) ->
            window
            |> Enum.take(limit)
            |> Enum.map(fn {at, ep} -> %{score: 0.0, at: at, episode: ep} end)

          _ ->
            []
        end
    end
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  defp module_loaded?(module) do
    case Code.ensure_loaded(module) do
      {:module, ^module} -> true
      _ -> false
    end
  end

  defp choose_candidate(results, state, cfg) do
    min_uncertainty = cfg |> Keyword.get(:min_uncertainty, 0.35) |> number_or(0.35)
    asked = MapSet.new(Map.get(state, :asked, []))

    results
    |> Enum.map(&candidate_from_result/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.reject(&MapSet.member?(asked, &1.signature))
    |> Enum.reject(&clarified?/1)
    |> Enum.filter(&(&1.uncertainty >= min_uncertainty))
    |> Enum.sort_by(&{&1.uncertainty, &1.recency}, :desc)
    |> List.first()
  end

  defp candidate_from_result(result) when is_map(result) do
    episode = Map.get(result, :episode) || Map.get(result, "episode") || result
    slate = Map.get(episode, :slate) || Map.get(episode, "slate") || %{}
    meta = Map.get(episode, :meta) || Map.get(episode, "meta") || %{}
    at = Map.get(result, :at) || Map.get(result, "at") || Map.get(meta, :at) || 0
    topic = extract_topic(slate, meta)

    if topic == "" do
      nil
    else
      uncertainty = uncertainty_score(result, episode, slate, meta, topic)
      signature = signature_for(topic, at, slate, meta)

      %{
        topic: topic,
        uncertainty: uncertainty,
        at: at,
        recency: at_number(at),
        signature: signature,
        reason: uncertainty_reason(result, slate, meta, topic),
        slate: slate,
        meta: meta
      }
    end
  end

  defp candidate_from_result(_), do: nil

  defp question_result(nil), do: :none

  defp question_result(%{} = candidate) do
    question =
      "Curiosity check: Earlier you mentioned \"#{candidate.topic}\". " <>
        "Should I treat that as a feature idea, a note to remember, or just context?"

    {:ok, question,
     %{
       topic: candidate.topic,
       uncertainty: Float.round(candidate.uncertainty, 3),
       reason: candidate.reason,
       episode_at: candidate.at,
       signature: candidate.signature
     }}
  end

  defp update_session_state(state, {:ok, _question, %{signature: signature}}, now_ms, cfg) do
    keep = cfg |> Keyword.get(:asked_keep, 40) |> positive_int_or(40)

    asked =
      [signature | Map.get(state, :asked, [])]
      |> Enum.uniq()
      |> Enum.take(keep)

    state
    |> Map.put(:last_asked_ms, now_ms)
    |> Map.put(:asked, asked)
  end

  defp update_session_state(state, _result, _now_ms, _cfg), do: state

  defp extract_topic(slate, meta) do
    [
      get_in_path(slate, [:si, :sentence]),
      get_in_path(slate, ["si", "sentence"]),
      get_in_path(slate, [:si, :meta, :sentence]),
      get_in_path(slate, [:si, "meta", "sentence"]),
      get_in_path(slate, ["si", "meta", "sentence"]),
      get_in_path(slate, [:si, :slate, :sentence]),
      get_in_path(slate, [:si, "slate", "sentence"]),
      get_in_path(slate, ["si", "slate", "sentence"]),
      Map.get(slate, :sentence),
      Map.get(slate, "sentence"),
      Map.get(meta, :sentence),
      Map.get(meta, "sentence"),
      Map.get(meta, :text),
      Map.get(meta, "text"),
      sentence_from_tokens(Map.get(slate, :tokens) || Map.get(slate, "tokens")),
      sentence_from_tokens(get_in_path(slate, [:si, :slate, :tokens])),
      sentence_from_tokens(get_in_path(slate, [:si, "slate", "tokens"])),
      sentence_from_tokens(get_in_path(slate, ["si", "slate", "tokens"])),
      winners_topic(Map.get(slate, :winners) || Map.get(slate, "winners"))
    ]
    |> Enum.find_value("", &usable_text/1)
    |> compact_topic()
  end

  defp sentence_from_tokens(tokens) when is_list(tokens) do
    singletons =
      tokens
      |> Enum.filter(fn
        %{} = token -> token_n(token) == 1
        _ -> false
      end)
      |> Enum.sort_by(&token_start/1)
      |> Enum.map(&token_phrase/1)
      |> Enum.reject(&(&1 == ""))

    if singletons != [] do
      Enum.join(singletons, " ")
    else
      tokens
      |> Enum.map(fn
        %{} = token -> token_phrase(token)
        other -> to_string(other)
      end)
      |> Enum.reject(&(&1 == ""))
      |> Enum.sort_by(&String.length/1, :desc)
      |> List.first()
    end
  end

  defp sentence_from_tokens(_), do: nil

  defp token_n(token) do
    value = Map.get(token, :n) || Map.get(token, "n")
    if is_integer(value), do: value, else: 1
  end

  defp token_start(token) do
    case Map.get(token, :span) || Map.get(token, "span") do
      [start | _] when is_integer(start) -> start
      {start, _stop} when is_integer(start) -> start
      _ -> Map.get(token, :token_index) || Map.get(token, "token_index") || 0
    end
  end

  defp token_phrase(token) do
    value =
      Map.get(token, :phrase) ||
        Map.get(token, "phrase") ||
        Map.get(token, :norm) ||
        Map.get(token, "norm")

    case value do
      text when is_binary(text) -> String.trim(text)
      nil -> ""
      other -> other |> to_string() |> String.trim()
    end
  end

  defp winners_topic(winners) when is_list(winners) do
    winners
    |> Enum.map(fn
      %{} = w -> Map.get(w, :lemma) || Map.get(w, "lemma") || Map.get(w, :id) || Map.get(w, "id")
      other -> other
    end)
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.take(5)
    |> Enum.join(", ")
  end

  defp winners_topic(_), do: nil

  defp usable_text(value) when is_binary(value) do
    value = String.trim(value)
    if value == "", do: nil, else: value
  end

  defp usable_text(_), do: nil

  defp compact_topic(text) do
    text
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
    |> String.slice(0, 90)
  end

  defp uncertainty_score(result, episode, slate, meta, topic) do
    explicit_uncertainty(meta) ||
      explicit_uncertainty(slate) ||
      explicit_uncertainty(episode) ||
      inferred_uncertainty(result, slate, meta, topic)
  end

  defp explicit_uncertainty(map) when is_map(map) do
    value =
      Map.get(map, :uncertainty) ||
        Map.get(map, "uncertainty") ||
        get_in_path(map, [:labels, :uncertainty]) ||
        get_in_path(map, ["labels", "uncertainty"])

    if is_number(value), do: clamp01(value), else: nil
  end

  defp explicit_uncertainty(_), do: nil

  defp inferred_uncertainty(result, slate, meta, topic) do
    confidence =
      [
        Map.get(result, :confidence),
        Map.get(result, "confidence"),
        Map.get(meta, :confidence),
        Map.get(meta, "confidence"),
        get_in_path(slate, [:si, :confidence]),
        get_in_path(slate, ["si", "confidence"]),
        get_in_path(slate, [:si, :response_meta, :confidence]),
        get_in_path(slate, ["si", "response_meta", "confidence"])
      ]
      |> Enum.find(&is_number/1)

    low_conf = if is_number(confidence), do: 1.0 - clamp01(confidence), else: 0.15
    fallback = if fallback_signal?(slate, meta), do: 0.3, else: 0.0
    vague = if vague_topic?(topic), do: 0.2, else: 0.0
    recall_gap = 1.0 - clamp01(Map.get(result, :score) || Map.get(result, "score") || 0.0)

    clamp01(max(low_conf, 0.15) + fallback + vague + recall_gap * 0.15)
  end

  defp uncertainty_reason(result, slate, meta, topic) do
    cond do
      explicit_uncertainty(meta) || explicit_uncertainty(slate) -> :explicit_uncertainty
      fallback_signal?(slate, meta) -> :fallback_signal
      vague_topic?(topic) -> :vague_episode
      is_number(Map.get(result, :confidence) || Map.get(result, "confidence")) -> :low_confidence
      true -> :weak_recall_signal
    end
  end

  defp fallback_signal?(slate, meta) do
    text =
      [
        Map.get(meta, :kind),
        Map.get(meta, "kind"),
        Map.get(meta, :response_text),
        Map.get(meta, "response_text"),
        get_in_path(slate, [:si, :response_text]),
        get_in_path(slate, ["si", "response_text"]),
        winners_topic(Map.get(slate, :winners) || Map.get(slate, "winners"))
      ]
      |> Enum.map(&to_string/1)
      |> Enum.join(" ")
      |> String.downcase()

    String.contains?(text, "fallback") or
      String.contains?(text, "quick todo list") or
      String.contains?(text, "short outline")
  end

  defp vague_topic?(topic) when is_binary(topic) do
    t = String.downcase(topic)

    String.contains?(t, "not sure") or String.contains?(t, "maybe") or
      String.contains?(t, "interesting")
  end

  defp vague_topic?(_), do: false

  defp clarified?(%{slate: slate, meta: meta}) do
    tags =
      List.wrap(Map.get(slate, :tags) || Map.get(slate, "tags")) ++
        List.wrap(Map.get(meta, :tags) || Map.get(meta, "tags"))

    tags
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.any?(&(&1 in ["clarified", "curiosity_clarified"]))
  end

  defp signature_for(topic, at, slate, meta) do
    id =
      Map.get(meta, :id) ||
        Map.get(meta, "id") ||
        Map.get(slate, :id) ||
        Map.get(slate, "id") ||
        topic

    :erlang.phash2({id, at, topic})
  end

  defp at_number(value) when is_integer(value), do: value
  defp at_number(value) when is_float(value), do: round(value)
  defp at_number(_), do: 0

  defp session_id(si) do
    Map.get(si, :session_id) || Map.get(si, :session) || Map.get(si, :conversation_id) || :global
  end

  defp text_from_si(si) do
    Map.get(si, :text) || Map.get(si, :sentence) || Map.get(si, :keyword) || ""
  end

  defp mood_value(mood, key) do
    value = get_in_path(mood, [:mood, key]) || Map.get(mood, key)
    if is_number(value), do: value * 1.0, else: 0.0
  end

  defp get_in_path(map, path) when is_map(map) and is_list(path) do
    Enum.reduce_while(path, map, fn key, acc ->
      case acc do
        %{} -> {:cont, Map.get(acc, key)}
        _ -> {:halt, nil}
      end
    end)
  end

  defp get_in_path(_, _), do: nil

  defp clamp01(value) when is_number(value), do: max(0.0, min(1.0, value * 1.0))
  defp number_or(value, _default) when is_number(value), do: value * 1.0
  defp number_or(_value, default), do: default

  defp positive_int_or(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_int_or(_value, default), do: default

  defp nonneg_int_or(value, _default) when is_integer(value) and value >= 0, do: value
  defp nonneg_int_or(_value, default), do: default
end
