defmodule Brain.LIFG.Stage2 do
  @moduledoc """
  LIFG.Stage2 — gating decisions for WM.

  Consumes the latest Stage1 event in `si.trace` and emits `:decisions`
  suitable for `Brain.WorkingMemory.ingest_stage2/4`.

  Decision encoding:
    * `{:commit, %{id, token_index, score, decision, source, ...}}`

  Telemetry (kept compatible with tests + legacy listeners):
    * `[:brain, :gate, :decision]` — (TEST EXPECTATION)
    * `[:brain, :wm, :gate]`       — (legacy/compat)

  Measurements: `%{score: float}`
  Metadata: `%{decision: :allow | :boost, source: :lifg, id: ..., token_index: ...}`
  """

  @gate_event_test [:brain, :gate, :decision]
  @gate_event_compat [:brain, :wm, :gate]

@spec run(map() | struct(), keyword()) ::
        {:ok, %{si: map() | struct(), event: map()}}
        | {:skip, %{si: map() | struct(), reason: term()}}
        | {:error, term()}
def run(si, opts \\ []) do
  case extract_stage1_event(si) do
    {:ok, ev1} ->
      if stage2_enabled?(opts) do
        case decide(ev1, si, opts) do
          {:ok, decisions, ev2} ->
            {:ok, %{si: si, event: Map.put(ev2, :decisions, decisions)}}

          {:skip, reason} ->
            {:skip, %{si: si, reason: reason}}

          {:error, reason} ->
            {:error, reason}
        end
      else
        {:skip, %{si: si, reason: :not_enabled}}
      end

    {:skip, reason} ->
      {:skip, %{si: si, reason: reason}}
  end
end

defp stage2_enabled?(opts) when is_list(opts) do
  # Default is OFF (tests expect :not_enabled when Stage1 exists).
  Keyword.get(opts, :lifg_stage2_enabled, Application.get_env(:brain, :lifg_stage2_enabled, false))
end

defp stage2_enabled?(_), do: Application.get_env(:brain, :lifg_stage2_enabled, false)

  # ───────────────────────────────────────────────────────────────────────────
  # Stage1 event extraction
  # ───────────────────────────────────────────────────────────────────────────

  defp extract_stage1_event(si) do
    trace = Map.get(si, :trace) || Map.get(si, "trace") || []

    ev =
      trace
      |> List.wrap()
      |> Enum.find(fn e ->
        is_map(e) and
          ((Map.get(e, :stage) == :lifg_stage1) or (Map.get(e, "stage") == :lifg_stage1) or
             Map.has_key?(e, :choices) or Map.has_key?(e, "choices"))
      end)

    if is_map(ev), do: {:ok, ev}, else: {:skip, :no_stage1_event}
  end

  # ───────────────────────────────────────────────────────────────────────────
  # Decide commits
  # ───────────────────────────────────────────────────────────────────────────

  defp decide(ev1, si, opts) when is_map(ev1) and is_list(opts) do
    choices = Map.get(ev1, :choices) || Map.get(ev1, "choices") || []
    if is_list(choices), do: do_decide(choices, si, opts), else: {:error, :bad_choices}
  end

  defp decide(_ev1, _si, _opts), do: {:error, :bad_stage1_event}

  defp do_decide([], _si, _opts), do: {:skip, :no_choices}

  defp do_decide(choices, si, opts) do
    min_score =
      opts
      |> Keyword.get(:lifg_min_score, nil)
      |> fallback_num(Keyword.get(opts, :min_score, nil))
      |> fallback_num(Keyword.get(opts, :gate_threshold, nil))
      |> fallback_num(Application.get_env(:brain, :lifg_stage2_min_score, 0.40))
      |> num()

    boost_margin =
      opts
      |> Keyword.get(:boost_margin, Application.get_env(:brain, :lifg_stage2_boost_margin, 0.10))
      |> num()

    tokens = Map.get(si, :tokens) || Map.get(si, "tokens") || []
    phrase_by_idx = Map.new(List.wrap(tokens), fn t -> {get_idx(t), get_phrase(t)} end)

    {decisions, committed} =
      Enum.reduce(choices, {[], 0}, fn ch0, {acc, n} ->
        ch = to_plain_map(ch0)
        ti = token_index(ch)
        chosen_id = chosen_id(ch)

        score = score_for(ch, chosen_id)
        margin = margin_for(ch)

        if is_integer(ti) and ti >= 0 and is_binary(chosen_id) and score >= min_score do
          decision = if margin >= boost_margin, do: :boost, else: :allow

          lemma =
            Map.get(ch, :lemma) ||
              Map.get(ch, "lemma") ||
              phrase_by_idx[ti] ||
              ""

          commit = %{
            id: chosen_id,
            token_index: ti,
            lemma: lemma,
            score: score,
            decision: decision,
            source: :lifg,
            payload: %{stage: :lifg_stage2, from: :lifg_stage1}
          }

          emit_gate(score, decision, chosen_id, ti)
          {[{:commit, commit} | acc], n + 1}
        else
          {acc, n}
        end
      end)

    if committed == 0 do
      {:skip, :no_commits}
    else
      ev2 = %{
        at_ms: System.system_time(:millisecond),
        min_score: min_score,
        boost_margin: boost_margin,
        committed: committed
      }

      {:ok, Enum.reverse(decisions), ev2}
    end
  end

  # ───────────────────────────────────────────────────────────────────────────
  # Telemetry
  # ───────────────────────────────────────────────────────────────────────────

  defp emit_gate(score, decision, id, token_index) do
    meas = %{score: score}
    meta = %{decision: decision, source: :lifg, id: id, token_index: token_index}

    :telemetry.execute(@gate_event_test, meas, meta)
    :telemetry.execute(@gate_event_compat, meas, meta)
  end

  # ───────────────────────────────────────────────────────────────────────────
  # Score / margin extraction
  # ───────────────────────────────────────────────────────────────────────────

  # Prefer stable fields produced by your Stage1 wrapper:
  #   - :score (often winner score or p_top1)
  #   - :p_top1 (probability top1)
  # Then fall back to maps :scores / :probs.
  defp score_for(ch, chosen_id) do
    s0 = Map.get(ch, :score) || Map.get(ch, "score")
    p0 = Map.get(ch, :p_top1) || Map.get(ch, "p_top1")

    cond do
      is_number(s0) -> num(s0)
      is_number(p0) -> num(p0)
      true -> score_from_maps(ch, chosen_id)
    end
  end

  defp score_from_maps(ch, chosen_id) when is_map(ch) and is_binary(chosen_id) do
    scores = Map.get(ch, :scores) || Map.get(ch, "scores")
    probs = Map.get(ch, :probs) || Map.get(ch, "probs")
    atom_key = existing_atom(chosen_id)

    cond do
      is_map(scores) ->
        num(Map.get(scores, chosen_id) || (atom_key && Map.get(scores, atom_key)) || 0.0)

      is_map(probs) ->
        num(Map.get(probs, chosen_id) || (atom_key && Map.get(probs, atom_key)) || 0.0)

      true ->
        0.0
    end
  end

  defp score_from_maps(_ch, _chosen_id), do: 0.0

  defp margin_for(ch) do
    m0 = Map.get(ch, :margin) || Map.get(ch, "margin")
    if is_number(m0), do: num(m0), else: margin_from_maps(ch)
  end

  defp margin_from_maps(ch) when is_map(ch) do
    scores = Map.get(ch, :scores) || Map.get(ch, "scores")
    probs = Map.get(ch, :probs) || Map.get(ch, "probs")

    vals =
      cond do
        is_map(probs) and map_size(probs) > 0 ->
          probs |> Map.values() |> Enum.map(&num/1)

        is_map(scores) and map_size(scores) > 0 ->
          scores |> Map.values() |> Enum.map(&num/1)

        true ->
          []
      end

    case Enum.sort(vals, :desc) do
      [a, b | _] -> max(a - b, 0.0)
      _ -> 0.0
    end
  end

  # ───────────────────────────────────────────────────────────────────────────
  # Choice field helpers
  # ───────────────────────────────────────────────────────────────────────────

  defp chosen_id(ch) when is_map(ch) do
    raw =
      Map.get(ch, :chosen_id) || Map.get(ch, "chosen_id") ||
        Map.get(ch, :winner_id) || Map.get(ch, "winner_id") ||
        Map.get(ch, :id) || Map.get(ch, "id")

    case raw do
      v when is_binary(v) -> v
      v when is_atom(v) -> Atom.to_string(v)
      _ -> nil
    end
  end

  defp chosen_id(_), do: nil

  defp token_index(ch) when is_map(ch) do
    v =
      Map.get(ch, :token_index) || Map.get(ch, "token_index") ||
        Map.get(ch, :index) || Map.get(ch, "index") || 0

    num_to_idx(v)
  end

  defp token_index(_), do: 0

  defp num_to_idx(i) when is_integer(i) and i >= 0, do: i

  defp num_to_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end

  defp num_to_idx(_), do: 0

  # Safe “maybe atom” lookup (does not create atoms)
  defp existing_atom(s) when is_binary(s) do
    try do
      String.to_existing_atom(s)
    rescue
      _ -> nil
    end
  end

  defp existing_atom(_), do: nil

  # ───────────────────────────────────────────────────────────────────────────
  # General utils
  # ───────────────────────────────────────────────────────────────────────────

  defp get_idx(%{index: i}) when is_integer(i), do: i
  defp get_idx(%{"index" => i}) when is_integer(i), do: i
  defp get_idx(%{token_index: i}) when is_integer(i), do: i
  defp get_idx(%{"token_index" => i}) when is_integer(i), do: i
  defp get_idx(_), do: 0

  defp get_phrase(%{phrase: p}) when is_binary(p), do: p
  defp get_phrase(%{"phrase" => p}) when is_binary(p), do: p
  defp get_phrase(_), do: ""

  defp to_plain_map(m) when is_map(m), do: m

  defp to_plain_map(m) do
    try do
      Map.from_struct(m)
    rescue
      _ -> %{}
    end
  end

  defp num(v) when is_integer(v), do: v * 1.0
  defp num(v) when is_float(v), do: v

  defp num(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp num(_), do: 0.0

  defp fallback_num(nil, b), do: b
  defp fallback_num(a, _b), do: a
end
