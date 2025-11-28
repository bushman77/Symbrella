defmodule Brain.WorkingMemory do
  @moduledoc """
  Pure working-memory utilities (no process state).

  WM item shape (newest-first list):

      %{
        id: term(),
        source: atom() | binary() | nil,
        activation: float(),
        score: float(),
        ts: non_neg_integer(),
        inserted_at: non_neg_integer(),
        last_bump: non_neg_integer(),
        payload: map()
      }

  Notes:

  • Backwards compatible with older entries like `%{id: "A", score: 0.4, ts: 1}`.
    We infer activation from `score` when missing, and we preserve/refresh `:ts`.
  """

  @type wm_item :: %{
          optional(:id) => any(),
          optional(:source) => atom() | String.t() | nil,
          optional(:activation) => float(),
          optional(:score) => float(),
          optional(:ts) => non_neg_integer(),
          optional(:inserted_at) => non_neg_integer(),
          optional(:last_bump) => non_neg_integer(),
          optional(:payload) => map()
        }

  @type cfg :: %{
          capacity: pos_integer(),
          decay_ms: pos_integer(),
          gate_threshold: number(),
          merge_duplicates?: boolean()
        }

  @spec normalize(map(), non_neg_integer(), keyword()) :: wm_item()
  def normalize(%{} = cand, now_ms, opts \\ []) do
    raw =
      cand[:activation] ||
        cand[:activation_snapshot] ||
        cand[:score] ||
        0.0

    act = Keyword.get(opts, :activation, raw * 1.0)
    score = (cand[:score] || raw) * 1.0
    ts = now_ms

    # Try to recover a "reason" tag from either the candidate or its nested payload.
    base_reason =
      cand[:reason] ||
        cand["reason"] ||
        case cand[:payload] || cand["payload"] do
          %{} = inner -> inner[:reason] || inner["reason"]
          _ -> nil
        end

    # If no explicit reason, infer for known probe shapes (e.g. curiosity probes).
    reason = base_reason || infer_reason(cand)

    # Ensure the payload we store carries the reason (if any),
    # so callers like CuriosityFlowTest can introspect payload[:reason].
    payload =
      case reason do
        nil -> cand
        r -> Map.put(cand, :reason, r)
      end

    # Prefer explicit source, otherwise fall back to reason, then :unknown
    src =
      cand[:source] ||
        cand["source"] ||
        reason ||
        :unknown

    %{
      id:
        cand[:id] || cand["id"] ||
          cand[:chosen_id] || cand["chosen_id"] ||
          cand[:lemma] || cand["lemma"] ||
          cand[:word] || cand["word"] ||
          cand[:phrase] || cand["phrase"] ||
          Kernel.make_ref(),
      source: src,
      activation: clamp01(act),
      score: clamp01(score),
      ts: ts,
      inserted_at: now_ms,
      last_bump: now_ms,
      payload: payload
    }
  end

  @spec upsert([wm_item()], wm_item(), cfg()) :: [wm_item()]
  def upsert(wm, item, %{merge_duplicates?: true}) do
    case Enum.split_with(wm, &same_identity?(&1, item)) do
      {[], rest} ->
        [item | rest]

      {[existing | tail_dups], rest} ->
        merged = merge_items(existing, item)

        [merged | tail_dups ++ rest]
        |> Enum.reject(&(&1 != merged and same_identity?(&1, merged)))
    end
  end

  def upsert(wm, item, _cfg), do: [item | wm]

  @spec decay([wm_item()], non_neg_integer(), pos_integer()) :: [wm_item()]
  def decay(wm, now_ms, decay_ms) when is_integer(decay_ms) and decay_ms > 0 do
    Enum.map(wm, fn it ->
      # Use last_bump → inserted_at → ts → now as recency anchor
      ts =
        it[:last_bump] ||
          it[:inserted_at] ||
          it[:ts] ||
          now_ms

      age = now_ms - ts
      factor = :math.pow(0.5, age / decay_ms)

      base_activation = it[:activation] || it[:score] || 0.0
      activation = clamp01(base_activation * factor)

      it
      |> Map.put(:activation, activation)
      |> Map.put_new(:ts, ts)
      |> Map.put_new(:inserted_at, ts)
      |> Map.put(:last_bump, ts)
    end)
  end

  def decay(wm, _now, _decay_ms), do: wm

  @spec trim([wm_item()], pos_integer()) :: [wm_item()]
  def trim(wm, cap) when is_integer(cap) and cap > 0, do: Enum.take(wm, cap)
  def trim(wm, _), do: wm

  @spec remove([wm_item()], any()) :: {[wm_item()], non_neg_integer()}
  def remove(wm, id) when not is_function(id) do
    {kept, _dropped} = Enum.split_with(wm, fn it -> it.id != id end)
    {kept, length(wm) - length(kept)}
  end

  def remove(wm, fun) when is_function(fun, 1) do
    {kept, _dropped} = Enum.split_with(wm, fn it -> fun.(it) == false end)
    {kept, length(wm) - length(kept)}
  end

  # --- internal helpers -------------------------------------------------------

  # Heuristic "why is this here?" inference for probes, used only
  # when no explicit :reason was provided upstream.
  defp infer_reason(cand) do
    id =
      cand[:id] ||
        cand["id"] ||
        ""

    lemma =
      cand[:lemma] ||
        cand["lemma"] ||
        cand[:word] ||
        cand["word"] ||
        cand[:phrase] ||
        cand["phrase"] ||
        ""

    source = cand[:source] || cand["source"] || nil

    id_down =
      case id do
        b when is_binary(b) -> String.downcase(b)
        _ -> ""
      end

    lemma_down =
      case lemma do
        b when is_binary(b) -> String.downcase(b)
        _ -> ""
      end

    source_down =
      case source do
        b when is_binary(b) -> String.downcase(b)
        a when is_atom(a) -> Atom.to_string(a) |> String.downcase()
        _ -> ""
      end

    cond do
      # Explicit curiosity source
      source_down == "curiosity" ->
        :curiosity

      # Canonical curiosity probe ids: "curiosity|probe|..."
      String.starts_with?(id_down, "curiosity|probe|") ->
        :curiosity

      String.contains?(id_down, "curiosity|probe|") ->
        :curiosity

      # NEW: modern curiosity probe ids from Thalamus/OFC/BG/DLPFC:
      # e.g. "probe|drop", "probe|keep", "probe|ofc|69"
      String.starts_with?(id_down, "probe|") and lemma_down == "probe" ->
        :curiosity

      # Fallback: explicit lemma "curiosity"
      lemma_down == "curiosity" ->
        :curiosity

      true ->
        nil
    end
  end

  defp merge_items(existing, item) do
    ts_existing =
      existing[:ts] ||
        existing[:last_bump] ||
        existing[:inserted_at] ||
        0

    ts_item =
      item[:ts] ||
        item[:last_bump] ||
        item[:inserted_at] ||
        0

    ts = max(ts_existing, ts_item)

    last_bump_existing = existing[:last_bump] || ts_existing
    last_bump_item = item[:last_bump] || ts_item

    %{
      existing
      | activation: max(existing[:activation] || 0.0, item[:activation] || 0.0),
        score: max(existing[:score] || 0.0, item[:score] || 0.0),
        ts: ts,
        inserted_at: existing[:inserted_at] || existing[:ts] || item[:inserted_at] || ts,
        last_bump: max(last_bump_existing, last_bump_item),
        payload: Map.merge(existing[:payload] || %{}, item[:payload] || %{})
    }
  end

  defp same_identity?(a, b), do: a.id == b.id

  defp clamp01(x) when is_number(x), do: x |> max(0.0) |> min(1.0)
end
