defmodule Core.Intent.Selection do
  @moduledoc """
  Lightweight intent selector.

  - Returns the incoming SemanticInput (map) with `:intent`, `:keyword`, `:confidence`.
  - Appends `{:intent, %{keyword, intent, confidence}}` to `si.trace`.
  - Emits telemetry so BrainLive can display it:

      * [:core,  :intent, :selected]
      * [:brain, :intent, :selected]

    measurements: %{confidence: float}
    metadata:     %{intent: atom, keyword: binary}
  """

  @type si :: map()
  @type intent :: :greet | :translate | :ask | :unknown

  @spec select(si(), Keyword.t()) :: si()
  def select(%{sentence: _} = si, _opts \\ []) do
    kw = extract_keyword(si)
    {intent, conf} = infer_intent(kw, si)

    si2 =
      si
      |> Map.put(:intent, intent)
      |> Map.put(:keyword, kw)
      |> Map.put(:confidence, conf)
      |> Map.update(:trace, [], &[{:intent, %{keyword: kw, intent: intent, confidence: conf}} | &1])

    emit(intent, kw, conf)
    si2
  end

  def select(si, _opts), do: si

  # ───────────────────────────── private ─────────────────────────────

# keep this version
defp emit(intent, kw, conf) when is_atom(intent) and is_binary(kw) and is_number(conf) do
  meas = %{confidence: conf}
  meta = %{intent: intent, keyword: kw}

  # 1) Update Brain's authoritative snapshot (best-effort; never crash)
  _ =
    try do
      Brain.set_latest_intent(%{intent: intent, keyword: kw, confidence: conf})
    catch
      _, _ -> :ok
    end

  # 2) Telemetry (best-effort)
  if function_exported?(:telemetry, :execute, 3) do
    :telemetry.execute([:core, :intent, :selected],  meas, meta)
    :telemetry.execute([:brain, :intent, :selected], meas, meta)
  end

  :ok
end

# fallback
defp emit(_, _, _), do: :ok

  # Pull a human-ish keyword to display
  defp extract_keyword(%{keyword: kw}) when is_binary(kw) and kw != "", do: String.downcase(kw)

  defp extract_keyword(%{tokens: tokens}) when is_list(tokens) do
    tokens
    |> Enum.map(fn t -> t[:phrase] || t["phrase"] || "" end)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
    |> String.trim()
    |> String.downcase()
  end

  defp extract_keyword(%{sentence: s}) when is_binary(s),
    do: s |> String.trim() |> String.downcase()

  defp extract_keyword(_), do: ""

  # Extremely small heuristic just to drive the UI
  defp infer_intent(kw, _si) when kw in ["", nil], do: {:unknown, 0.0}

  defp infer_intent(kw, _si) do
    cond do
      Regex.match?(~r/^(hi|hello|hey|yo|gm|good (morning|afternoon|evening))\b/i, kw) ->
        {:greet, 0.70}

      Regex.match?(~r/\b(translate|what(?:'s| is) (.+?) in)\b/i, kw) ->
        {:translate, 0.50}

      true ->
        {:ask, 0.55}
    end
  end
end

