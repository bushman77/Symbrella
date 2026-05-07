defmodule Core.Pipeline.Trace do
  @moduledoc """
  Small trace-event contract for Core pipeline stages.

  Trace entries are intentionally compact and dashboard-friendly. Stage-specific
  details live under `:meta`; common fields stay predictable.
  """

  @type event :: %{
          required(:stage) => atom(),
          required(:input_summary) => map(),
          required(:decision) => atom(),
          required(:reason) => atom() | String.t(),
          required(:scores) => map(),
          required(:meta) => map(),
          required(:ts_ms) => integer()
        }

  @spec append(map(), atom(), keyword() | map()) :: map()
  def append(%{} = si, stage, attrs) when is_atom(stage) do
    ev = event(si, stage, attrs)
    Map.update(si, :trace, [ev], fn trace -> [ev | List.wrap(trace)] end)
  end

  @spec event(map(), atom(), keyword() | map()) :: event()
  def event(%{} = si, stage, attrs) when is_atom(stage) do
    attrs = normalize_attrs(attrs)
    meta = attrs |> Map.get(:meta, %{}) |> normalize_meta()

    %{
      stage: stage,
      input_summary: Map.get(attrs, :input_summary, input_summary(si)),
      decision: Map.get(attrs, :decision, :observed),
      reason: Map.get(attrs, :reason, :pipeline_stage),
      scores: attrs |> Map.get(:scores, %{}) |> clamp_scores(),
      meta: meta,
      ts_ms: System.system_time(:millisecond)
    }
  end

  defp normalize_attrs(attrs) when is_list(attrs), do: Map.new(attrs)
  defp normalize_attrs(attrs) when is_map(attrs), do: attrs
  defp normalize_attrs(_), do: %{}

  defp normalize_meta(meta) when is_map(meta), do: meta
  defp normalize_meta(_), do: %{}

  defp input_summary(si) do
    tokens = Map.get(si, :tokens, [])
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence")

    %{
      sentence_chars: if(is_binary(sentence), do: String.length(sentence), else: 0),
      token_count: if(is_list(tokens), do: length(tokens), else: 0),
      source: Map.get(si, :source) || Map.get(si, "source")
    }
  end

  defp clamp_scores(scores) when is_map(scores) do
    Map.new(scores, fn {key, value} -> {key, clamp01(value)} end)
  end

  defp clamp_scores(_), do: %{}

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
