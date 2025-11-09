defmodule Brain.LIFG.SlateFilter do
  @moduledoc """
  Filters impossible (lemma, POS) pairs from per-token candidate slates before Stage-1.

  Default rules (overridable via `config :brain, :lifg_slate_filter_rules`):
    - "a"   → allow only det/article/particle
    - "A"   → same as "a" (drop capital-A nouns)
    - "eat" → allow only verb

  Phrases (pos == "phrase") always pass. Unknown POS passes (fail-safe).
  Emits telemetry: [:brain, :lifg, :slate_filter_drop]
  """

  alias Brain.Utils.Safe

  @default_rules [
    %{lemma: "a",   allow: ~w(det article particle), drop_others?: true},
    %{lemma: "A",   allow: ~w(det article particle), drop_others?: true},
    %{lemma: "eat", allow: ~w(verb),                 drop_others?: true}
  ]

  @doc "Sanitize a per-token slate map: %{idx => [cand, ...]}."
  def sanitize_map(slate_map, opts \\ []) when is_map(slate_map) do
    rules = rules_from_env(opts)

    slate_map
    |> Enum.into(%{}, fn {idx, cands} ->
      {kept, dropped} = partition_allowed(List.wrap(cands), rules)
      emit_drop_telemetry(idx, dropped)
      {idx, kept}
    end)
  end

  # ── internals ─────────────────────────────────────────────────────────

  defp rules_from_env(opts) do
    Keyword.get(opts, :rules, Application.get_env(:brain, :lifg_slate_filter_rules, @default_rules))
    |> List.wrap()
    |> Enum.map(fn r ->
      %{
        lemma: down(r[:lemma]),
        allow: r[:allow] |> List.wrap() |> MapSet.new(),
        drop_others?: !!r[:drop_others?]
      }
    end)
  end

  defp partition_allowed(list, rules) do
    Enum.split_with(list, fn cand -> allowed?(cand, rules) end)
  end

  defp allowed?(cand, rules) do
    id   = Safe.get(cand, :id) |> to_string_safe()
    pos  = pos_from_id(id)
    base = base_from_id(id) || Safe.get(cand, :lemma) || Safe.get(cand, :word) || ""
    lemma = down(base)

    cond do
      pos == "phrase"          -> true
      is_nil(pos) or pos == "" -> true   # fail-safe if POS missing
      true ->
        case Enum.find(rules, fn r -> r.lemma == lemma end) do
          nil -> true
          %{allow: allow} -> MapSet.member?(allow, pos)
        end
    end
  end

  defp emit_drop_telemetry(_idx, []), do: :ok
  defp emit_drop_telemetry(idx, dropped) do
    ids = Enum.map(dropped, fn c -> Safe.get(c, :id) |> to_string_safe() end)

    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(
        [:brain, :lifg, :slate_filter_drop],
        %{count: length(dropped)},
        %{token_index: idx, ids: ids, reason: :pos_allowlist}
      )
    end

    :ok
  end

  # ── tiny parsers ──────────────────────────────────────────────────────

  defp base_from_id(nil), do: nil
  defp base_from_id(id) when is_binary(id), do: id |> String.split("|") |> List.first()

  defp pos_from_id(nil), do: nil
  defp pos_from_id(id) when is_binary(id) do
    case String.split(id, "|") do
      [_lemma, pos, _sense] -> pos
      _ -> nil
    end
  end

  defp down(nil), do: ""
  defp down(s) when is_binary(s),
    do: s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")
  defp down(v),
    do: v |> Kernel.to_string() |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")

  defp to_string_safe(nil), do: ""
  defp to_string_safe(v) when is_binary(v), do: v
  defp to_string_safe(v), do: Kernel.to_string(v)
end

