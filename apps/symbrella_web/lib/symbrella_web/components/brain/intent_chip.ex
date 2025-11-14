defmodule SymbrellaWeb.Components.Brain.IntentChip do
  @moduledoc """
  Pill showing the latest *user intent* classification.

  Expects:
    %{intent: atom(),
      keyword: binary(),
      confidence: number() | nil,
      source: atom() | nil,
      at_ms: integer() | nil}
  """
  use Phoenix.Component

  attr :intent, :map, default: nil
  attr :class, :string, default: ""

  def intent_chip(assigns) do

assigns
|> IO.inspect
    intent = assigns[:intent] || %{}
    label = Map.get(intent, :intent, :unknown)
    keyword = Map.get(intent, :keyword, "")
    conf = Map.get(intent, :confidence)
    source = Map.get(intent, :source, :core)
    at_ms = Map.get(intent, :at_ms)

    kw_shown =
      cond do
        is_binary(keyword) and byte_size(keyword) > 28 -> String.slice(keyword, 0, 28) <> "…"
        is_binary(keyword) -> keyword
        true -> ""
      end

    pct = fmt_pct(conf)
    since = fmt_since(at_ms)
    {badge_cls_base, dot_cls} = label_style(label)
    badge_cls = emphasize_by_conf(badge_cls_base, conf)

    assigns =
      assigns
      |> assign(:label, label)
      |> assign(:kw_shown, kw_shown)
      |> assign(:kw_full, keyword || "")
      |> assign(:pct, pct)
      |> assign(:since, since)
      |> assign(:source, source)
      |> assign(:badge_cls, badge_cls)
      |> assign(:dot_cls, dot_cls)

    ~H"""
    <div
      class={[
        "flex items-center gap-2 rounded-2xl px-3 py-2 shadow-sm border border-black/5",
        @class
      ]}
      title={"#{@label} · " <> (@kw_full || "")}
    >
      <span class="text-[10px] font-semibold tracking-wide">Intent</span>
      
    <!-- colored label pill -->
      <span class={[
        "inline-flex items-center gap-1 rounded-full border px-2 py-0.5 text-[10px] font-medium",
        @badge_cls
      ]}>
        <span class={["inline-block h-1.5 w-1.5 rounded-full", @dot_cls]} />
        {to_string(@label)}
      </span>
      
    <!-- keyword (truncated, full in tooltip via container title) -->
      <%= if @kw_shown != "" do %>
        <span class="text-[10px] opacity-80">· {@kw_shown}</span>
      <% end %>
      
    <!-- confidence -->
      <%= if @pct != "--" do %>
        <span class="text-[10px] opacity-80">· {@pct}</span>
      <% end %>
      
    <!-- since -->
      <%= if @since do %>
        <span class="text-[10px] opacity-60">· {@since} ago</span>
      <% end %>
      
    <!-- source -->
      <span class="text-[10px] opacity-60">( {to_string(@source)} )</span>
    </div>
    """
  end

  # Base palette per label
  defp label_style(:abuse), do: {"bg-red-50 border-red-200 text-red-700", "bg-red-500"}
  defp label_style(:insult), do: {"bg-amber-50 border-amber-200 text-amber-700", "bg-amber-500"}
  defp label_style(:ask), do: {"bg-sky-50 border-sky-200 text-sky-700", "bg-sky-500"}
  defp label_style(:translate), do: {"bg-teal-50 border-teal-200 text-teal-700", "bg-teal-500"}

  defp label_style(:greet),
    do: {"bg-emerald-50 border-emerald-200 text-emerald-700", "bg-emerald-500"}

  defp label_style(:unknown), do: {"bg-zinc-50 border-zinc-200 text-zinc-700", "bg-zinc-500"}
  defp label_style(_), do: {"bg-zinc-50 border-zinc-200 text-zinc-700", "bg-zinc-500"}

  # Subtle emphasis for high confidence
  defp emphasize_by_conf(base, conf) when is_float(conf) and conf >= 0.8,
    do: base <> " ring-1 ring-current/30"

  defp emphasize_by_conf(base, _), do: base

  # Format confidence as percentage; robust for ints/floats/strings/nil
  defp fmt_pct(nil), do: "--"
  defp fmt_pct(conf) when is_integer(conf), do: fmt_pct(conf * 1.0)

  defp fmt_pct(conf) when is_float(conf) do
    val = if conf <= 1.0, do: conf * 100.0, else: conf
    :erlang.float_to_binary(val, decimals: 1) <> "%"
  end

  defp fmt_pct(conf) when is_binary(conf) do
    case Float.parse(conf) do
      {f, _} -> fmt_pct(f)
      _ -> "--"
    end
  end

  defp fmt_pct(_), do: "--"

  # "since" formatter (uses system time at render -> updates each LiveView diff)
  defp fmt_since(nil), do: nil

  defp fmt_since(at_ms) when is_integer(at_ms) do
    now = System.system_time(:millisecond)
    delta = max(now - at_ms, 0)

    cond do
      delta < 1_000 -> Integer.to_string(delta) <> "ms"
      delta < 60_000 -> :erlang.float_to_binary(delta / 1000.0, decimals: 1) <> "s"
      delta < 3_600_000 -> :erlang.float_to_binary(delta / 60_000.0, decimals: 1) <> "m"
      true -> Integer.to_string(div(delta, 3_600_000)) <> "h"
    end
  end

  defp fmt_since(_), do: nil
end
