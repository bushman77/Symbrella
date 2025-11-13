defmodule SymbrellaWeb.Region do
  @moduledoc """
  Minimal, compile-time safe contract for region overlays.

  • No compile-time calls into region modules beyond constant getters
  • Accepts `tweak:` as a map, keyword, or quoted literal and normalizes it
  • Optional metadata callbacks (title/subtitle/desc/etc.) are supported but not required
  """

  @type key_t :: atom()
  @type path_t :: String.t()
  @type colors_t :: {String.t(), String.t()}
  @type anchor_t :: {number(), number()}
  @type tweak_t :: %{dx: number(), dy: number(), s: number()}

  @callback key() :: key_t
  @callback path() :: path_t
  @callback colors() :: colors_t
  @callback anchor() :: anchor_t
  @callback tweak() :: tweak_t

  # Optional, *pure metadata* (never used by the renderer at compile-time)
  @callback title() :: String.t() | nil
  @callback subtitle() :: String.t() | nil
  @callback desc() :: String.t() | nil
  @callback modules() :: [String.t() | atom()] | nil
  @callback telemetry() :: [String.t() | atom()] | nil
  @callback config_examples() :: [String.t()] | nil

  @optional_callbacks title: 0, subtitle: 0, desc: 0, modules: 0, telemetry: 0, config_examples: 0

  # --- helpers used at macro-expansion time ----------------------------------

  # Allow passing tweak as: map, keyword, or quoted literal of either
  defp to_map({:%{}, _, _} = quoted),
    do: quoted |> Code.eval_quoted([]) |> elem(0)

  defp to_map({:__block__, _, [kw]}) when is_list(kw) and kw != [],
    do: Map.new(kw)

  defp to_map(kw) when is_list(kw), do: Map.new(kw)
  defp to_map(%{} = m), do: m
  defp to_map(_), do: %{}

  defp to_num(v) when is_integer(v), do: v * 1.0
  defp to_num(v) when is_float(v), do: v
  defp to_num(_), do: nil

  defp sanitize_tweak(raw) do
    m = to_map(raw)

    dx = m[:dx] || m["dx"] || 0
    dy = m[:dy] || m["dy"] || 0
    s = m[:s] || m["s"] || 1.0

    dx = to_num(dx) || 0.0
    dy = to_num(dy) || 0.0
    s = to_num(s) || 1.0

    %{dx: dx, dy: dy, s: s}
  end

  defmacro __using__(opts) do
    key = Keyword.fetch!(opts, :key)
    path = Keyword.fetch!(opts, :path)
    colors = Keyword.fetch!(opts, :colors)
    anchor = Keyword.fetch!(opts, :anchor)
    tweak0 = Keyword.get(opts, :tweak, %{dx: 0, dy: 0, s: 1.0})

    # Optional metadata (pure constants if you choose to provide them)
    title = Keyword.get(opts, :title, nil)
    subtitle = Keyword.get(opts, :subtitle, nil)
    desc = Keyword.get(opts, :desc, nil)
    modules = Keyword.get(opts, :modules, nil)
    telem = Keyword.get(opts, :telemetry, nil)
    confs = Keyword.get(opts, :config_examples, nil)

    # Normalize tweak at compile time and emit as a literal map
    tweak = sanitize_tweak(tweak0)

    quote do
      @behaviour SymbrellaWeb.Region

      @impl true
      def key, do: unquote(key)
      @impl true
      def path, do: unquote(path)
      @impl true
      def colors, do: unquote(colors)
      @impl true
      def anchor, do: unquote(anchor)
      @impl true
      def tweak, do: unquote(Macro.escape(tweak))

      # Friendly alias
      @doc false
      def colours, do: colors()

      # Optional metadata (return constants if provided; otherwise nil/[])
      @doc false
      @impl SymbrellaWeb.Region
      def title, do: unquote(title)

      @doc false
      @impl SymbrellaWeb.Region
      def subtitle, do: unquote(subtitle)

      @doc false
      @impl SymbrellaWeb.Region
      def desc, do: unquote(desc)

      @doc false
      @impl SymbrellaWeb.Region
      def modules, do: unquote(modules)

      @doc false
      @impl SymbrellaWeb.Region
      def telemetry, do: unquote(telem)

      @doc false
      @impl SymbrellaWeb.Region
      def config_examples, do: unquote(confs)
    end
  end
end
