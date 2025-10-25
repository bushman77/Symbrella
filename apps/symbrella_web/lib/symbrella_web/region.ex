# lib/symbrella_web/region.ex
defmodule SymbrellaWeb.Region do
  @moduledoc false

  @callback key()    :: atom()
  @callback path()   :: String.t()
  @callback colors() :: {String.t(), String.t()}
  @callback anchor() :: {number(), number()}
  @callback tweak()  :: %{dx: number(), dy: number(), s: number()}

  # Normalize possibly-quoted literals at compile time
  defp normalize_map_literal({:%{}, _, _} = quoted), do: quoted |> Code.eval_quoted([]) |> elem(0)
  defp normalize_map_literal(%{} = m), do: m
  defp normalize_map_literal(_), do: %{dx: 0, dy: 0, s: 1.0}

  defmacro __using__(opts) do
    key    = Keyword.fetch!(opts, :key)
    path   = Keyword.fetch!(opts, :path)
    colors = Keyword.fetch!(opts, :colors)
    anchor = Keyword.fetch!(opts, :anchor)
    tweak0 = Keyword.get(opts, :tweak, %{dx: 0, dy: 0, s: 1.0})

    # If someone passed tweak: quote(do: %{...}), evaluate it now.
    tweak  = normalize_map_literal(tweak0)

    quote do
      @behaviour SymbrellaWeb.Region

      @impl true 
def key,    do: unquote(key)
      @impl true 
def path,   do: unquote(path)
      @impl true 
def colors, do: unquote(colors)
      @impl true 
def anchor, do: unquote(anchor)
      # Emit a literal map (never a quoted AST)
      @impl true 
def tweak,  do: unquote(Macro.escape(tweak))

      # friendly shims
      def colours, do: colors()
      def region,  do: tweak()
      def acchor,  do: anchor()
    end
  end
end

