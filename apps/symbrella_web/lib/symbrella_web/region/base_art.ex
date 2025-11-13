defmodule SymbrellaWeb.Region.BaseArt do
  @moduledoc """
  Base brain line-art as a <g> component.

  • Renders a <g> inside the caller's <svg> (shares viewBox; no drift)
  • Provides viewbox/1 so the parent can size its <svg> correctly
  """

  use Phoenix.Component

  @svg_path Application.app_dir(:symbrella_web, "priv/static/images/brain.svg")
  @external_resource @svg_path

  # Load raw SVG once at compile time (no local helpers used here)
  @svg_raw if File.exists?(@svg_path), do: File.read!(@svg_path), else: nil

  # Parse the default viewBox from the on-disk SVG (still safe at compile time)
  @vb (if is_binary(@svg_raw) do
         case Regex.run(~r/viewBox\s*=\s*"([^"]+)"/, @svg_raw) do
           [_, vb] ->
             [minx, miny, w, h] =
               vb
               |> String.split(~r/\s+/, trim: true)
               |> Enum.map(fn s ->
                 case Float.parse(s) do
                   {f, _} -> f
                   :error -> 0.0
                 end
               end)

             %{minx: minx, miny: miny, w: w, h: h}

           _ ->
             %{minx: 0.0, miny: 0.0, w: 516.0, h: 406.0}
         end
       else
         %{minx: 0.0, miny: 0.0, w: 516.0, h: 406.0}
       end)

  @doc """
  Returns `%{minx, miny, w, h}` for sizing the parent `<svg>`.
  If an override SVG string is provided, we parse its viewBox; else we use the default.
  """
  def viewbox(nil), do: @vb

  def viewbox(svg) when is_binary(svg) and byte_size(svg) > 0 do
    case Regex.run(~r/viewBox\s*=\s*"([^"]+)"/, svg) do
      [_, vb] ->
        [minx, miny, w, h] =
          vb
          |> String.split(~r/\s+/, trim: true)
          |> Enum.map(fn s ->
            case Float.parse(s) do
              {f, _} -> f
              :error -> 0.0
            end
          end)

        %{minx: minx, miny: miny, w: w, h: h}

      _ ->
        @vb
    end
  end

  @doc """
  Draw the base art as a `<g>` inside the caller's `<svg>`.
  Accepts optional `:svg` override (string).
  """
  attr :svg, :string, default: nil

  def group(assigns) do
    svg_src =
      case assigns[:svg] do
        s when is_binary(s) and byte_size(s) > 0 -> s
        _ -> @svg_raw || ""
      end

    inner = svg_inner(svg_src)
    assigns = assign(assigns, :inner, inner)

    ~H"""
    <g id="base" vector-effect="non-scaling-stroke">
      {Phoenix.HTML.raw(@inner)}
    </g>
    """
  end

  # -------- Helpers (runtime) --------

  defp svg_inner(svg) when is_binary(svg) do
    stripped = strip_size_attrs(svg)

    case Regex.run(~r/<svg[^>]*>(.*)<\/svg>/s, stripped, capture: :all_but_first) do
      [inner] -> inner
      _ -> stripped
    end
  end

  defp strip_size_attrs(svg) when is_binary(svg) do
    svg
    |> String.replace(~r/\swidth="[^"]*"/, "")
    |> String.replace(~r/\sheight="[^"]*"/, "")
  end
end
