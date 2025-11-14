defmodule SymbrellaWeb.Components.Brain.Regions do
  @moduledoc """
  Region-related UI helpers & components extracted from `SymbrellaWeb.BrainHTML`.

  Public API:
    • `label_for/1`            – pretty label for a region atom (uses Registry + fallback)
    • `region_options/1`       – options for the Region <select> ([], or pairs passthrough)
    • `draw_regions/1`         – ordered list of regions to paint
    • `region_summary/1`       – function component for the Region Summary card
  """

  use SymbrellaWeb, :html

  alias SymbrellaWeb.Region.Registry, as: RegionRegistry

  # Prefrontal macro → children; used by draw_regions/1
  @region_groups %{
    prefrontal: ~w(dlpfc vmpfc dmpfc fpc lifg ofc)a
  }

  # ---------- Labels ----------------------------------------------------------

  @doc "Pretty label for a region atom using Registry first, then a sane fallback."
  def label_for(key) when is_atom(key) do
    RegionRegistry.label_for(key) || default_label(key)
  end

  def label_for(other), do: other |> to_string() |> String.upcase()

  # Local fallback so this module is self-contained
  defp default_label(:lifg), do: "LIFG"
  defp default_label(:pmtg), do: "PMTG"
  defp default_label(:ofc), do: "OFC"
  defp default_label(:acc), do: "ACC"
  defp default_label(:dlpfc), do: "DLPFC"
  defp default_label(:vmpfc), do: "VMPFC"
  defp default_label(:dmpfc), do: "DMPFC"
  defp default_label(:fpc), do: "FPC"
  defp default_label(:atl), do: "ATL"
  defp default_label(:salience), do: "Salience"
  defp default_label(:temporal), do: "Temporal"
  defp default_label(:frontal), do: "Frontal"
  defp default_label(:cerebellum), do: "Cerebellum"
  defp default_label(:thalamus), do: "Thalamus"
  defp default_label(:basal_ganglia), do: "Basal Ganglia"
  defp default_label(:hippocampus), do: "Hippocampus"
  defp default_label(:prefrontal), do: "Prefrontal"
  defp default_label(other), do: other |> to_string() |> String.upcase()

  # ---------- Region options (for header select) ------------------------------

  @doc """
  Build region select options.

  When given `[]`, computes a preferred ordering then appends the rest of
  registry keys. When given pairs, passthrough and normalize.
  """
  def region_options([]) do
    preferred = [
      :lifg,
      :pmtg,
      :hippocampus,
      :thalamus,
      :basal_ganglia,
      :ofc,
      :acc,
      :dlpfc,
      :vmpfc,
      :dmpfc,
      :fpc,
      :prefrontal,
      :atl,
      :salience,
      :temporal,
      :frontal,
      :cerebellum
    ]

    keys =
      preferred
      |> Enum.filter(&RegionRegistry.available?/1)
      |> Kernel.++(RegionRegistry.keys() -- preferred)
      |> Enum.uniq()

    Enum.map(keys, &{&1, label_for(&1)})
  end

  def region_options(pairs) do
    Enum.map(pairs, fn
      {key, label, _color, _region} -> {key, label}
      {key, label} -> {key, label}
      atom when is_atom(atom) -> {atom, label_for(atom)}
      _other -> {:lifg, label_for(:lifg)}
    end)
  end

  # ---------- Draw order for overlays ----------------------------------------

  @doc "Return the ordered region list to draw, filtered by availability."
  def draw_regions(selected) do
    base = [:frontal, :temporal, :cerebellum]
    deep = [:thalamus, :basal_ganglia, :hippocampus]
    other = [:pmtg, :atl, :acc]
    pfc_children = Map.get(@region_groups, :prefrontal, [])

    order =
      if selected == :prefrontal,
        do: base ++ [:prefrontal] ++ pfc_children ++ deep ++ other,
        else: base ++ pfc_children ++ deep ++ other

    Enum.filter(order, &RegionRegistry.available?/1)
  end

  # ---------- Region Summary (component) --------------------------------------

  # Kept public so it can be called as <Regions.region_summary ... />
  attr :region, :map, default: %{}
  attr :selected, :atom, default: :lifg
  attr :state, :any, default: nil
  attr :snapshot, :any, default: nil
  attr :region_status, :any, default: nil
  attr :region_state, :map, default: nil

  def region_summary(assigns) do
    region = Map.get(assigns, :region, %{})
    selected = Map.get(assigns, :selected, :lifg)

    state_map =
      assigns[:state] ||
        assigns[:snapshot] ||
        assigns[:region_status] ||
        get_in(assigns, [:region_state, :snapshot]) ||
        %{}

    title =
      Map.get(region, :title) ||
        selected |> Atom.to_string() |> String.upcase()

    subtitle = Map.get(region, :subtitle) || default_subtitle(selected)
    desc = Map.get(region, :desc) || default_region_desc(selected)
    mods = Map.get(region, :modules, [])
    telem = Map.get(region, :telemetry, [])
    confs = Map.get(region, :config_examples, [])

    assigns =
      assigns
      |> assign(:title, title)
      |> assign(:subtitle, subtitle)
      |> assign(:desc, desc)
      |> assign(:mods, mods)
      |> assign(:telem, telem)
      |> assign(:confs, confs)
      |> assign(:state_map, state_map)

    ~H"""
    <div class="border rounded-xl p-4 shadow-sm">
      <h2 class="font-semibold">{@title} — {@subtitle}</h2>
      <p class="text-sm text-zinc-600 mb-3">{@desc}</p>

      <div class="grid grid-cols-1 md:grid-cols-3 gap-3">
        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Modules</h3>
          <%= if @mods == [] do %>
            <div class="text-sm text-zinc-400">None</div>
          <% else %>
            <ul class="text-xs space-y-1">
              <%= for m <- @mods do %>
                <li><code>{m}</code></li>
              <% end %>
            </ul>
          <% end %>
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Telemetry</h3>
          <%= if @telem == [] do %>
            <div class="text-sm text-zinc-400">N/A</div>
          <% else %>
            <ul class="text-xs space-y-1">
              <%= for t <- @telem do %>
                <li><code>{t}</code></li>
              <% end %>
            </ul>
          <% end %>
        </div>

        <div>
          <h3 class="text-sm text-zinc-500 mb-1">Config (examples)</h3>
          <%= if @confs == [] do %>
            <div class="text-sm text-zinc-400">No examples</div>
          <% else %>
            <ul class="text-xs space-y-1">
              <%= for c <- @confs do %>
                <li><code>{c}</code></li>
              <% end %>
            </ul>
          <% end %>
        </div>
      </div>

      <div class="mt-4">
        <h3 class="text-sm text-zinc-500 mb-1">Region State</h3>
        <pre class="text-xs leading-5 overflow-x-auto p-2 rounded bg-black/5 dark:bg-white/5">
          <%= inspect(@state_map, pretty: true, width: 100, limit: :infinity) %>
        </pre>
      </div>
    </div>
    """
  end

  # --- local helpers used by the summary -------------------------------------

  defp default_subtitle(:lifg), do: "Competitive Sense Selection"
  defp default_subtitle(_), do: "Module Summary"

  defp default_region_desc(:lifg),
    do:
      "Selects the winning sense per token from si.sense_candidates with boundary guards and " <>
        "scoring (lex_fit, rel_prior, activation, intent_bias)."

  defp default_region_desc(_), do: "Overview of the selected module."
end
