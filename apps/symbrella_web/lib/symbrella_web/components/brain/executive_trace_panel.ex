defmodule SymbrellaWeb.Components.Brain.ExecutiveTracePanel do
  @moduledoc """
  Read-only, turn-correlated executive-control observability.

  Evidence has already been sanitized by CognitiveTraceStore. The panel
  distinguishes a planner's computed result, merged control values, controls
  used by a *shadow* comparison, and the unchanged authoritative live action.
  No runtime edges or execution effects are inferred from this presentation.
  """

  use SymbrellaWeb, :html

  @executive_regions [:prefrontal, :dlpfc, :fpc, :dmpfc, :vmpfc, :salience]

  @doc "Summarize only explicitly captured Phase 7 receipts in the selected turn."
  def report(%{events: events}) when is_list(events) do
    prefrontal = last_evidence(events, :prefrontal_control)
    action = last_evidence(events, :action_selection)

    %{
      has_prefrontal?: not is_nil(prefrontal),
      has_action?: not is_nil(action),
      available?: is_map(prefrontal) and Map.get(prefrontal, :available) == true,
      planners: if(is_map(prefrontal), do: Map.get(prefrontal, :planners, []), else: []),
      controls: if(is_map(prefrontal), do: Map.get(prefrontal, :controls, %{}), else: %{}),
      feedback_quality:
        if(is_map(prefrontal), do: Map.get(prefrontal, :feedback_quality, :unknown), else: :unknown),
      shadow: action
    }
  end

  def report(_), do: %{
    has_prefrontal?: false, has_action?: false, available?: false,
    planners: [], controls: %{}, feedback_quality: :unknown, shadow: nil
  }

  @doc "Return a planner's explicit observation, not a conceptual relationship."
  def planner(report, region) when is_map(report) and region in @executive_regions do
    Enum.find(Map.get(report, :planners, []), &(&1.region == region))
  end

  def planner(_, _), do: nil

  @doc "The canonical merged values owned by this planner."
  def merged_values(report, %{merged_keys: keys}) when is_map(report) and is_list(keys) do
    controls = Map.get(report, :controls, %{})

    for key <- keys,
        value = Map.get(controls, key),
        not is_nil(value),
        do: %{key: key, value: value}
  end

  def merged_values(_, _), do: []

  @doc "Keys explicitly applied to scoring in the shadow selector, by region."
  def shadow_used(report, region) when is_map(report) do
    report
    |> Map.get(:shadow)
    |> case do
      %{applied_sources: sources} when is_list(sources) ->
        for %{key: key, region: source_region} <- sources,
            source_region == region, do: key

      _ -> []
    end
  end

  defp last_evidence(events, stage) do
    events
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{stage: ^stage, status: :ok, evidence: %{} = evidence} -> evidence
      _ -> nil
    end)
  end

  defp region_name(:dlpfc), do: "DLPFC"
  defp region_name(:fpc), do: "FPC"
  defp region_name(:dmpfc), do: "DMPFC"
  defp region_name(:vmpfc), do: "VMPFC"
  defp region_name(:salience), do: "Salience"
  defp region_name(:prefrontal), do: "Prefrontal"
  defp region_name(other), do: to_string(other)

  defp computation(:produced), do: "computed"
  defp computation(:no_opinion), do: "called · no opinion"
  defp computation(:unavailable), do: "unavailable · not called"
  defp computation(:failed), do: "called · failed"
  defp computation(:rejected), do: "called · invalid output"
  defp computation(_), do: "not observed"

  attr :trace, :any, default: nil

  def panel(assigns) do
    assigns = assign(assigns, :executive, report(assigns.trace))

    ~H"""
    <section id="atlas-executive-observability"
      class="mt-5 rounded-xl border border-violet-700/60 bg-zinc-900/75 p-4 sm:p-5"
      aria-label="Turn-correlated prefrontal executive observability">
      <h3 class="text-lg font-semibold text-violet-200">
        Executive control · selected turn
      </h3>
      <p class="mt-2 text-sm text-zinc-300">
        These observations come from Phase 7 of this instrumented turn.
        A computed planner result is not proof of a live decision change.
        Only the shadow selector uses the proposed score adjustments.
      </p>

      <p :if={not @executive.has_prefrontal?}
        id="atlas-executive-empty" class="mt-4 text-sm text-zinc-400">
        No prefrontal execution receipt in the selected turn.
        It may have used a different route, or been recorded before this instrumentation.
      </p>

      <div :if={@executive.has_prefrontal?} class="mt-4">
        <p class="text-xs text-zinc-300">
          Prefrontal: {if @executive.available?, do: "computed", else: "not available"} ·
          Outcome feedback: {@executive.feedback_quality}
        </p>
        <div id="atlas-executive-planners" class="mt-3 grid gap-3 sm:grid-cols-2">
          <article :for={entry <- @executive.planners}
            data-planner-region={entry.region}
            data-planner-status={entry.status}
            class="rounded-lg border border-violet-800/70 bg-zinc-950/80 p-3">
            <div class="flex flex-wrap items-center justify-between gap-2">
              <strong class="text-sm text-violet-100">{region_name(entry.region)}</strong>
              <span class="text-xs text-zinc-300">{computation(entry.status)}</span>
            </div>
            <p class="mt-2 text-xs text-zinc-400">
              Proposed keys: {if entry.proposed_keys == [], do: "none",
                else: Enum.join(entry.proposed_keys, ", ")}
            </p>
            <p :if={entry.superseded_keys != []} class="mt-1 text-xs text-rose-200">
              Superseded or not merged: {Enum.join(entry.superseded_keys, ", ")}
            </p>
            <p class="mt-1 text-xs text-emerald-200">
              Merged controls:
              {if merged_values(@executive, entry) == [],
                do: "none",
                else: Enum.map_join(merged_values(@executive, entry), " · ",
                  fn %{key: key, value: value} -> "#{key}: #{value}" end)}
            </p>
            <p class="mt-1 text-xs text-amber-200">
              Used in shadow scoring:
              {if shadow_used(@executive, entry.region) == [],
                do: "none",
                else: Enum.join(shadow_used(@executive, entry.region), ", ")}
            </p>
          </article>
        </div>
        <p :if={@executive.planners == []} class="mt-3 text-sm text-zinc-400">
          Prefrontal stage returned without a recorded planner report.
        </p>
      </div>

      <div :if={is_map(@executive.shadow)} id="atlas-executive-shadow"
        class="mt-4 rounded-lg border border-zinc-700 bg-zinc-950/80 p-3">
        <h4 class="font-semibold text-cyan-200">Live vs shadow action</h4>
        <p class="mt-2 text-sm text-zinc-200">
          Live action: <strong>{@executive.shadow.live_action}</strong> ·
          Shadow proposal: <strong>{@executive.shadow.shadow_action}</strong>
        </p>
        <p class="mt-1 text-xs text-zinc-300">
          Shadow status: {@executive.shadow.shadow_status} ·
          Changed recommendation: {@executive.shadow.changed}
        </p>
        <p class="mt-1 text-xs text-amber-200">
          Applied keys:
          {if @executive.shadow.applied_keys == [],
            do: "none",
            else: Enum.join(@executive.shadow.applied_keys, ", ")}
        </p>
        <p class="mt-2 text-xs text-zinc-400">
          These are proposed comparisons only. The original live selector result
          remains authoritative, including command permissions.
        </p>
        <ol :if={@executive.shadow.adjustments != []}
          id="atlas-executive-scores" class="mt-3 space-y-1 text-xs text-zinc-300">
          <li :for={candidate <- @executive.shadow.adjustments}
            class="flex flex-wrap justify-between gap-2">
            <span>{candidate.action}</span>
            <span class="font-mono">
              {Float.round(candidate.baseline * 1.0, 3)} →
              {Float.round(candidate.proposed * 1.0, 3)}
              ({Float.round(candidate.delta * 1.0, 3)})
            </span>
          </li>
        </ol>
      </div>
      <p :if={not @executive.has_action?} class="mt-3 text-xs text-zinc-400">
        No action-selection receipt in the selected turn.
      </p>
    </section>
    """
  end

  attr :executive, :map, required: true
  attr :region, :atom, required: true

  def inspector(assigns) do
    assigns =
      assigns
      |> assign(:entry, planner(assigns.executive, assigns.region))
      |> assign(:relevant?, assigns.region in @executive_regions)

    ~H"""
    <section :if={@relevant?} id="atlas-region-executive"
      class="mt-5 rounded-lg border border-violet-800/70 bg-zinc-950/70 p-3">
      <h4 class="font-semibold text-violet-200">Executive evidence · selected turn</h4>
      <div :if={@region == :prefrontal} class="mt-2 text-xs text-zinc-300">
        <p>Prefrontal stage:
          {if @executive.has_prefrontal?, do: "observed", else: "not observed"}
        </p>
        <p class="mt-1">Planner receipts: {length(@executive.planners)}</p>
        <ul id="atlas-prefrontal-planner-summary" class="mt-2 space-y-1">
          <li :for={planner <- @executive.planners}
            class="flex flex-wrap justify-between gap-2">
            <span>{region_name(planner.region)}</span>
            <span>{computation(planner.status)}</span>
          </li>
        </ul>
        <p class="mt-2">Live vs shadow:
          {case @executive.shadow do
            %{shadow_status: status} -> status
            _ -> "not recorded"
          end}
        </p>
        <p :if={is_map(@executive.shadow)} class="mt-1">
          Live {@executive.shadow.live_action} ·
          shadow {@executive.shadow.shadow_action} ·
          changed proposal {@executive.shadow.changed}
        </p>
      </div>
      <div :if={@entry != nil} class="mt-2 text-xs">
        <p class="text-zinc-200">
          {computation(@entry.status)} · {@entry.reason}
        </p>
        <p class="mt-1 text-emerald-200">
          Merged:
          {if merged_values(@executive, @entry) == [], do: "none",
            else: Enum.map_join(merged_values(@executive, @entry), " · ",
              fn %{key: key, value: value} -> "#{key}: #{value}" end)}
        </p>
        <p class="mt-1 text-amber-200">
          Shadow scoring:
          {if shadow_used(@executive, @region) == [],
            do: "not used",
            else: Enum.join(shadow_used(@executive, @region), ", ")}
        </p>
      </div>
      <p :if={@entry == nil and @region != :prefrontal} class="mt-2 text-xs text-zinc-400">
        No planner receipt for this region in the selected turn.
      </p>
    </section>
    """
  end
end
