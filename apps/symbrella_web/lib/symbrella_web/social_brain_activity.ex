defmodule SymbrellaWeb.SocialBrainActivity do
  @moduledoc """
  Short-lived, memory-only activity signal for the /brain dashboard.

  Reports that real read-only lexical/semantic stages ran, not that neurons
  fired or that Colony statements were learned. No external text, user names,
  tokens, or cognitive reports are retained. The last aggregate expires in
  five minutes and is lost on restart.
  """

  use GenServer

  alias Brain.Bus

  @topic "brain:social"
  @ttl_ms 300_000

  def start_link(_opts), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  def record(report) when is_map(report) do
    if Process.whereis(__MODULE__) do
      GenServer.call(__MODULE__, {:record, summarize(report)})
    else
      :unavailable
    end
  end

  def latest do
    if Process.whereis(__MODULE__), do: GenServer.call(__MODULE__, :latest), else: nil
  end

  @impl true
  def init(_), do: {:ok, nil}

  @impl true
  def handle_call({:record, summary}, _from, _state) do
    stamp = System.monotonic_time(:millisecond)
    activity = Map.put(summary, :recorded_at_ms, System.system_time(:millisecond))
    Bus.broadcast(@topic, {:social_cognition, activity})

    :telemetry.execute(
      [:symbrella_web, :social, :analysis],
      Map.take(activity, [:segments, :lifg_choices, :atl_winners,
        :supported_interpretations, :participants, :relations]),
      %{source: :colony_external, mode: :read_only_shadow}
    )

    Process.send_after(self(), {:expire, stamp}, @ttl_ms)
    {:reply, :ok, %{activity: activity, stamp: stamp}}
  end

  @impl true
  def handle_call(:latest, _from, state) do
    activity =
      case state do
        %{activity: activity, stamp: stamp} ->
          if System.monotonic_time(:millisecond) - stamp < @ttl_ms, do: activity, else: nil

        _ -> nil
      end

    {:reply, activity, state}
  end

  @impl true
  def handle_info({:expire, stamp}, %{stamp: stamp}) do
    Bus.broadcast(@topic, {:social_cognition, nil})
    {:noreply, nil}
  end

  def handle_info({:expire, _}, state), do: {:noreply, state}

  defp summarize(report) do
    segments = Map.get(report, :segments, [])
    discourse = Map.get(report, :discourse) || %{}
    conversation = Map.get(report, :conversation) || %{}

    %{
      source: :colony_external,
      mode: :read_only_shadow,
      segments: length(segments),
      lifg_choices: count(segments, [:result, :lifg, :chosen_count]),
      atl_winners: count(segments, [:result, :atl, :winner_count]),
      supported_interpretations: length(Map.get(discourse, :interpretations, [])),
      participants: length(Map.get(conversation, :participants, [])),
      relations: length(Map.get(conversation, :relations, []))
    }
  end

  defp count(segments, keys) do
    Enum.reduce(segments, 0, fn segment, acc ->
      case get_in(segment, keys) do
        value when is_integer(value) and value >= 0 -> acc + value
        _ -> acc
      end
    end)
  end
end
