defmodule SymbrellaWeb.SocialBrainActivity do
  @moduledoc """
  Short-lived, memory-only activity signal for the /brain dashboard.

  Reports read-only lexical/semantic stages and operator-initiated transient
  neuron activation requests, without claiming that Colony statements were learned. No external text, user names,
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
    # A diagnostic snapshot at completion avoids confusing a late-opened
    # /brain chart (whose samples start at mount) with a failed neuron bridge.
    # This is an all-source snapshot, NOT a receipt attributing individual
    # Brain.Cell activations to Colony.
    runtime_neurons =
      case SymbrellaWeb.BrainRuntime.activation_snapshot() do
        {:ok, %{} = snapshot} ->
          Enum.count(snapshot, fn {_id, activation} ->
            is_number(activation) and activation > 0
          end)

        _ ->
          nil
      end

    activity =
      summary
      |> Map.put(:recorded_at_ms, System.system_time(:millisecond))
      |> Map.put(:runtime_active_neurons_at_completion, runtime_neurons)

    Bus.broadcast(@topic, {:social_cognition, activity})

    # Emit exactly one trace per completed thread rather than one per
    # sentence, so the bounded Brain blackboard retains useful context.
    :telemetry.execute(
      [:core, :social, :cognitive_shadow],
      Map.take(activity, [:segments, :lifg_choices, :atl_winners, :syntax_frames,
        :dictionary_rows, :pos_covered_tokens, :processed_tokens,
        :supported_interpretations, :neuron_stimuli_issued])
      |> Map.merge(%{
        original_post_sentences: activity.source_breakdown.original_post.sentences,
        original_post_stimuli: activity.source_breakdown.original_post.neuron_stimuli,
        reply_sentences: activity.source_breakdown.replies.sentences,
        reply_stimuli: activity.source_breakdown.replies.neuron_stimuli
      }),
      %{
        source: :colony_external, mode: :read_only_shadow,
        analysis_scope: :thread_aggregate,
        sentence_status: if(activity.supported_interpretations > 0,
          do: :partially_interpreted, else: :not_yet_interpreted),
        provenance: :external_unverified
      }
    )

    :telemetry.execute(
      [:core, :social, :neuron_observation],
      %{stimuli_issued: activity.neuron_stimuli_issued,
        inspected_segments: activity.segments,
        observation_disabled: Map.get(activity.neuron_observation_statuses, :disabled, 0),
        brain_unavailable: Map.get(activity.neuron_observation_statuses, :brain_unavailable, 0),
        no_attested_targets: Map.get(activity.neuron_observation_statuses, :no_attested_targets, 0),
        stimuli_issued_segments: Map.get(activity.neuron_observation_statuses, :stimuli_issued, 0)},
      %{source: :colony_external, mode: :transient_neural_observation,
        analysis_scope: :thread_aggregate}
    )

    :telemetry.execute(
      [:symbrella_web, :social, :analysis],
      Map.take(activity, [:segments, :lifg_choices, :atl_winners,
        :supported_interpretations, :participants, :relations,
        :neuron_stimuli_issued]),
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
    sections = Map.get(report, :sections) || %{}

    post_segments =
      case get_in(sections, [:original_post, :segments]) do
        items when is_list(items) -> items
        _ -> Enum.filter(segments, &source_post?/1)
      end

    reply_sources =
      case Map.get(sections, :replies) do
        items when is_list(items) ->
          items

        _ ->
          segments
          |> Enum.filter(&(not source_post?(&1)))
          |> Enum.group_by(fn segment ->
            Map.get(segment, :source_index) || Map.get(segment, :source_label) ||
              Map.get(segment, :label, "unknown")
          end)
          |> Enum.map(fn {key, source_segments} ->
            %{index: key, segments: source_segments}
          end)
      end

    reply_segments = Enum.flat_map(reply_sources, &Map.get(&1, :segments, []))

    scopes = %{
      original_post: source_metrics(post_segments),
      replies: source_metrics(reply_segments)
    }

    reply_breakdown =
      Enum.map(reply_sources, fn reply ->
        %{
          reply_index: Map.get(reply, :index),
          metrics: source_metrics(Map.get(reply, :segments, []))
        }
      end)

    %{
      source: :colony_external,
      mode: :read_only_shadow,
      source_breakdown: scopes,
      reply_breakdown: reply_breakdown,
      neuron_stimuli_issued: count(segments, [:result, :brain_observation, :stimuli_issued]),
      neuron_observation_statuses:
        segments
        |> Enum.map(&get_in(&1, [:result, :brain_observation, :status]))
        |> Enum.reject(&is_nil/1)
        |> Enum.frequencies(),
      processed_tokens: count(segments, [:result, :token_count]),
      pos_covered_tokens: count(segments, [:result, :syntax, :audit, :pos_covered_tokens]),
      dictionary_rows: count(segments, [:result, :syntax, :audit, :ltm_source_rows]),
      syntax_frames: Enum.reduce(segments, 0, fn segment, acc ->
        acc + length(get_in(segment, [:result, :syntax, :signatures]) || [])
      end),
      segments: length(segments),
      lifg_choices: count(segments, [:result, :lifg, :chosen_count]),
      atl_winners: count(segments, [:result, :atl, :winner_count]),
      supported_interpretations: length(Map.get(discourse, :interpretations, [])),
      participants: length(Map.get(conversation, :participants, [])),
      relations: length(Map.get(conversation, :relations, []))
    }
  end

  # Keep post and reply metrics apart; a total never implies that a reply
  # was asserted by the original author. No usernames or raw text are retained.
  defp source_metrics(segments) do
    %{
      sentences: length(segments),
      lifg_choices: count(segments, [:result, :lifg, :chosen_count]),
      atl_winners: count(segments, [:result, :atl, :winner_count]),
      neuron_stimuli: count(segments, [:result, :brain_observation, :stimuli_issued]),
      interpreted_sentences: Enum.count(segments, fn segment ->
        get_in(segment, [:result, :sentence_meaning, :status]) in
          [:structurally_interpreted, :interpreted, :understood, :resolved]
      end)
    }
  end

  defp source_post?(%{source_type: :original_post}), do: true
  defp source_post?(%{source_type: :reply}), do: false
  defp source_post?(%{source_label: "Original post"}), do: true

  defp source_post?(%{label: label}) when is_binary(label),
    do: String.starts_with?(label, "Original post")

  defp source_post?(_), do: false

  defp count(segments, keys) do
    Enum.reduce(segments, 0, fn segment, acc ->
      case get_in(segment, keys) do
        value when is_integer(value) and value >= 0 -> acc + value
        _ -> acc
      end
    end)
  end
end
