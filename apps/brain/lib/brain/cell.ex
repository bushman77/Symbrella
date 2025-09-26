defmodule Brain.Cell do
  @moduledoc "Runtime neuron process. GenServer state IS %Db.BrainCell{}."
  use GenServer
  alias Db.BrainCell, as: Schema

  @registry Brain.Registry
  @type state :: Schema.t()

  # ---------- Child spec ----------
  def child_spec(arg) do
    id =
      case arg do
        %Schema{id: i} -> i
        %{} = m -> Map.fetch!(m, :id)
        i when is_binary(i) -> i
      end

    %{
      id: {:cell, id},
      start: {__MODULE__, :start_link, [arg]},
      restart: :transient
    }
  end

  # ---------- Public API ----------
  @spec start_link(Schema.t() | map()) :: GenServer.on_start()
  def start_link(%Schema{id: id} = cell),
    do: GenServer.start_link(__MODULE__, cell, name: via(id))

  def start_link(%{} = attrs) do
    id = Map.fetch!(attrs, :id)
    schema = cast_map_to_schema(attrs)
    GenServer.start_link(__MODULE__, schema, name: via(id))
  end

  def start(%Schema{} = cell),
    do: DynamicSupervisor.start_child(Brain.CellSup, {__MODULE__, cell})

  def get(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> GenServer.call(pid, :get_state)
      _ -> nil
    end
  end

  def get_state(pid), do: GenServer.call(pid, :get_state)
  def fire(pid, amount \\ 0.1),           do: GenServer.cast(pid, {:fire, amount})
  def apply_substance(pid, :dopamine, a), do: GenServer.cast(pid, {:apply_nt, :dopamine, a})
  def apply_substance(pid, :serotonin, a),do: GenServer.cast(pid, {:apply_nt, :serotonin, a})
  def attenuate(pid, factor),             do: GenServer.cast(pid, {:attenuate, factor})
  def hydrate(pid, %Schema{} = schema),   do: GenServer.cast(pid, {:hydrate, schema})

  # ---------- GenServer ----------
  @impl true
  def init(%Schema{} = schema) do
    # No register/deregister casts; Brain doesn’t handle them
    {:ok, schema}
  end

  @impl true
  def handle_call(:get_state, _from, %Schema{} = s), do: {:reply, s, s}

  @impl true
  def handle_call(:status, _from, %Schema{} = s) do
    {:reply, %{id: s.id, word: s.word, activation: s.activation}, s}
  end

  @impl true
  def handle_cast(:stop, s), do: {:stop, :normal, s}

  # ← This is what Brain sends after {:activate_cells, …}
  @impl true
  def handle_cast({:activate, payload}, %Schema{} = s) do
    spike = Map.get(payload, :delta, 0.1)
    decay = Map.get(payload, :decay, 0.98)

    new_act = clamp((s.activation || 0.0) * decay + spike)
    new_mod = modulated_activation(new_act, s.dopamine || 0.0, s.serotonin || 0.0)

    # Report back so Brain updates its active_cells map
    GenServer.cast(Brain, {:activation_report, s.id, new_act})
    {:noreply, %Schema{s | activation: new_act, modulated_activation: new_mod}}
  end

  @impl true
  def handle_cast({:fire, amount}, %Schema{} = s) when is_number(amount) do
    new_act = clamp((s.activation || 0.0) + amount)
    {:noreply, %Schema{s |
      activation: new_act,
      modulated_activation: modulated_activation(new_act, s.dopamine || 0.0, s.serotonin || 0.0)}}
  end

  @impl true
  def handle_cast({:apply_nt, :dopamine, amt}, %Schema{} = s) when is_number(amt) do
    now = DateTime.utc_now()
    nd  = clamp((s.dopamine || 0.0) + amt)
    {:noreply, %Schema{s |
      dopamine: nd,
      modulated_activation: modulated_activation(s.activation || 0.0, nd, s.serotonin || 0.0),
      last_dose_at: now, last_substance: "dopamine"}}
  end

  @impl true
  def handle_cast({:apply_nt, :serotonin, amt}, %Schema{} = s) when is_number(amt) do
    now = DateTime.utc_now()
    ns  = clamp((s.serotonin || 0.0) + amt)
    {:noreply, %Schema{s |
      serotonin: ns,
      modulated_activation: modulated_activation(s.activation || 0.0, s.dopamine || 0.0, ns),
      last_dose_at: now, last_substance: "serotonin"}}
  end

  @impl true
  def handle_cast({:attenuate, factor}, %Schema{} = s)
      when is_number(factor) and factor >= 0.0 and factor <= 1.0 do
    base =
      cond do
        is_number(s.modulated_activation) -> s.modulated_activation
        is_number(s.activation)           -> s.activation
        true                              -> 0.0
      end

    {:noreply, %Schema{s | modulated_activation: clamp01(base * factor)}}
  end

  @impl true
  def handle_cast({:hydrate, %Schema{} = lex}, %Schema{} = s) do
    fields = [:definition, :example, :gram_function, :synonyms, :antonyms, :semantic_atoms,
              :embedding, :token_id, :position, :connections]

    merged =
      Enum.reduce(fields, s, fn k, acc ->
        newv = Map.get(lex, k)
        oldv = Map.get(acc, k)
        cond do
          is_nil(newv) -> acc
          newv == [] and is_list(oldv) -> acc
          true -> Map.put(acc, k, newv)
        end
      end)

    {:noreply, merged}
  end

  # ---------- Helpers ----------
  defp cast_map_to_schema(attrs) do
    defaults = Map.from_struct(%Schema{})
    struct!(Schema, Map.merge(defaults, attrs))
  end

  def modulated_activation(a, d, s), do: Float.round(a + d - s, 4)
  defp clamp(v) when v < 0.0, do: 0.0
  defp clamp(v) when v > 1.0, do: 1.0
  defp clamp(v), do: v
  defp clamp01(x) when x < 0.0, do: 0.0
  defp clamp01(x) when x > 1.0, do: 1.0
  defp clamp01(x), do: x
  defp via(id), do: {:via, Registry, {@registry, id}}
end

