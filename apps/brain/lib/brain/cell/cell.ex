defmodule Brain.Cell do
  @moduledoc """
  Runtime neuron process. GenServer state IS %Db.BrainCell{}.
  """
  use GenServer
  alias Db.BrainCell, as: Schema

  @registry Brain.Registry

  @type state :: Schema.t()

  # Public API ---------------------------------------------------------

  @spec start_link(Schema.t() | map()) :: GenServer.on_start()
  def start_link(%Schema{id: id} = cell),
    do: GenServer.start_link(__MODULE__, cell, name: via(id))

  def start_link(%{} = attrs) do
    id = Map.fetch!(attrs, :id)
    schema = cast_map_to_schema(attrs)
    GenServer.start_link(__MODULE__, schema, name: via(id))
  end

  @doc "Start under Brain.CellSup."
  def start(%Schema{} = cell),
    do: DynamicSupervisor.start_child(Brain.CellSup, {__MODULE__, cell})

  @doc "Lookup a running cell and return its state (or nil)."
  def get(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> GenServer.call(pid, :get_state)
      _ -> nil
    end
  end

  def get_state(pid), do: GenServer.call(pid, :get_state)

  def fire(pid, amount \\ 0.1), do: GenServer.cast(pid, {:fire, amount})
  def apply_substance(pid, :dopamine, amt),  do: GenServer.cast(pid, {:apply_nt, :dopamine, amt})
  def apply_substance(pid, :serotonin, amt), do: GenServer.cast(pid, {:apply_nt, :serotonin, amt})
  def attenuate(pid, factor), do: GenServer.cast(pid, {:attenuate, factor})

  @doc "Hydrate lexical fields from a full %Db.BrainCell{} (non-destructive to runtime counters)."
  def hydrate(pid, %Schema{} = schema), do: GenServer.cast(pid, {:hydrate, schema})

  # GenServer ----------------------------------------------------------

  @impl true
  def init(%Schema{id: id} = schema) do
    send(Brain, {:cell_started, {id, self()}})
    {:ok, schema}
  end

  @impl true
  def handle_call(:get_state, _from, %Schema{} = s), do: {:reply, s, s}

  @impl true
  def handle_cast({:fire, amount}, %Schema{} = s) when is_number(amount) do
    new_act = clamp(s.activation + amount)
    {:noreply, %Schema{s |
      activation: new_act,
      modulated_activation: modulated_activation(new_act, s.dopamine, s.serotonin)
    }}
  end

  @impl true
  def handle_cast({:apply_nt, :dopamine, amt}, %Schema{} = s) when is_number(amt) do
    now = DateTime.utc_now()
    nd  = clamp(s.dopamine + amt)
    {:noreply, %Schema{s |
      dopamine: nd,
      modulated_activation: modulated_activation(s.activation, nd, s.serotonin),
      last_dose_at: now, last_substance: "dopamine"
    }}
  end

  @impl true
  def handle_cast({:apply_nt, :serotonin, amt}, %Schema{} = s) when is_number(amt) do
    now = DateTime.utc_now()
    ns  = clamp(s.serotonin + amt)
    {:noreply, %Schema{s |
      serotonin: ns,
      modulated_activation: modulated_activation(s.activation, s.dopamine, ns),
      last_dose_at: now, last_substance: "serotonin"
    }}
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
    # Only overlay lexical/content fields; preserve runtime counters.
    fields = [:definition, :example, :gram_function, :synonyms, :antonyms,
              :semantic_atoms, :embedding, :token_id, :position, :connections]

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

  # Helpers ------------------------------------------------------------

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

