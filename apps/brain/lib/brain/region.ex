defmodule Brain.Region do
  @moduledoc """
  Region GenServer helper macro.

  Extracted from `Brain` to keep the Brain coordinator smaller while preserving
  the existing `use Brain, region: :xyz` API (re-exported by Brain).

  This macro provides:
    - start_link/1 with optional :name (atom/module or registered name)
    - child_spec/1
    - default init/call/cast/info handlers
    - defoverridable for easy customization

  Default region state includes:
    - :region
    - :opts (map)
    - :stats (map)
    - :assistant (from `Brain.Config.assistant/0` when available)
  """

  @doc false
  @spec opts_to_map(map() | keyword() | any()) :: map()
  def opts_to_map(%{} = m), do: m

  def opts_to_map(opts) when is_list(opts) do
    try do
      Enum.into(opts, %{})
    rescue
      _ -> %{}
    end
  end

  def opts_to_map(_), do: %{}

  @doc false
  @spec opt(map() | keyword() | any(), atom() | binary(), any()) :: any()
  def opt(opts, key, default \\ nil) do
    m = opts_to_map(opts)

    cond do
      is_atom(key) ->
        Map.get(m, key) || Map.get(m, Atom.to_string(key)) || default

      is_binary(key) ->
        Map.get(m, key) || Map.get(m, String.to_atom(key)) || default

      true ->
        default
    end
  end

  @doc false
  @spec start_name(atom() | module(), map()) :: any()
  def start_name(default_name, %{} = opts_map) do
    Map.get(opts_map, :name) || Map.get(opts_map, "name") || default_name
  end

  @doc false
  @spec base_state(atom(), map() | keyword() | any()) :: map()
  def base_state(region, opts) do
    opts_map = opts_to_map(opts)

    assistant =
      if Code.ensure_loaded?(Brain.Config) and function_exported?(Brain.Config, :assistant, 0) do
        Brain.Config.assistant()
      else
        nil
      end

    %{
      region: region,
      opts: opts_map,
      stats: %{},
      assistant: assistant
    }
  end

  @doc """
  Use inside a module to make it a region GenServer.

      defmodule Brain.LIFG do
        use Brain, region: :lifg
      end
  """
  defmacro __using__(opts) do
    region = Keyword.fetch!(opts, :region)

    quote location: :keep, bind_quoted: [region: region] do
      use GenServer
      @region region

      @doc false
      def region, do: @region

      @doc false
      def start_link(opts \\ []) do
        opts_map = Brain.Region.opts_to_map(opts)
        name = Brain.Region.start_name(__MODULE__, opts_map)

        # Important: pass `opts` through as-is so regions can accept either a keyword list or a map.
        GenServer.start_link(__MODULE__, opts, name: name)
      end

      @doc false
      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          shutdown: 500
        }
      end

      @impl true
      def init(opts), do: {:ok, Brain.Region.base_state(@region, opts)}

      @impl true
      def handle_call(:status, _from, state), do: {:reply, state, state}

      @impl true
      def handle_cast(_msg, state), do: {:noreply, state}

      @impl true
      def handle_info(_msg, state), do: {:noreply, state}

      defoverridable start_link: 1,
                     child_spec: 1,
                     init: 1,
                     handle_call: 3,
                     handle_cast: 2,
                     handle_info: 2
    end
  end
end

