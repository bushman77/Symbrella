# apps/brain/lib/brain/region.ex
defmodule Brain.Region do
  @moduledoc """
  Region GenServer helper macro.

  Extracted from `Brain` to keep the Brain coordinator smaller while preserving
  the existing `use Brain, region: :xyz` API (re-exported by Brain).

  Key compatibility rule:
    * **`init/1` receives a keyword list by default** (as before), so existing
      region modules that do `Keyword.get(opts, ...)` do not break.
    * The macro still normalizes and stores `:opts` as a map in the region state.

  This macro provides:
    - start_link/1 with optional :name (module/atom or registered name)
    - child_spec/1
    - default init/call/cast/info handlers
    - defoverridable for easy customization

  Default region state includes:
    - :region
    - :opts (map)
    - :stats (map)
    - :assistant (from `Brain.Config.assistant/0`, when available)
  """

  @doc false
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
  def opts_to_init_arg(opts) when is_list(opts), do: opts
  def opts_to_init_arg(%{} = m), do: Map.to_list(m)
  def opts_to_init_arg(_), do: []

  @doc false
  def start_name(default_name, %{} = opts_map) do
    Map.get(opts_map, :name) || Map.get(opts_map, "name") || default_name
  end

  @doc false
  def base_state(region, opts) do
    opts_map = opts_to_map(opts)

    assistant =
      try do
        if Code.ensure_loaded?(Brain.Config) and function_exported?(Brain.Config, :assistant, 0) do
          Brain.Config.assistant()
        else
          %{}
        end
      rescue
        _ -> %{}
      catch
        _, _ -> %{}
      end

    %{
      region: region,
      opts: opts_map,
      stats: %{},
      assistant: assistant
    }
  end

  @doc false
  defmacro __before_compile__(env) do
    # Define region/0 only if the target module doesn't already define it.
    if Module.defines?(env.module, {:region, 0}) do
      quote do
      end
    else
      quote location: :keep do
        @doc false
        def region, do: @region
      end
    end
  end

  @doc """
  Use inside a module to make it a region GenServer.

      defmodule Brain.LIFG do
        use Brain, region: :lifg
      end
  """
  defmacro __using__(opts) do
    region = Keyword.fetch!(opts, :region)
    caller = __CALLER__.module

    # Guard against accidental double-use (e.g. `use Brain` + `use Brain.Region`).
    # Second expansion becomes a no-op to avoid duplicate function clauses.
    if Module.get_attribute(caller, :__brain_region_macro_used__) do
      quote location: :keep do
      end
    else
      quote location: :keep, bind_quoted: [region: region] do
        @__brain_region_macro_used__ true

        use GenServer
        @region region
        @before_compile Brain.Region

        @doc false
        def start_link(opts \\ []) do
          opts_map = Brain.Region.opts_to_map(opts)
          name = Brain.Region.start_name(__MODULE__, opts_map)
          init_arg = Brain.Region.opts_to_init_arg(opts)
          GenServer.start_link(__MODULE__, init_arg, name: name)
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
        def handle_call(other, _from, state),
          do: {:reply, {:error, {:unknown_call, other}}, state}

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
end
