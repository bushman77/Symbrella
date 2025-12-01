defmodule Brain.Region do
  @moduledoc """
  Region GenServer helper macro.

  This is extracted from `Brain` to keep the Brain coordinator smaller while
  preserving the existing `use Brain, region: :xyz` API (re-exported by Brain).

  Usage (preferred):

      defmodule Brain.LIFG do
        use Brain, region: :lifg
      end

  This macro provides:
  - start_link/1 with optional :name
  - child_spec/1
  - default init/call/cast/info handlers
  - defoverridable for easy customization
  """

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
      def start_link(opts) do
        name =
          if is_list(opts) do
            Keyword.get(opts, :name, __MODULE__)
          else
            Map.get(Map.new(opts), :name, __MODULE__)
          end

        GenServer.start_link(__MODULE__, opts, name: name)
      end

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
      def init(opts), do: {:ok, %{region: @region, opts: Map.new(opts), stats: %{}}}

      @impl true
      def handle_call(:status, _from, state), do: {:reply, state, state}

      @impl true
      def handle_cast(_msg, state), do: {:noreply, state}

      @impl true
      def handle_info(_msg, state), do: {:noreply, state}

      # 👇 critical line:
      defoverridable start_link: 1,
                     child_spec: 1,
                     init: 1,
                     handle_call: 3,
                     handle_cast: 2,
                     handle_info: 2
    end
  end
end

