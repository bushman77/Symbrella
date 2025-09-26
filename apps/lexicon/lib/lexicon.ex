defmodule Lexicon do
  @moduledoc """
  Minimal internal GenServer.
  No public API, no handlers—just lifecycle.
  """
  use GenServer
  use Tesla

  plug Tesla.Middleware.BaseUrl, "https://api.dictionaryapi.dev/api/v2/entries/en/"
  plug Tesla.Middleware.JSON
  plug Tesla.Middleware.Timeout, timeout: 5_000

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  @spec init(term()) :: {:ok, %{}}
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_call({:fetch_word, word}, _from, state) do
    response = get("#{URI.encode(word)}")
    {:reply, response, state}
  end

@doc "Compat stub for the Mix scaffold test."
  def hello, do: :world
end

