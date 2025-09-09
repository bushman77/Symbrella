defmodule SymbrellaWeb.PageController do
  use SymbrellaWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
