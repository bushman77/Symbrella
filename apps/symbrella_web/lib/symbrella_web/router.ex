defmodule SymbrellaWeb.Router do
  use SymbrellaWeb, :router
  import Phoenix.LiveView.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {SymbrellaWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", SymbrellaWeb do
    pipe_through :browser

    live "/", HomeLive, :index
    live "/brain", BrainLive
    # Optional deep-linking to a specific region (e.g., /brain/lifg)
    live "/brain/:region", BrainLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", SymbrellaWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:symbrella_web, :dev_routes) do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: SymbrellaWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
