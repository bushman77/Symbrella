defmodule SymbrellaWeb.Router do
  use SymbrellaWeb, :router
  import Phoenix.LiveView.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug SymbrellaWeb.Plugs.WebAuth
    plug SymbrellaWeb.Plugs.ChatSession
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

    # Route literal downloads before the parameterized /brain/:region route.
    get "/brain/report.txt", BrainReportController, :show

    # The HTTP plug protects initial navigation; on_mount independently
    # protects LiveView sessions reconnecting through the socket transport.
    live_session :protected, on_mount: [{SymbrellaWeb.Plugs.WebAuth, :require_authenticated}] do
      live "/", HomeLive, :index
      live "/episodes", EpisodesLive
      live "/semantic", SemanticLive
      # Read-only Colony browser stays behind the existing authenticated session.
      live "/social", SocialLive, :index
      live "/social/:post_id", SocialLive, :show
      live "/brain", BrainLive
      live "/brain/:region", BrainLive
    end


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
