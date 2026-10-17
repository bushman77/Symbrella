defmodule SymbrellaWeb.BrainReportController do
  @moduledoc """
  Native text export for the Brain diagnostics page. Works without JavaScript.
  """
  use SymbrellaWeb, :controller

  alias SymbrellaWeb.BrainReport

  def show(conn, params) do
    region = BrainReport.resolve_region(Map.get(params, "region"))
    report = BrainReport.generate(region)

    conn =
      conn
      |> put_resp_content_type("text/plain", "utf-8")
      |> put_resp_header("cache-control", "no-store, private")
      |> put_resp_header("x-content-type-options", "nosniff")

    conn =
      if Map.get(params, "download") == "1" do
        time = DateTime.utc_now() |> DateTime.to_date() |> Date.to_iso8601()
        filename = "symbrella-brain-#{region}-#{time}.txt"
        put_resp_header(conn, "content-disposition", "attachment; filename=\"#{filename}\"")
      else
        put_resp_header(conn, "content-disposition", "inline")
      end

    send_resp(conn, 200, report)
  end
end
