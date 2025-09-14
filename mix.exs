defmodule Symbrella.Umbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
      # no preferred_cli_env — single DB setup keeps it simple
    ]
  end

  # Root-only deps (not shared with child apps)
  defp deps do
    [
      # lets the umbrella root run `mix format` on ~H/.heex files
      {:phoenix_live_view, ">= 0.0.0"}
    ]
  end

  # Handy umbrella-wide tasks
  defp aliases do
    [
      {:setup, ["deps.get"]},

      # database lifecycle (Repo = Db), no seeds here
      {:"db.setup",   ["ecto.create -r Db", "ecto.migrate -r Db"]},
      {:"db.reset",   ["ecto.drop -r Db", "ecto.create -r Db", "ecto.migrate -r Db"]},
      {:"db.migrate", ["ecto.migrate -r Db"]},
      {:"db.rollback", ["ecto.rollback -r Db"]},
      {:"db.migrations", ["ecto.migrations -r Db"]},

      # convenience alias without a dot in the name
      {:dbreset, ["db.reset"]},

      # assets (use your existing Tailwind/Esbuild profiles)
      {:"assets.build",  ["tailwind default", "esbuild default"]},
      {:"assets.deploy", ["tailwind default --minify", "esbuild default --minify", "phx.digest"]},

      # tests (no seeding)
      {:test, [
        "ecto.create -r Db --quiet",
        "ecto.migrate -r Db --quiet",
        "test"
      ]}
    ]
  end
end

