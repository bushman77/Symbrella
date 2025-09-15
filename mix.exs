defmodule Symbrella.Umbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  # Root-only deps (not shared with child apps)
  defp deps do
    [
      {:phoenix_live_view, ">= 0.0.0"}
    ]
  end

  # ---------- Helpers used by aliases ----------

  # We delete both potential locations to be safe:
  # 1) runtime build priv: _build/<env>/lib/core/priv/negcache
  # 2) source priv: apps/core/priv/negcache
  def negcache_dirs do
    env = Atom.to_string(Mix.env())
    root = File.cwd!()

    [
      Path.join([root, "_build", env, "lib", "core", "priv", "negcache"]),
      Path.join([root, "apps", "core", "priv", "negcache"])
    ]
  end

  def clean_negcache(_args) do
    for dir <- negcache_dirs() do
      File.rm_rf!(dir)
      File.mkdir_p!(dir)
      Mix.shell().info("✔ negcache cleared: " <> dir)
    end
  end

  # ---------- Umbrella-wide tasks ----------
  defp aliases do
    [
      {:"negcache.clean", [&__MODULE__.clean_negcache/1]},
      {:"cache.nuke",     [&__MODULE__.clean_negcache/1]},

      # DB lifecycle (Repo = Db) + negcache wipe
      {:"db.setup",   ["ecto.create -r Db", "ecto.migrate -r Db", "negcache.clean"]},
      {:"db.reset",   ["ecto.drop -r Db", "ecto.create -r Db", "ecto.migrate -r Db", "negcache.clean"]},
      {:"db.migrate", ["ecto.migrate -r Db"]},
      {:"db.rollback", ["ecto.rollback -r Db"]},
      {:"db.migrations", ["ecto.migrations -r Db"]},

      # convenience alias without a dot in the name
      {:dbreset, ["db.reset"]},

      # assets
      {:"assets.build",  ["tailwind default", "esbuild default"]},
      {:"assets.deploy", ["tailwind default --minify", "esbuild default --minify", "phx.digest"]},

      # tests — keep cache clean so negatives don't bleed between runs
      {:test, [
        "ecto.create -r Db --quiet",
        "ecto.migrate -r Db --quiet",
        "negcache.clean",
        "test"
      ]}
    ]
  end
end

