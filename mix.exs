defmodule Symbrella.Umbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      listeners: [Phoenix.CodeReloader]
    ]
  end

  def cli do
    [
      preferred_envs: [precommit: :test]
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options.
  #
  # Dependencies listed here are available only for this project
  # and cannot be accessed from applications inside the apps/ folder.
  defp deps do
    [
      # Required to run "mix format" on ~H/.heex files from the umbrella root
      {:phoenix_live_view, ">= 0.0.0"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  #
  # Aliases listed here are available only for this project
  # and cannot be accessed from applications inside the apps/ folder.

# umbrella mix.exs
defp aliases do
  [
    setup: ["deps.get"],
    "assets.build": [
      "do --app symbrella_web cmd --cd assets node node_modules/tailwindcss/lib/cli.js " <>
        "-c tailwind.config.js -i css/app.css -o ../priv/static/assets/app.css",
      "esbuild symbrella_web"
    ],
    "assets.deploy": [
      "do --app symbrella_web cmd --cd assets node node_modules/tailwindcss/lib/cli.js " <>
        "-c tailwind.config.js -i css/app.css -o ../priv/static/assets/app.css --minify",
      "esbuild symbrella_web --minify",
      "phx.digest"
    ]
  ]
end

end
