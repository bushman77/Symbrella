defmodule Llm.MixProject do
  use Mix.Project

  def project do
    [
      app: :llm,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      # {:sibling_app_in_umbrella, in_umbrella: true}
      {:tesla, "~> 1.8"},
      {:jason, "~> 1.4"},
      # If you don’t already depend on Finch from another umbrella app,
      # keep it here so the adapter is available at compile/runtime:
      {:finch, "~> 0.18"}
    ]
  end
end
