defmodule Lexicon.MixProject do
  use Mix.Project

  def project do
    [
      app: :lexicon,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Note: NO :mod here. This stays a plain library with no supervision tree.
  def application do
    [
      extra_applications: [:logger, :tesla, :jason]
    ]
  end

  def deps do
    [
      {:tesla, "~> 1.8"},
      {:jason, "~> 1.4"},
      # If you don’t already depend on Finch from another umbrella app,
      # keep it here so the adapter is available at compile/runtime:
      {:finch, "~> 0.18"}
    ]
  end
end

