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
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :hackney]
    ]
  end

  defp deps do
    [
      {:hackney, "~> 1.25"},
      {:jason, "~> 1.4"}
    ]
  end
end
