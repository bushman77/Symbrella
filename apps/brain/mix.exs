defmodule Brain.MixProject do
  use Mix.Project

  def project do
    [
      app: :brain,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
      # note: no :mod here; Symbrella supervises Brain
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:stream_data, "~> 0.6", only: :test},
      {:benchee, "~> 1.3", only: :dev},
      {:core, in_umbrella: true},
      {:db, in_umbrella: true}
    ]
  end
end
