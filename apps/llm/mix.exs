# apps/llm/mix.exs
defmodule Llm.MixProject do
  use Mix.Project

  def project do
    [
      app: :llm,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Supervise temporary Colony access tokens in RAM. No network calls
  # occur until an explicit API read or separately authorized social write.
  def application do
    [
      mod: {Llm.Application, []},
      extra_applications: [:logger, :ssl]
    ]
  end

  def deps do
    [
      {:req, "~> 0.5"},
      {:jason, "~> 1.4"},
      {:castore, "~> 1.0"}
    ]
  end
end
