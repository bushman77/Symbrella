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

  # Supervise ephemeral Colony JWTs in RAM. A separately supervised worker
  # may provision credentials at startup only for an explicitly named NEW agent;
  # otherwise network calls occur only on authorized Colony reads/writes.
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
