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

  # No Application module here — nothing auto-starts.
  def application do
    [
      extra_applications: [:logger, :ssl]
    ]
  end

  def deps do
    [
      {:tesla, "~> 1.9"},
      {:finch, "~> 0.17"},
      {:jason, "~> 1.4"},
      {:castore, "~> 1.0"}
    ]
  end
end

