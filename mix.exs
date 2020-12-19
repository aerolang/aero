defmodule Aero.MixProject do
  use Mix.Project

  def project do
    [
      app: :aero,
      version: "0.1.0",
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env),
      escript: [main_module: :aero_cli],
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["test/support"]
  defp elixirc_paths(_),     do: []

  defp deps do
    [
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:getopt, "~> 1.0.1"}
    ]
  end
end
