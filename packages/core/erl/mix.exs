defmodule Aero.Core.MixProject do
  use Mix.Project

  def project do
    [
      app: :aero_core,
      version: "0.1.0",
      language: :erlang,
      compilers: [:rustler] ++ Mix.compilers,
      rustler_crates: rustler_crates(),
      deps: deps()
    ]
  end

  defp deps do
    [
      {:rustler, "~> 0.18.0"}
    ]
  end

  defp rustler_crates do
    [
      aero_core_f64: [
        path: "native/aero_core_f64",
        mode: (if Mix.env == :prod, do: :release, else: :debug),
      ]
    ]
  end
end
