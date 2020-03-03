defmodule PocketOS.MixProject do
  use Mix.Project

  def project do
    [
      app: :pocketos,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make] ++ Mix.compilers,
      deps: deps(),
      atomvm: [
        start: Main,
        flash_offset: 0x210000
      ]
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
      {:exatomvm, github: "bettio/exatomvm", runtime: false},
      {:elixir_make, "~> 0.4", runtime: false}
    ]
  end
end
