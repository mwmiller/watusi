defmodule Watusi.MixProject do
  use Mix.Project

  def project do
    [
      app: :watusi,
      version: "0.1.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      aliases: aliases(),
      description: "A native Elixir WebAssembly Text (WAT) to Binary (WASM) converter.",
      source_url: "https://github.com/mwmiller/watusi",
      homepage_url: "https://github.com/mwmiller/watusi",
      package: [
        maintainers: ["Matt Miller"],
        licenses: ["MIT"],
        links: %{"GitHub" => "https://github.com/mwmiller/watusi"},
        files: ~w(lib .formatter.exs mix.exs README.md LICENSE)
      ],
      docs: [
        main: "Watusi",
        extras: ["README.md"]
      ]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  def cli do
    [
      preferred_envs: [precommit: :test]
    ]
  end

  defp aliases do
    [
      precommit: ["format --check-formatted", "credo --strict", "test"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:leb128, "~> 1.0"},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end
end
