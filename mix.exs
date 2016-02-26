defmodule Jwerl.Mixfile do
  use Mix.Project

  def project do
    [app: :jwerl,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [ 
      {:jsx, "~> 2.6.2"},
    ]
  end
end
