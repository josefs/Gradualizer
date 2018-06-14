defmodule Gradualizer.Mixfile do
  use Mix.Project

  def project do
    [app: :gradualizer,
     version: "0.0.1",
     language: :erlang,
     deps: deps()]
  end

  def application do
    [applications: [], mod: {:gradualizer, []}]
  end

  defp deps do
    [
      {:mix_erlang_tasks, "0.1.0"},
    ]
  end
end
