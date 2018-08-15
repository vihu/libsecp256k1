defmodule Libsecp256k1.Mixfile do
  use Mix.Project

  def project do
    [app: :libsecp256k1,
     version: "0.1.4",
     language: :erlang,
     description: "Erlang NIF bindings for the the libsecp256k1 library",
     package: [
       maintainers: ["Matthew Branton", "Geoffrey Hayes"],
       licenses: ["MIT"],
       links: %{"GitHub" => "https://github.com/exthereum/libsecp256k1"}
     ],
     deps: deps()
     ]
  end

  defp deps() do
    [
      {:mix_erlang_tasks, "0.1.0"},
    ]
  end
end
