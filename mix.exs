defmodule Libsecp256k1.Mixfile do
  use Mix.Project

  def project do
    {filelist, 0} = System.cmd("git", ["ls-files"])
    [app: :libsecp256k1,
     version: "0.1.5",
     language: :erlang,
     description: "Erlang NIF bindings for the the libsecp256k1 library",
     package: [
       files: Enum.drop(String.split(filelist, "\n"), -1),
       maintainers: ["Matthew Branton", "Geoffrey Hayes"],
       licenses: ["MIT"],
       links: %{"GitHub" => "https://github.com/exthereum/libsecp256k1"}
     ],
     deps: deps()
     ]
  end

  defp deps() do
    [
      {:mix_erlang_tasks, "0.1.0", only: :dev},
      {:ex_doc, "~> 0.17", only: :dev, runtime: false},
    ]
  end
end
