defmodule Year2023.Day25 do
  defmodule Graph do
    def new(), do: %{}

    def add_edge(graph, v1, v2) do
      Map.update(graph, v1, [v2], fn edges -> [v2 | edges] end)
      |> Map.update(v2, [v1], fn edges -> [v1 | edges] end)
    end

    def cut_edge(graph, v1, v2) do
      Map.update!(graph, v1, fn neighbours -> Enum.reject(neighbours, &(&1 == v2)) end)
      |> Map.update!(v2, fn neighbours -> Enum.reject(neighbours, &(&1 == v1)) end)
    end

    def neighbours(graph, v1), do: Map.get(graph, v1, [])
    def vertices(graph), do: Map.keys(graph)

    def size_from(graph, v) do
      size_from(graph, [v], MapSet.new(), 0)
    end

    defp size_from(_graph, [], _seen, size), do: size

    defp size_from(graph, [v | q], seen, size) do
      if v in seen do
        size_from(graph, q, seen, size)
      else
        size_from(graph, q ++ neighbours(graph, v), MapSet.put(seen, v), size + 1)
      end
    end

    def compute_traffic(graph) do
      vertices(graph)
      |> Enum.map(&{&1, neighbours(graph, &1)})
      |> Enum.map(fn {v, ns} ->
        compute_traffic(graph, Enum.map(ns, &{v, &1}), MapSet.new([v]), %{})
      end)
      |> Enum.reduce(fn traffic, total ->
        Map.to_list(traffic)
        |> Enum.reduce(total, fn {k, v}, acc ->
          Map.update(acc, k, v, &(&1 + v))
        end)
      end)
    end

    defp compute_traffic(_graph, [], _seen, traffic), do: traffic

    defp compute_traffic(graph, [{from, to} | q], seen, traffic) do
      if to in seen do
        compute_traffic(graph, q, seen, traffic)
      else
        edge = [from, to] |> Enum.sort()
        new_traffic = Map.update(traffic, edge, 1, &(&1 + 1))

        next =
          neighbours(graph, to)
          |> Enum.map(&{to, &1})

        compute_traffic(graph, q ++ next, MapSet.put(seen, to), new_traffic)
      end
    end
  end

  @doc ~S"""
      iex> AdventOfCode.example(2023, 25)
      ...> |> Year2023.Day25.p1
      0
  """
  def p1(lines) do
    graph = parse_graph(lines)
    verts = Graph.vertices(graph)
    size = verts |> Enum.count()

    to_cut =
      graph
      |> Graph.compute_traffic()
      |> Map.to_list()
      |> Enum.sort_by(fn {_k, v} -> v end, :desc)
      |> Enum.take(3)
      |> Enum.map(fn {k, _v} -> k end)

    cut_graph =
      to_cut
      |> Enum.reduce(graph, fn [v1, v2], g ->
        Graph.cut_edge(g, v1, v2)
      end)

    subgraph_size = Graph.size_from(cut_graph, hd(verts))
    {size, subgraph_size, size - subgraph_size, subgraph_size * (size - subgraph_size)}
  end

  @doc ~S"""
      iex> AdventOfCode.example(2023, 25)
      ...> |> Year2023.Day25.p2
      0
  """
  def p2(lines) do
    Enum.count(lines)
  end

  def parse_graph(lines) do
    graph = Graph.new()

    lines
    |> Enum.reduce(graph, fn line, g ->
      [from | tos] = Regex.scan(~r/[a-zA-Z]+/, line) |> Enum.flat_map(& &1)

      Enum.reduce(tos, g, fn to, acc ->
        Graph.add_edge(acc, from, to)
      end)
    end)
  end
end
