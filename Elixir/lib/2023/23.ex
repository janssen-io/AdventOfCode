defmodule Year2023.Day23 do
  import Point, only: [+++: 2, ---: 2]

  @doc ~S"""
      iex> AdventOfCode.example(2023, 23)
      ...> |> Year2023.Day23.p1
      0
  """
  def p1(lines) do
    parse_grid(lines)
    |> walk()
  end

  @doc ~S"""
      iex> AdventOfCode.example(2023, 23)
      ...> |> Year2023.Day23.p2
      0
  """
  def p2(lines) do
    grid = parse_grid(lines)

    start = {1, 0}
    destination = {max_x(grid) - 1, max_y(grid)}

    grid
    |> weighted_graph()
    |> IO.inspect(label: :graph)
    |> walk_graph([{start, []}], destination)
    |> Enum.max()
  end

  def walk(grid, q \\ [{{1, 0}, MapSet.new()}], results \\ [])

  def walk(_grid, [], results), do: results

  def walk(grid, [{pos = {x, y}, seen} | q], results) do
    if pos in seen do
      walk(grid, q, results)
    else
      new_seen = MapSet.put(seen, pos)

      if y == max_y(grid) do
        IO.inspect(MapSet.size(seen))
        walk(grid, q, [MapSet.size(seen) | results])
      else
        case Map.get(grid, pos, :wall) do
          :wall ->
            raise "How did I end up on a wall? #{x}, #{y}"

          _ ->
            next =
              valid_steps(grid, pos, seen)
              |> Enum.zip(Stream.cycle([new_seen]))

            walk(grid, next ++ q, results)
        end
      end
    end
  end

  def walk_graph(graph, q, to, results \\ [])
  def walk_graph(_graph, [], _to, results), do: results

  def walk_graph(graph, [{pos, seen} | q], to, results) do
    cond do
      pos in seen ->
        walk_graph(graph, q, to, results)

      pos == to ->
        length = path_length(graph, [to | seen])
        new_results = [length | results]
        walk_graph(graph, q, to, new_results)

      true ->
        new_seen = [pos | seen]
        neighbours = Map.get(graph, pos) |> Map.keys()
        next = Enum.zip(neighbours, Stream.cycle([new_seen]))

        walk_graph(graph, next ++ q, to, results)
    end
  end

  def path_length(graph, intersections, length \\ 0)
  def path_length(_graph, [_], length), do: length

  def path_length(graph, [a, b | xs], length) do
    section_length = Map.get(graph, a) |> Map.get(b)

    if section_length == nil do
      IO.inspect({graph, a, b})
      raise "No edge"
    end

    path_length(graph, [b | xs], length + section_length)
  end

  def intersection?(grid, pos) do
    case Map.get(grid, pos, :wall) do
      :wall -> false
      _ -> valid_steps(grid, pos) |> Enum.count() > 2
    end
  end

  def weighted_graph(grid) do
    intersections =
      Map.keys(grid)
      |> Enum.filter(&intersection?(grid, &1))
      |> Enum.concat([{1, 0}, {max_x(grid) - 1, max_y(grid)}])

    pairs = for from <- intersections, to <- intersections, do: {from, to}

    Enum.reject(pairs, fn {from, to} -> from == to end)
    |> Enum.reduce(%{}, fn {from, to}, graph ->
      distance = distance(grid, from, to, [{from, MapSet.new()}])

      case distance do
        [] ->
          graph

        [d] ->
          Map.update(graph, from, %{to => d}, fn neighbours -> Map.put(neighbours, to, d) end)
      end
    end)
  end

  def distance(grid, from, to, q \\ [], distances \\ [])
  def distance(_grid, _from, _to, [], distances), do: distances

  def distance(grid, from, to, [{pos, seen} | q], distances) do
    cond do
      pos in seen ->
        distance(grid, from, to, q, distances)

      pos == to ->
        [MapSet.size(seen) | distances]

      pos != from && intersection?(grid, pos) ->
        distance(grid, from, to, q, distances)

      true ->
        next =
          valid_steps(grid, pos, seen)
          |> Enum.zip(Stream.cycle([MapSet.put(seen, pos)]))

        distance(grid, from, to, next ++ q, distances)
    end
  end

  def max_y(grid) do
    Map.keys(grid)
    |> Enum.max_by(&Elf.snd/1)
    |> then(&Elf.snd/1)
  end

  def max_x(grid) do
    Map.keys(grid)
    |> Enum.max_by(&Elf.fst/1)
    |> then(&Elf.fst/1)
  end

  def valid_steps(grid, pos, seen \\ MapSet.new()) do
    case Map.get(grid, pos, :wall) do
      :east ->
        [pos +++ {1, 0}, pos --- {1, 0}]

      # [pos +++ {1, 0}]

      :south ->
        [pos +++ {0, 1}, pos --- {0, 1}]

      # [pos +++ {0, 1}]

      :wall ->
        raise "Cannot pass through walls (#{pos})"

      :path ->
        [pos +++ {1, 0}, pos +++ {0, 1}, pos --- {1, 0}, pos --- {0, 1}]
        |> Enum.reject(fn loc -> Map.get(grid, loc, :wall) == :wall end)
    end
    |> Enum.reject(&(&1 in seen))
  end

  def parse_grid(lines) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, g ->
      line
      |> String.codepoints()
      |> Enum.with_index()
      |> Enum.reduce(g, fn {c, x}, gg ->
        case c do
          "#" -> Map.put(gg, {x, y}, :wall)
          ">" -> Map.put(gg, {x, y}, :east)
          "v" -> Map.put(gg, {x, y}, :south)
          "." -> Map.put(gg, {x, y}, :path)
        end
      end)
    end)
  end
end
