defmodule Year2023.Day17 do
  defmodule State do
    # starting block does not incur heating cost
    defstruct cost: 0, last_moves: [{0, 0}, {-1, 0}], location: {0, 0}
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 17)
  ...> |> Year2023.Day17.p1
  0
  """
  def p1(lines) do
    grid = parse_grid(lines)
    IO.inspect(target(grid))
    start = Heap.new(&by_cost/2)
    r = find_path(grid, target(grid), 0, 3, Heap.push(start, %State{}))
    AdventOfCode.PixelGrid.new(:crumbs)
    r.last_moves
    |> Enum.each(fn {x, y} ->
      AdventOfCode.PixelGrid.set(:crumbs, {x, y}, %{ heatmap: Map.get(grid, {x, y}, 9), max: 10})
    end)
  AdventOfCode.PixelGrid.print(:crumbs, grid)

  counted = r.last_moves
  |> Enum.reduce(0, fn {x, y}, sum -> Map.get(grid, {x, y}, 0) + sum end)
  {r.cost, counted - Map.get(grid, {0, 0})}
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 17)
  ...> |> Year2023.Day17.p2
  0
  """
  def p2(lines) do
    grid = parse_grid(lines)
    IO.inspect(target(grid))
    start = Heap.new(&by_cost/2)
    r = find_path(grid, target(grid), 4, 10, Heap.push(start, %State{}))
    AdventOfCode.PixelGrid.new(:crumbs)
    r.last_moves
    |> Enum.each(fn {x, y} ->
      AdventOfCode.PixelGrid.set(:crumbs, {x, y}, %{ heatmap: Map.get(grid, {x, y}, 9), max: 10})
    end)
  AdventOfCode.PixelGrid.print(:crumbs, grid)

  counted = r.last_moves
  |> Enum.reduce(0, fn {x, y}, sum -> Map.get(grid, {x, y}, 0) + sum end)
  {r.cost, counted - Map.get(grid, {0, 0})}
  end

  def parse_grid(lines) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, grid ->
      String.codepoints(line)
      |> Enum.with_index()
      |> Enum.reduce(grid, fn {c, x}, subgrid ->
        Map.put(subgrid, {x, y}, String.to_integer(c))
      end)
    end)
  end

  def find_path(grid, destination, min_straight, max_straight, heap, seen \\ MapSet.new())

  def find_path(grid, destination, min_straight, max_straight, heap, seen) do
    current = Heap.root(heap)
    {length, _dir_vector} = straight_path_length(current.last_moves)
    cond do
      current == nil -> []
      current.location == destination &&  length >= min_straight -> current
      true ->
        key = {current.location, straight_path_length(Enum.take(current.last_moves, max_straight + 1))}
        if Map.get(seen, key) <= current.cost do
          find_path(grid, destination, min_straight, max_straight, Heap.pop(heap), seen)
        else
          next_seen = Map.put(seen, key, current.cost)
          next_moves = next(grid, min_straight, max_straight, current)
          # next_q = Enum.sort_by(q ++ next_moves, &cost_sort/1)
          next_q = Enum.reduce(next_moves, Heap.pop(heap), fn move, h -> Heap.push(h, move) end)
          find_path(grid, destination, min_straight, max_straight, next_q, next_seen)
        end
      end
  end

  def next(grid, min_straight, max_straight, %State{location: loc, cost: total, last_moves: last}) do
    {length, direction = {ux, uy}} = straight_path_length(last)

    next_delta =
      cond do
        length < min_straight && uy == 0 -> [{1, 0}, {-1, 0}]
        length < min_straight && ux == 0 -> [{0, 1}, {0, -1}]
        length == max_straight && uy == 0 -> [{0, 1}, {0, -1}]
        length == max_straight && ux == 0 -> [{1, 0}, {-1, 0}]
        true -> [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]
      end

    next_delta
    |> Enum.reject(&(&1 +++ direction) == {0, 0}) # only allow 90 degree turns
    |> Enum.map(&(&1 +++ loc))
    |> Enum.map(&{&1, Map.get(grid, &1)})
    |> Enum.reject(fn {_, cost} -> cost == nil end)
    |> Enum.map(fn {loc, cost} ->
      %State{
        location: loc,
        cost: total + cost,
        last_moves: [loc | last]
      }
    end)
  end

  def cost_sort(%State{cost: total}), do: total
  def by_cost(%State{cost: a}, %State{cost: b}), do: a < b

  def {a, b} --- {c, d}, do: {a - c, b - d}
  def {a, b} +++ {c, d}, do: {a + c, b + d}

  def absolute({a, b}), do: {abs(a), abs(b)}

  def target(grid) do
    coords = Map.keys(grid)
    {max_x, _} = Enum.max_by(coords, fn {x, _} -> x end)
    {_, max_y} = Enum.max_by(coords, fn {_, y} -> y end)
    {max_x, max_y}
  end

  def sign(0), do: 0
  def sign(x) when x < 0, do: -1
  def sign(x) when x > 0, do: 1

  def unit({x, y}), do: {sign(x), sign(y)}

  def straight_path_length([]), do: 0
  def straight_path_length([_]), do: 0
  def straight_path_length([a = {x, y}, b={v, w} | xs]) do
    {ux, uy} = unit(a --- b)
    cond do
      (x == v) -> {straight_path_length([a, b | xs], :x), {ux, uy}}
      (y == w) -> {straight_path_length([a, b | xs], :y), {ux, uy}}
      true -> {0, nil}
    end
  end

  def straight_path_length([], _), do: 0
  def straight_path_length([_], _), do: 0
  def straight_path_length([{x,_}, b = {v, _} | xs], :x) do
    cond do
      x == v -> 1 + straight_path_length([b | xs], :x)
      true -> 0
    end
  end

  def straight_path_length([{_,y}, b = {_, w} | xs], :y) do
    cond do
      y == w -> 1 + straight_path_length([b | xs], :y)
      true -> 0
    end
  end
end
