defmodule Year2023.Day18 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 18)
  ...> |> Year2023.Day18.p1
  0
  """
  def p1(lines) do
    AdventOfCode.PixelGrid.new(:map)
    {grid, _} = make_grid(lines)

    edge_size = Enum.count(Map.keys(grid))

    AdventOfCode.PixelGrid.print(:map, grid)
    count = flood_fill_count(grid, [{1,1}])
    AdventOfCode.PixelGrid.print(:map, grid)
    count + edge_size
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 18)
  ...> |> Year2023.Day18.p2
  0
  """
  def p2(lines) do
    0
  end

  def make_grid(lines) do
    lines
    |> Enum.reduce(init(%{}), &dig_line/2)
  end

  def {a,b} +++ {c,d}, do: {a+c, b+d}
  def mul({a,b}, n), do: {n * a, n * b}

  def dig_line(line, grid_pos) do
    [[_, dir, length, colour]] = Regex.scan(~r/([URDL]) (\d+) \(#(.+)\)/, line)

    Stream.cycle([{dir, colour}])
    |> Enum.take(String.to_integer(length))
    |> Enum.reduce(grid_pos, &dig/2)
  end

  def dig({dir, colour}, {grid, pos}) do
    # IO.inspect(pos)
    next_pos = pos +++ delta(dir)
    AdventOfCode.PixelGrid.set(:map, next_pos, hex_to_rgb(colour))
    {Map.put(grid, next_pos, "#"), next_pos}
  end

  def hex_to_rgb(hex) do
    [r1, r2, g1, g2, b1, b2] = String.codepoints(hex)
    [
      String.to_integer("#{r1}#{r2}", 16),
      String.to_integer("#{g1}#{g2}", 16),
      String.to_integer("#{b1}#{b2}", 16)
    ]
  end

  def delta("U"), do: {0, -1}
  def delta("R"), do: {1, 0}
  def delta("D"), do: {0, 1}
  def delta("L"), do: {-1, 0}

  def init(grid), do: {grid, {0, 0}}

  def flood_fill_count(grid, q, seen \\ MapSet.new())
  def flood_fill_count(_grid, [], seen), do: MapSet.size(seen)
  def flood_fill_count(grid, [cell | q], seen) do
    if cell in seen || Map.get(grid, cell) == "#" do
      flood_fill_count(grid, q, seen)
    else
      AdventOfCode.PixelGrid.set(:map, cell, [100, 100, 100])
      next =
        [delta("U"), delta("R"), delta("D"), delta("L")]
        |> Enum.map(& (cell +++ &1))

      flood_fill_count(grid, q ++ next, MapSet.put(seen, cell))
    end
  end

end
