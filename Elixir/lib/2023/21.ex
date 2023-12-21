defmodule Year2023.Day21 do
  import Point, only: [+++: 2]

  @doc ~S"""
      iex> AdventOfCode.example(2023, 21)
      ...> |> Year2023.Day21.p1(6)
      16
  """
  def p1(lines, times \\ 64) do
    parse_grid(lines)
    |> simulate(times)
    |> MapSet.size()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 21)
  ...> |> Year2023.Day21.p2
  0
  """
  def p2(lines) do
    0
  end

  def simulate(grid, n) do
    {start, _} = Map.to_list(grid) |> Enum.find(fn {_, c} -> c == "S" end)
    1..n
    |> Enum.reduce(MapSet.new([start]), fn _, locs ->
      l = tick(grid, locs)
      # print(grid, l)
      # IO.read(:line)
      l
    end)
  end

  def tick(grid, locations) do
    Enum.reduce(locations, MapSet.new(), fn coord, new_locs ->
      [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]
      |> Enum.reduce(new_locs, fn delta, locs ->
        if Map.get(grid, coord +++ delta, "#") != "#" do
          MapSet.put(locs, coord +++ delta)
        else
          locs
        end
      end)
    end)
  end

  def get_inf(grid, {x, y}) do
    max_x = Map.get(grid, :max_x)
    max_y = Map.get(grid, :max_y)

    {mx, my} = {mod(x, max_x + 1), mod(y, max_y + 1)}

    result = Map.get(grid, {mx, my})
    if result == nil, do: raise "{#{x}, #{y}} -> {#{mx}, #{my}} not in grid (#{max_x}, #{max_y})"

    result
  end

  def parse_grid(lines) do
    Enum.with_index(lines)
    |> Enum.reduce(%{}, fn {line, y}, g ->
      String.codepoints(line)
      |> Enum.with_index()
      |> Enum.reduce(g, fn {c, x}, gg ->
        Map.put(gg, :max_x, max(Map.get(gg, :max_x, 0), x))
        |> Map.put({x, y}, c)
      end)
      |> Map.put(:max_y, max(Map.get(g, :max_y, y), y))
    end)
  end

  def print(grid, locations) do
    {max_x, _} = Map.keys(grid) |> Enum.max_by(&Elf.fst/1)
    {_, max_y} = Map.keys(grid) |> Enum.max_by(&Elf.snd/1)
    IO.inspect(locations)
    for y <- 0..max_y do
      for x <- 0..max_x do
        c = Map.get(grid, {x, y})
        cond do
          c == '#' && c in locations -> raise "Garden plot on rocks"
          {x, y} in locations -> IO.write("O")
          true -> IO.write(c)
        end
      end
      IO.write("\n")
    end
  end

  @doc """
      iex> Year2023.Day21.mod(8, 5)
      3

      iex> Year2023.Day21.mod(-8, 5)
      2

      iex> Year2023.Day21.mod(-1, 5)
      4

      iex> Year2023.Day21.mod(-5, 5)
      0
  """
  def mod(n, m) when n >= 0, do: rem(n, m)
  def mod(n, m) when rem(-n, m) == 0, do: 0
  def mod(n, m), do: m - rem(-n, m)
end
