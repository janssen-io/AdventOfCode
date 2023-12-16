defmodule Year2023.Day16 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 16)
  ...> |> Year2023.Day16.p1
  46
  """
  def p1(lines) do
    grid = parse_grid(lines)
    run(grid, [{{0, 0}, :e}])
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 16)
  ...> |> Year2023.Day16.p2
  51
  """
  def p2(lines) do
    grid = parse_grid(lines)
    run(grid, get_entries(grid))
  end

  def run(grid, entries) do
    entries
    |> Enum.reduce([], fn entry, results ->
        r = send_beam(grid, [entry])
        |> count_energised()
        [r | results]
      end)
      |> Enum.max()
  end

  def parse_grid(lines) do
    lines
    |> Stream.with_index()
    |> Enum.reduce(%{}, fn {line, y}, final_map ->
      String.codepoints(line)
      |> Enum.with_index()
      |> Enum.reduce(final_map, fn {c, x}, map ->
        cond do
          c == "|" -> Map.put(map, {x, y}, MapSet.new([:split_v]))
          c == "-" -> Map.put(map, {x, y}, MapSet.new([:split_h]))
          c == "/" -> Map.put(map, {x, y}, MapSet.new([:mirror_se]))
          c == "\\" -> Map.put(map, {x, y}, MapSet.new([:mirror_ne]))
          true -> map
        end
        |> Map.update(:max_x, 0, fn mx -> if x > mx, do: x, else: mx end)
      end)
      |> Map.update(:max_y, 0, fn my -> if y > my, do: y, else: my end)
    end)
  end

  def send_beam(map, next, seen \\ MapSet.new())

  def send_beam(map, [], _) do
    map
  end

  def send_beam(map, [{coord = {x, y}, direction} | next], seen) do
    if Map.get(map, :max_x) < x || x < 0 ||
         Map.get(map, :max_y) < y || y < 0 ||
         {coord, direction} in seen do
      send_beam(map, next, seen)
    else
      location = Map.get(map, coord, MapSet.new())

      todo =
        cond do
          MapSet.size(location) == 0 ->
            [{move(coord, direction), direction} | next]

          true ->
            cond do
              :split_v in location && is_v(direction) ->
                [{move(coord, direction), direction} | next]

              :split_v in location ->
                [
                  {move(coord, :n), :n},
                  {move(coord, :s), :s}
                  | next
                ]

              :split_h in location && !is_v(direction) ->
                [{move(coord, direction), direction} | next]

              :split_h in location ->
                [
                  {move(coord, :e), :e},
                  {move(coord, :w), :w}
                  | next
                ]

              :mirror_ne in location ->
                bounced = bounce(:mirror_ne, direction)
                [{move(coord, bounced), bounced} | next]

              :mirror_se in location ->
                bounced = bounce(:mirror_se, direction)
                [{move(coord, bounced), bounced} | next]

              true ->
                [{move(coord, direction), direction} | next]
            end
        end

      send_beam(
        Map.put(map, coord, MapSet.put(location, direction)),
        todo,
        MapSet.put(seen, {coord, direction})
      )
    end
  end

  def merge(m1, m2) do
    Map.keys(m2)
    |> Enum.filter(&is_coord/1)
    |> Enum.reduce(m1, fn coord, map ->
      right = Map.get(m2, coord)
      left = Map.get(m1, coord, MapSet.new())
      merged = MapSet.union(left, right)
      Map.put(map, coord, merged)
    end)
  end

  def is_coord({_, _}), do: true
  def is_coord(_), do: false

  def is_v(:n), do: true
  def is_v(:s), do: true
  def is_v(_), do: false

  def north({x, y}), do: {x, y - 1}
  def south({x, y}), do: {x, y + 1}
  def west({x, y}), do: {x - 1, y}
  def east({x, y}), do: {x + 1, y}

  def move(coord, :n), do: north(coord)
  def move(coord, :s), do: south(coord)
  def move(coord, :w), do: west(coord)
  def move(coord, :e), do: east(coord)

  # mirror, direction of travel
  def bounce(:mirror_se, :n), do: :e
  def bounce(:mirror_se, :e), do: :n
  def bounce(:mirror_se, :s), do: :w
  def bounce(:mirror_se, :w), do: :s
  def bounce(:mirror_ne, :n), do: :w
  def bounce(:mirror_ne, :w), do: :n
  def bounce(:mirror_ne, :s), do: :e
  def bounce(:mirror_ne, :e), do: :s

  def count_energised(map) do
    dirs = MapSet.new([:n, :e, :s, :w])
    has_dir = fn vals -> MapSet.intersection(vals, dirs) |> MapSet.size() > 0 end

    Map.values(map)
    |> Enum.count(fn v -> is_map(v) && has_dir.(v) end)
  end

  def print(grid) do
    IO.puts("Grid:")
    dirs = MapSet.new([:n, :e, :s, :w])

    for y <- 0..Map.get(grid, :max_y) do
      for x <- 0..Map.get(grid, :max_x) do
        vals = MapSet.new(Map.get(grid, {x, y}, []))
        has_dir = MapSet.intersection(vals, dirs) |> MapSet.size() > 0

        if has_dir do
          IO.write("#")
        else
          IO.write(".")
        end
      end

      IO.write("\n")
    end

    grid
  end

  def get_entries(grid) do
    max_x = Map.get(grid, :max_x)
    max_y = Map.get(grid, :max_y)
    top = for x <- 0..max_x, do: {{x, 0}, :s}
    bottom = for x <- 0..max_x, do: {{x, max_y}, :n}
    left = for y <- 0..max_y, do: {{0, y}, :e}
    right = for y <- 0..max_y, do: {{max_x, y}, :w}
    Enum.concat([top, bottom, left, right])
  end
end
