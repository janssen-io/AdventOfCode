defmodule Year2023.Day10 do
  @shapes %{
    |: ["N", "S"],
    -: ["E", "W"],
    L: ["N", "E"],
    J: ["N", "W"],
    "7": ["S", "W"],
    F: ["S", "E"]
  }

  @doc ~S"""
  iex> AdventOfCode.example(2023, 10)
  ...> |> Year2023.Day10.p1
  0
  """
  def p1(lines) do
    parse(lines)
    |> find_loop()
    |> MapSet.size()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 10)
  ...> |> Year2023.Day10.p2
  0
  """
  def p2(lines) do
    Application.put_env(:elixir, :ansi_enabled, true)
    map = parse(lines)
    loop = find_loop(map)
    IO.inspect(scaled(map[:start]), label: :exploded_start)
    new_map = explode(loop, map)
    new_loop = MapSet.new(Map.keys(new_map) |> Enum.filter(&is_coord/1))
    IO.inspect(MapSet.size(new_loop) / 3, label: :loop_size)

    AdventOfCode.PixelGrid.new(:heatmap)

    outside =
      floodfill(
        new_loop,
        {-1, new_map[:max_x] + 1, -1, new_map[:max_y] + 1},
        {-1, -1},
        MapSet.new()
      )

    new_loop |> Enum.each(fn coords -> AdventOfCode.PixelGrid.set(:heatmap, coords, [0, 0, 0]) end)
    # AdventOfCode.PixelGrid.print(:heatmap)

    # draw(new_loop, new_map, outside)

    exploded_centers =
      Map.keys(map)
      |> Enum.filter(&is_coord/1)
      |> Enum.map(&scaled/1)
      |> MapSet.new()

    originals = MapSet.intersection(exploded_centers, outside)

    AdventOfCode.PixelGrid.new(:small)
    originals |> Enum.each(fn coords -> AdventOfCode.PixelGrid.set(:small, original(coords), [0, 255, 255]) end)
    loop |> Enum.each(fn coords -> AdventOfCode.PixelGrid.set(:small, coords, [0, 0, 0]) end)
    AdventOfCode.PixelGrid.print(:small)

    19600 - MapSet.size(loop) - MapSet.size(originals)
  end

  def is_coord({_, _}), do: true
  def is_coord(_), do: false

  def explode(loop, map) do
    new_map = %{
      max_x: map[:max_x] * 3 + 2,
      max_y: map[:max_y] * 3 + 2
    }

    map
    |> Map.filter(fn {k, _} -> k in loop end)
    |> Map.to_list()
    |> Enum.reduce(new_map, &explode_shape/2)
  end

  def explode_shape({coords, shape = "|"}, m) do
    new_center = {xn, yn} = scaled(coords)

    Map.put(m, new_center, shape)
    |> Map.put({xn, yn - 1}, shape)
    |> Map.put({xn, yn + 1}, shape)
  end

  def explode_shape({coords, shape = "-"}, m) do
    new_center = {xn, yn} = scaled(coords)

    Map.put(m, new_center, shape)
    |> Map.put({xn - 1, yn}, shape)
    |> Map.put({xn + 1, yn}, shape)
  end

  def explode_shape({coords, shape = "7"}, m) do
    new_center = {xn, yn} = scaled(coords)

    Map.put(m, new_center, shape)
    |> Map.put({xn - 1, yn}, "-")
    |> Map.put({xn, yn + 1}, "|")
  end

  def explode_shape({coords, shape = "F"}, m) do
    new_center = {xn, yn} = scaled(coords)

    Map.put(m, new_center, shape)
    |> Map.put({xn + 1, yn}, "-")
    |> Map.put({xn, yn + 1}, "|")
  end

  def explode_shape({coords, shape = "J"}, m) do
    new_center = {xn, yn} = scaled(coords)

    Map.put(m, new_center, shape)
    |> Map.put({xn - 1, yn}, "-")
    |> Map.put({xn, yn - 1}, "|")
  end

  def explode_shape({coords, shape = "L"}, m) do
    new_center = {xn, yn} = scaled(coords)

    Map.put(m, new_center, shape)
    |> Map.put({xn + 1, yn}, "-")
    |> Map.put({xn, yn - 1}, "|")
  end

  def explode_shape({coords, shape = "S"}, m) do
    explode_shape({coords, "7"}, m)
  end

  def scaled({x, y}), do: {x * 3 + 1, y * 3 + 1}
  def original({x, y}), do: {div(x - 1, 3), div(y - 1, 3)}

  def floodfill(
        loop,
        bounds = {min_x, max_x, min_y, max_y},
        coords = {x, y},
        breadcrumbs,
        depth \\ 0
      ) do
    if x < min_x or x > max_x or y < min_y or y > max_y or coords in breadcrumbs or coords in loop do
      breadcrumbs
    else
      pixel(coords, depth)

      MapSet.put(breadcrumbs, coords)
      |> f(loop, bounds, north(coords), depth + 1)
      |> f(loop, bounds, south(coords), depth + 1)
      |> f(loop, bounds, east(coords), depth + 1)
      |> f(loop, bounds, west(coords), depth + 1)
    end
  end

  def pixel({x, y}, depth) do
    AdventOfCode.PixelGrid.set(:heatmap, {x, y}, %{heatmap: depth, max: 10_000})
  end

  def f(breadcrumbs, loops, bounds, coords, depth) do
    floodfill(loops, bounds, coords, breadcrumbs, depth)
  end

  def parse(lines) do
    Enum.with_index(lines)
    |> Enum.reduce(Map.new(), &parse_row/2)
  end

  def parse_row({row, y}, map) do
    map = Map.update(map, :max_y, 0, fn max_y -> if max_y < y, do: y, else: max_y end)

    String.codepoints(row)
    |> Enum.with_index()
    |> Enum.reduce(map, fn {col, x}, new_map ->
      Map.put(new_map, {x, y}, col)
      |> Map.update(:max_x, 0, fn max_x -> if max_x < x, do: x, else: max_x end)
      |> Map.update(:start, {0, 0}, fn start -> if col == "S", do: {x, y}, else: start end)
    end)
  end

  def draw(loop, map, outside \\ MapSet.new()) do
    for y <- 0..map[:max_y] do
      c =
        for x <- 0..map[:max_x] do
          cond do
            {x, y} in loop and {x, y} in outside ->
              IO.write("A")
              0

            {x, y} in loop ->
              IO.write(Map.get(map, {x, y}))
              0

            {x, y} in outside ->
              IO.write(" ")
              0

            true ->
              IO.write(".")
              1
          end
        end

      IO.write("\n")
      c
    end
    |> List.flatten()
    |> Enum.sum()
  end

  def find_loop(map) do
    {x, y} = Map.get(map, :start)
    walk_loop(map, {x, y + 1}, "N", MapSet.new([{x, y}, {x, y + 1}]))
  end

  def north({x, y}), do: {x, y - 1}
  def south({x, y}), do: {x, y + 1}
  def east({x, y}), do: {x + 1, y}
  def west({x, y}), do: {x - 1, y}

  def walk_loop(map, start, from_dir, breadcrumbs) do
    case walk(map, start, from_dir) do
      nil ->
        raise "you didn'scaled walk right..."

      {next_coord, next_from_dir} ->
        if next_coord in breadcrumbs do
          breadcrumbs
        else
          walk_loop(map, next_coord, next_from_dir, MapSet.put(breadcrumbs, next_coord))
        end
    end
  end

  def walk(map, coord = {x, y}, from_dir) do
    shape = Map.get(map, coord)

    case shape do
      nil ->
        nil

      _ ->
        next_dir = next(shape, from_dir)
        # IO.inspect({shape, from_dir, next_dir})

        case next_dir do
          nil -> nil
          "N" -> {north(coord), "S"}
          "E" -> {east(coord), "W"}
          "S" -> {south(coord), "N"}
          "W" -> {west(coord), "E"}
        end
    end
  end

  @doc ~S"""
  iex> Year2023.Day10.next("|", "S")
  "N"
  iex> Year2023.Day10.next("|", "N")
  "S"
  iex> Year2023.Day10.next("|", "E")
  nil
  """
  Enum.each(Map.keys(@shapes), fn shape ->
    dirs = Map.get(@shapes, shape)

    def next(unquote(to_string(shape)), dir) do
      case Enum.find_index(unquote(dirs), &(&1 == dir)) do
        nil -> nil
        n -> Enum.at(unquote(dirs), rem(n + 1, 2))
      end
    end
  end)
end
