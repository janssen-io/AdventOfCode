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
    Enum.count(lines)
  end

  def walk(grid, q \\ [{{1, 0}, MapSet.new()}], results \\ [])

  def walk(_grid, [], results), do: results

  def walk(grid, [{pos = {x, y}, seen} | q], results) do
    if pos in seen do
      walk(grid, q, results)
    end

    new_seen = MapSet.put(seen, pos)

    if y == max_y(grid) do
      IO.inspect(MapSet.size(new_seen) - 1)
      walk(grid, q, [new_seen | results])
    end

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

  def max_y(grid) do
    Map.keys(grid)
    |> Enum.max_by(&Elf.snd/1)
    |> then(&Elf.snd/1)
  end

  def valid_steps(grid, pos, seen) do
    case Map.get(grid, pos, :wall) do
      :east ->
        # [pos +++ {1, 0}, pos --- {1, 0}]
        [pos +++ {1, 0}]

      :south ->
        # [pos +++ {0, 1}, pos --- {0, 1}]
        [pos +++ {0, 1}]

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
