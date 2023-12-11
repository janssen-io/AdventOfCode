defmodule Year2023.Day11 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 11)
  ...> |> Year2023.Day11.p1
  374
  """
  def p1(lines) do
    lines
    |> create_grid()
    |> expand(&Elf.fst/1, fn {x, y}, d -> {x + d, y} end)
    |> expand(&Elf.snd/1, fn {x, y}, d -> {x, y + d} end)
    |> pairwise_distance()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 11)
  ...> |> Year2023.Day11.p2(100)
  8410
  """
  def p2(lines, age \\ 1_000_000) do
    lines
    |> create_grid()
    |> expand(&Elf.fst/1, fn {x, y}, d -> {x + d, y} end, age)
    |> expand(&Elf.snd/1, fn {x, y}, d -> {x, y + d} end, age)
    |> pairwise_distance()
  end

  def create_grid(lines) do
    lines
    |> Stream.with_index()
    |> Enum.reduce(%{}, fn {line, y}, map ->
      String.codepoints(line)
      |> Stream.with_index()
      |> Enum.reduce(map, fn {c, x}, map_prime ->
        if c == "#" do
          Map.put(map_prime, {x, y}, c)
        else
          map_prime
        end
      end)
    end)
  end

  def expand(grid, selector, updater, age \\ 2) do
    distances = [0 | distances_to_move(grid, selector, age)]

    {expanded_grid, _} =
      Map.keys(grid)
      |> Enum.group_by(selector)
      |> Map.to_list()
      |> Enum.sort_by(&Elf.fst/1)
      |> Enum.map(&Elf.snd/1)
      |> Enum.with_index()
      |> Enum.reduce({%{}, 0}, fn {group, i}, {new_grid, sum_distance} ->
        d = sum_distance + Enum.at(distances, i)

        updated_grid =
          group
          |> Enum.reduce(new_grid, fn {x, y}, new_grid_prime ->
            Map.put(new_grid_prime, updater.({x, y}, d), "#")
          end)

        {updated_grid, d}
      end)

    expanded_grid
  end

  def distances_to_move(grid, selector, age \\ 2) do
    cs =
      Map.keys(grid)
      |> Stream.map(selector)
      |> Stream.uniq()
      |> Enum.sort(:asc)

    cs
    |> Enum.zip(tl(cs))
    |> Enum.map(fn {c1, c2} -> c2 - c1 - 1 end)
    |> Enum.map(fn d -> d * (age - 1) end)
  end

  def pairwise_distance(grid) do
    Map.keys(grid)
    |> pairs()
    |> Enum.map(fn {l, r} -> distance(l, r) end)
    |> Enum.sum()
  end

  def distance({x, y}, {a, b}), do: abs(b - y) + abs(a - x)

  @doc """
  iex> Year2023.Day11.pairs([1,2,3,4,5])
  [{1,2}, {1,3}, {1,4}, {1,5}, {2,3}, {2,4}, {2,5}, {3,4}, {3,5}, {4,5}]
  """
  def pairs([]), do: []
  def pairs([x|ys]) do
    Enum.map(ys, fn y -> {x, y} end)
    |> Enum.concat(pairs(ys))
  end
end
