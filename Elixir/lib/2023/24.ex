defmodule Year2023.Day24 do
  import Point, only: [+++: 2, ---: 2]
  import Elf, only: [fst: 1, snd: 1]

  @doc ~S"""
      iex> AdventOfCode.example(2023, 24)
      ...> |> Year2023.Day24.p1({7, 27})
      0
  """
  def p1(lines, test_area \\ {200_00_000_000_0000, 400_000_000_000_000}) do
    Enum.count(lines)
  end

  @doc ~S"""
      iex> AdventOfCode.example(2023, 24)
      ...> |> Year2023.Day24.p2
      0
  """
  def p2(lines) do
    Enum.count(lines)
  end

  def parse_hailstone(line) do
    [x, y, z, dx, dy, dz] =
      Elf.get_ints(line, :-)
      |> Enum.map(&String.to_integer/1)

    %{
      pos: {x, y, z},
      dir: {dx, dy, dz}
    }
  end

  def direction2d(%{x: x, y: y}), do: y / x
  def parallel2d?(dir1, dir2), do: abs(direction2d(dir1)) == abs(direction2d(dir2))

  def line2d(v) do
    p0 = v.pos
    p1 = v.pos +++ v.dir
    slope = (snd(p1) - snd(p0)) / (fst(p1) - fst(p0))
    # y - p0.y = slope * (x - p0.x)
    # y = slope * x - slope * p0.x + p0.y
    # => y_intersect = slope * p0.x + p0.y
    y_intersect = fst(p0) * slope + snd(p0)
    {slope, y_intersect}
  end

  def fx(x, line2d), do: line2d.a * x + line2d.b

  def intersect2d(v1, v2) do
    if parallel2d?(v1.dir, v2.dir) do
      {false, :parallel}
    else
      {a1, b1} = line2d(v1)
      {a2, b2} = line2d(v2)
      x_i = (b2 - b1) / (a1 - a2)
      y_i = a1 * x_i + b1
      {x_i, y_i}
    end
  end
end
