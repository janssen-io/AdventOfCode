defmodule Year2023.Day24 do
  import Point, only: [+++: 2]
  import Elf, only: [fst: 1, snd: 1, sign: 1]

  @doc ~S"""
      iex> AdventOfCode.example(2023, 24)
      ...> |> Year2023.Day24.p1({7, 27})
      2
  """
  def p1(lines, test_area \\ {200_000_000_000_000, 400_000_000_000_000}) do
    Enum.map(lines, &parse_hailstone/1)
    |> Enum.map(&line2d/1)
    |> Elf.pairs()
    |> Enum.reject(fn {l1, l2} -> parallel2d?(l1, l2) end)
    |> Enum.map(fn {l1, l2} -> intersect2d(l1, l2) end)
    |> IO.inspect()
    |> Enum.count(fn {future?, p_i} -> future? && in_area(test_area, p_i) end)
  end

  def in_area(_, {:inf, :inf}), do: false

  def in_area({lower, upper}, {x, y}) do
    lower <= x && x <= upper &&
      lower <= y && y <= upper
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
    [x, y, z, dx, dy, dz] = Elf.parse_ints(line, :-)

    %{
      pos: {x, y, z},
      dir: {dx, dy, dz}
    }
  end

  @doc """
  Checks whether two lines are parallel.
  A line {a, b} is defined by its slope (a) and y-intersect (b) (y = ax + b).
  """
  def parallel2d?({a1, _, _}, {a2, _, _}), do: a1 == a2

  def line2d(v) do
    p0 = v.pos
    p1 = v.pos +++ v.dir
    # line = ax * b, with a = slope = dy/dx
    slope = (snd(p1) - snd(p0)) / (fst(p1) - fst(p0))
    # y = slope * x
    # p0.y = slope * p0.x + b
    # => b = p0.y - slope * p0.x
    y_intersect = snd(p0) - fst(p0) * slope
    {slope, y_intersect, v}
  end

  def fx(x, line2d), do: line2d.a * x + line2d.b

  def intersect2d({a1, b1, v1}, {a2, b2, v2}) do
    x_i = (b2 - b1) / (a1 - a2)
    y_i = a1 * x_i + b1
    p_i = {x_i, y_i}

    {future?(p_i, v1) && future?(p_i, v2), p_i}
  end

  @doc """
  Check if the calculated point is in the future of the given line.
  It's in the future if it's on the same side of the starting position 'pos'
  as the direction 'dir'.
  """
  def future?({x, _}, %{pos: {x_s, _, _}, dir: {dx, _, _}}) do
    sign(dx) == sign(x - x_s)
  end
end
