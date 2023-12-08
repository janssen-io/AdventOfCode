defmodule Year2022.Day04 do
  @doc ~S"""
  iex> AdventOfCode.example(2022, 04)
  ...> |> Year2022.Day04.p1
  2
  """
  def p1(lines) do
    solver(lines, &isWhollyContained/2)
  end

  @doc ~S"""
  iex> AdventOfCode.example(2022, 04)
  ...> |> Year2022.Day04.p2
  4
  """
  def p2(lines) do
    solver(lines, &isPartiallyContained/2)
  end

  defp solver(instructions, check) do
    instructions
    |> Enum.map(fn pair ->
      [left, right] = String.split(pair, ",")
      left = String.split(left, "-") |> Enum.map(&String.to_integer/1)
      right = String.split(right, "-") |> Enum.map(&String.to_integer/1)
      check.(left, right)
    end)
    |> Enum.count(fn x -> x end)
  end

  def isWhollyContained(left, right) do
    [l0, l1] = left
    [r0, r1] = right
    (l0 >= r0 && l1 <= r1) || (r0 >= l0 && r1 <= l1)
  end

  def isPartiallyContained(left, right) do
    [l0, l1] = left
    [r0, r1] = right

    (l0 >= r0 && l0 <= r1) ||
      (l1 >= r0 && l1 <= r1) ||
      (r0 >= l0 && r0 <= l1) ||
      (r1 >= l0 && r1 <= l1)
  end
end
