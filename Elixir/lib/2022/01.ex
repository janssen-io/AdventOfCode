defmodule Year2022.Day01 do
  @doc ~S"""
  iex> AdventOfCode.example(2022, 01)
  ...> |> Year2022.Day01.p1
  24_000
  """
  def p1(lines) do
    lines
    |> calories_by_elf()
    |> hd
  end

  @doc ~S"""
  iex> AdventOfCode.example(2022, 01)
  ...> |> Year2022.Day01.p2
  45_000
  """
  def p2(lines) do
    lines
    |> calories_by_elf()
    |> Enum.take(3)
    |> Enum.sum()
  end

  def calories_by_elf(lines) do
    lines
    |> Stream.chunk_by(&(&1 != ""))
    |> Stream.reject(&(&1 == [""]))
    |> Stream.map(fn elf -> Enum.map(elf, &String.to_integer/1) end)
    |> Stream.map(&Enum.sum/1)
    |> Enum.sort(&>=/2)
  end
end
