Code.require_file("../aoc.exs")

defmodule DayFour do
  import Aoc, only: [is_digit: 1]

  def solve(lines) do
    p1(lines) |> IO.inspect(label: 'p1')
    p2(lines) |> IO.inspect(label: 'p2')
  end

  def p1(lines) do
    lines
    |> Stream.map(&calculate/1)
    |> Enum.sum()
  end

  def p2(lines) do
  end

  def calculate(line) do
    [left, right] = String.split(line, "|")
    winning = get_digits(left) |> tl |> MapSet.new
    hand = get_digits(right) |> MapSet.new
    size = MapSet.intersection(winning, hand)
      |> MapSet.size
    case size do
      0 -> 0
      n -> 2 ** (n - 1)
    end
  end

  def get_digits(string) do
    Regex.scan(~r/\d+/, string) |> List.flatten
  end

end

# Aoc.readAndSolve("02.input", &DayTwo.solve/1, ["\r\n", "\n"], true)
File.stream!("04.input", [], :line)
|> DayFour.solve

