Code.require_file("../aoc.exs")

defmodule DayTwo do
  def solve(lines) do
    max = %{
      red: 12,
      green: 13,
      blue: 14,
    }
    lines
    |> Stream.filter(fn l -> get_max(l, "red") <= max.red end)
    |> Stream.filter(fn l -> get_max(l, "green") <= max.green end)
    |> Stream.filter(fn l -> get_max(l, "blue") <= max.blue end)
    |> Stream.map(fn l -> Regex.scan(~r/\d+/, l) |> hd end)
    |> Stream.map(&hd/1)
    |> Stream.map(&String.to_integer/1)
    |> Enum.sum
    |> IO.inspect
  end

  def get_max(game, colour) do
    Regex.scan(~r/(\d+) #{colour}/, game)
    |> Enum.map(&List.last/1)
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce(&reduce_max/2)
    |> IO.inspect(label: "max #{colour}")
  end

  def reduce_max(next, max) do
    if next > max do
      next
    else
      max
    end
  end

end

Aoc.readAndSolve("02.input", &DayTwo.solve/1, ["\r\n", "\n"], true)

