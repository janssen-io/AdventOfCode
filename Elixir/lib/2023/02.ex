defmodule Year2023.Day02 do
  def p1(lines) do
    lines
    # remove impossible games
    |> Stream.filter(fn l -> get_max(l, "red") <= 12 end)
    |> Stream.filter(fn l -> get_max(l, "green") <= 13 end)
    |> Stream.filter(fn l -> get_max(l, "blue") <= 14 end)
    # get game numbers
    |> Stream.map(fn l -> Regex.scan(~r/\d+/, l) |> hd end)
    |> Stream.map(&hd/1)
    |> Stream.map(&String.to_integer/1)
    |> Enum.sum
  end

  def p2(lines) do
    lines
    # get minimum number of cubes required = max listed of every colour
    |> Stream.map(fn l -> [get_max(l, "red"), get_max(l, "green"), get_max(l, "blue")] end)
    # calculate power by multiplying them
    |> Stream.map(fn l -> Enum.reduce(l, &*/2) end)
    |> Enum.sum
  end

  def get_max(game, colour) do
    Regex.scan(~r/(\d+) #{colour}/, game)
    |> Enum.map(&List.last/1)
    |> Enum.map(&String.to_integer/1)
    |> Enum.max
  end
end
