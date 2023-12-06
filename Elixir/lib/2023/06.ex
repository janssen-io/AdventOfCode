defmodule Year2023.Day06 do
  @doc ~S"""
  ## Examples

    iex> Year2023.Day06.part_1(AdventOfCode.input(2023, 6, :example))
    288
  """
  def part_1(input) do
    input
    |> Enum.map(&Elf.get_digits/1)
    |> Enum.map(fn x -> Enum.map(x, &String.to_integer/1) end)
    |> Enum.zip()
    |> Enum.map(fn {time, distance} -> race(time, distance) end)
    |> Enum.product
  end

  @doc ~S"""
  ## Examples

    iex> Year2023.Day06.part_2(AdventOfCode.input(2023, 6, :example))
    71503
  """
  def part_2(input) do
    input
    |> Enum.map(&Elf.get_digits/1)
    |> Enum.map(&Enum.join/1)
    |> Enum.map(&String.to_integer/1)
    |> then(fn [time, distance] -> race(time, distance) end)
  end

  @doc ~S"""
  Calculate the number of mm travelled when charging the boat for the given number of ms
  ## Examples

    iex> Year2023.Day06.distance(0, 7)
    0
    iex> Year2023.Day06.distance(1, 7)
    6
    iex> Year2023.Day06.distance(2, 7)
    10
    iex> Year2023.Day06.distance(3, 7)
    12
    iex> Year2023.Day06.distance(5, 7)
    10
    iex> Year2023.Day06.distance(6, 7)
    6
  """
  def distance(charge_time, total_time) do
    (total_time - charge_time) * charge_time
  end

  @doc ~S"""
  Calculate the number of possible ways to win the race
  ## Examples

    iex> Year2023.Day06.race(7, 9)
    4
    iex> Year2023.Day06.race(15, 40)
    8
    iex> Year2023.Day06.race(30, 200)
    9
    iex> Year2023.Day06.race(71530, 940200)
    71503
  """
  def race(time, distance_to_beat) do
    Stream.map(1..(time - 1), &distance(&1, time))
    |> Stream.filter(&(&1 > distance_to_beat))
    |> Enum.count
  end
end
