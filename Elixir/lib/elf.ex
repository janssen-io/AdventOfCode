defmodule Elf do
  defguard is_digit(c) when c in ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

  @doc ~S"""
    ## Example
    iex> Elf.get_ints("Part 1: 10 12 14 -15", :-)
    ["1", "10", "12", "14", "-15"]
  """
  def get_ints(string, :-) do
    Regex.scan(~r/-?\d+/, string)
    |> List.flatten
  end

  def get_ints(string, :+) do
    Regex.scan(~r/\d+/, string)
    |> List.flatten
  end
  @doc ~S"""
    iex> Elf.get_ints("Part 1: 10 12 14 -15")
    ["1", "10", "12", "14", "15"]
  """
  def get_ints(string), do: get_ints(string, :+)

  @doc ~S"""
    ## Example
    iex> Elf.parse_ints("Part 1: 10 12 14 -15")
    [1, 10, 12, 14, 15]
    iex> Elf.parse_ints("Part 1: 10 12 14 -15", :-)
    [1, 10, 12, 14, -15]
  """
  def parse_ints(string, sign \\ :+) do
    get_ints(string, sign)
    |> Enum.map(&String.to_integer/1)
  end

  @doc ~S"""
    ## Example
    iex> Elf.get_numbers("Part 1: 10.33 14 -15")
    ["1", "10.33", "14", "-15"]
  """
  def get_numbers(string) do
    Regex.scan(~r/-?\d+(?:\.\d+)?/, string)
    |> List.flatten
  end

  def fst([a, _]), do: a
  def fst({a, _}), do: a
  def snd([_, b]), do: b
  def snd({_, b}), do: b

  @doc ~S"""
    iex> Elf.lcm(2, 3)
    6
    iex> Elf.lcm(4, 4)
    4
    iex> Elf.lcm(6, 9)
    18
  """
  def lcm(a, b), do: div((a * b), Integer.gcd(a, b))

  @doc ~S"""
  iex> Elf.convert_range({5, 10}, {20, 30}, 8)
  23
  iex> Elf.convert_range({5, 10}, {20, 30}, 8.0)
  23.0
  """
  def convert_range(a, b, n) when is_integer(n), do: convert_range(a, b, n + 0.0) |> Float.round() |> trunc
  def convert_range({x1, x2}, {y1, y2}, n) when is_float(n), do: ((n - x1) / x2) * (y2 - y1) + y1

end
