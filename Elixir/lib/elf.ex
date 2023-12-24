defmodule Elf do
  defguard is_digit(c) when c in ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

  @doc ~S"""
    ## Example
    iex> Elf.get_ints("Part 1: 10 12 14 -15", :-)
    ["1", "10", "12", "14", "-15"]
  """
  def get_ints(string, :-) do
    Regex.scan(~r/-?\d+/, string)
    |> List.flatten()
  end

  def get_ints(string, :+) do
    Regex.scan(~r/\d+/, string)
    |> List.flatten()
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
    |> List.flatten()
  end

  def fst([a | _]), do: a
  def fst({a, _}), do: a
  def fst({a, _, _}), do: a
  def snd([_, b | _]), do: b
  def snd({_, b, _}), do: b

  def uncurry(f), do: fn {a, b} -> f.(a, b) end

  def sign(0), do: 0
  def sign(x) when x < 0, do: -1
  def sign(x) when x > 0, do: 1

  @doc """
      iex> Year2023.Day11.pairs([1,2,3,4,5])
      [{1,2}, {1,3}, {1,4}, {1,5}, {2,3}, {2,4}, {2,5}, {3,4}, {3,5}, {4,5}]
  """
  def pairs([]), do: []

  def pairs([x | ys]) do
    Enum.map(ys, fn y -> {x, y} end)
    |> Enum.concat(pairs(ys))
  end

  @doc ~S"""
    iex> Elf.lcm(2, 3)
    6
    iex> Elf.lcm(4, 4)
    4
    iex> Elf.lcm(6, 9)
    18
  """
  def lcm(a, b), do: div(a * b, Integer.gcd(a, b))

  def lcm([x]), do: x
  def lcm([x, y]), do: lcm(x, y)
  def lcm([x, y | xs]), do: lcm([lcm(x, y) | xs])

  @doc ~S"""
  iex> Elf.convert_range({5, 10}, {20, 30}, 8)
  23
  iex> Elf.convert_range({5, 10}, {20, 30}, 8.0)
  23.0
  """
  def convert_range(a, b, n) when is_integer(n),
    do: convert_range(a, b, n + 0.0) |> Float.round() |> trunc

  def convert_range({x1, x2}, {y1, y2}, n) when is_float(n), do: (n - x1) / x2 * (y2 - y1) + y1
end

defmodule Point do
  def add({x1, y1, z1}, {x2, y2, z2}), do: {x1 + x2, y1 + y2, z1 + z2}
  def add({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}

  def sub({x1, y1, z1}, {x2, y2, z2}), do: {x1 - x2, y1 - y2, z1 - z2}
  def sub({x1, y1}, {x2, y2}), do: {x1 - x2, y1 - y2}

  def mul({x1, y1, z1}, {x2, y2, z2}), do: {x1 * x2, y1 * y2, z1 * z2}
  def mul({x1, y1}, {x2, y2}), do: {x1 * x2, y1 * y2}

  def len({x1, y1, z1}), do: (x1 ** 2 + y1 ** 2 + z1 ** 2) ** 0.5
  def len({x1, y1}), do: (x1 ** 2 + y1 ** 2) ** 0.5

  def lhs +++ rhs, do: add(lhs, rhs)
  def lhs --- rhs, do: sub(lhs, rhs)
end

defmodule Memo do
  def new(name), do: Agent.start_link(fn -> %{} end, name: name)

  def get(name, key, default \\ nil), do: Agent.get(name, &Map.get(&1, key, default))
  def set(name, key, value), do: Agent.update(name, &Map.put(&1, key, value))
  def update(name, key, f, default \\ 0), do: Agent.update(name, &Map.update(&1, key, default, f))

  def get_or_set(name, key, factory) do
    cached = get(name, key)

    if cached != nil do
      update(name, :hits, &(&1 + 1), 1)
      hits = get(name, :hits)

      if rem(hits, 1000) == 0 do
        misses = get(name, :misses)
        IO.inspect({hits, misses})
      end

      cached
    else
      update(name, :misses, &(&1 + 1), 1)
      misses = get(name, :misses)

      if rem(misses, 1000) == 0 do
        hits = get(name, :hits, 0)
        IO.inspect({hits, misses})
      end

      value = factory.()
      set(name, key, value)
      value
    end
  end
end
