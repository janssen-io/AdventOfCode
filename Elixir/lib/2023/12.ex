import Bitwise

defmodule Year2023.Day12 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 12)
  ...> |> Year2023.Day12.p1
  0
  """
  def p1(lines) do
    lines
    |> Stream.map(&String.split/1)
    |> Stream.map(fn [map, groups] ->
      %{
        groups:
          groups
          |> String.split(",")
          |> Enum.map(&String.to_integer(&1, 10)),
        length: String.length(map),
        springs: %{
          min: map
            |> String.replace(~r/[.?]/, "0")
            |> String.replace("#", "1")
            |> String.to_integer(2),
          max: map
            |> String.replace(~r/\./, "0")
            |> String.replace(~r/[#?]/, "1")
            |> String.to_integer(2)
        }
      }
    end)
    |> Stream.map(fn data ->
      nums(data.groups, data.length, [])
      |> Enum.map(&Enum.join/1)
      |> Enum.map(&String.to_integer(&1, 2))
      |> Enum.reduce(0, fn arrangement, sum ->
        fits_broken = (data.springs.min &&& arrangement) == data.springs.min
        fits_puzzle = (data.springs.max &&& arrangement) == arrangement
        if fits_broken && fits_puzzle do
          sum + 1
        else
          sum
        end
      end)
    end)
    |> Enum.into([])
    |> Enum.sum
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 12)
  ...> |> Year2023.Day12.p2
  0
  """
  def p2(lines) do
    0
  end

  def nums(_, 0, _), do: []

  @doc """
  iex> Year2023.Day12.nums([1,1,1], 6, [])
  [
    [1,0,1,0,1,0],
    [1,0,1,0,0,1],
    [1,0,0,1,0,1],
    [0,1,0,1,0,1],
  ]
  iex> Year2023.Day12.nums([1], 3, [])
  [
    [1,0,0],
    [0,1,0],
    [0,0,1],
  ]
  iex> Year2023.Day12.nums([2], 4, [])
  [
    [1,1,0,0],
    [0,1,1,0],
    [0,0,1,1],
  ]
  iex> Year2023.Day12.nums([1,2], 5, [])
  [
    [1,0,1,1,0],
    [1,0,0,1,1],
    [0,1,0,1,1],
  ]
  iex> Year2023.Day12.nums([1,1,1,1,1], 20, []) |> Enum.count
  132
  """
  def nums([g], length, head) do
    if required_space([g]) > length do
      []
    else
      available_space = length

      positions(g, available_space)
      |> Enum.map(fn {_, tail} ->
        Enum.concat(
          head,
          pad_right(tail, available_space, 0)
        )
      end)
    end
  end

  def nums([g | gs], length, head) do
    if required_space([g | gs]) > length do
      []
    else
      # -1 for 0 separator
      available_space = length - required_space(gs) - 1

      new_heads =
        positions(g, available_space)
        |> Enum.map(fn {s, group} -> {s, Enum.concat([head, group, [0]])} end)

      Enum.flat_map(
        new_heads,
        fn {s, new_head} -> nums(gs, length - s - 1, new_head) end
      )
    end
  end

  @doc """
  iex> Year2023.Day12.required_space([1,1])
  3
  iex> Year2023.Day12.required_space([1,8])
  10
  iex> Year2023.Day12.required_space([1,1,1])
  5
  """
  def required_space(groups), do: Enum.sum(groups) + Enum.count(groups) - 1

  def pad_right(xs, size, elem \\ 0) do
    pad_size = size - Enum.count(xs)

    if pad_size > 0 do
      Enum.concat(xs, repeat([elem], pad_size))
    else
      xs
    end
  end

  def pad_left(xs, size, elem \\ 0) do
    pad_size = size - Enum.count(xs)

    if pad_size > 0 do
      Enum.concat(repeat([elem], pad_size), xs)
    else
      xs
    end
  end

  @doc """
  iex> Year2023.Day12.repeat([0], 4)
  [0,0,0,0]
  """
  def repeat(enumerable, n) do
    Stream.cycle(enumerable)
    |> Enum.take(n)
  end

  @doc ~S"""
    iex> Year2023.Day12.positions(1, 1) |> Enum.map(&Elf.snd/1)
    [[1]]
    iex> Year2023.Day12.positions(1, 4) |> Enum.map(&Elf.snd/1)
    [[1], [0, 1], [0, 0, 1], [0, 0, 0, 1]]
    iex> Year2023.Day12.positions(2, 2) |> Enum.map(&Elf.snd/1)
    [[1,1]]
    iex> Year2023.Day12.positions(2, 4) |> Enum.map(&Elf.snd/1)
    [[1,1], [0,1,1], [0,0,1,1]]
  """
  def positions(group_size, length) do
    group = repeat([1], group_size)
    if group_size > length, do: raise("#{group_size} > #{length}")

    for x <- group_size..length do
      {x, pad_left(group, x, 0)}
    end
  end
end
