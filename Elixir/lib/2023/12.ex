import Bitwise

defmodule Year2023.Day12 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 12)
  ...> |> Year2023.Day12.p1
  21
  iex> AdventOfCode.input(2023, 12)
  ...> |> Year2023.Day12.p1
  7792
  """
  def p1(lines) do
    Memo.new(:memo)

    lines
    |> Stream.map(&String.split/1)
    |> Stream.map(fn [map, groups] ->
      %{
        groups:
          groups
          |> String.split(",")
          |> Enum.map(&String.to_integer(&1, 10)),
        length: String.length(map),
        springs: map
      }
    end)
    |> Stream.map(fn data ->
      cached_nums(data.groups, data.length, data.springs)
    end)
    |> Enum.sum()
  end

  def p2(lines) do
    Memo.new(:memo)

    lines
    |> Stream.map(&String.split/1)
    |> Stream.map(fn [map, groups] ->
      ["#{map}?#{map}?#{map}?#{map}?#{map}", "#{groups},#{groups},#{groups},#{groups},#{groups}"]
    end)
    |> Stream.map(fn [map, groups] ->
      %{
        groups:
          groups
          |> String.split(",")
          |> Enum.map(&String.to_integer(&1, 10)),
        length: String.length(map),
        springs: map
      }
    end)
    |> Stream.map(fn data ->
      cached_nums(data.groups, data.length, data.springs)
    end)
    |> Enum.sum()
  end

  @doc """
  iex> Year2023.Day12.nums([1], 2, "#.")
  [
    [1,0],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1], 2, "?.")
  [
    [1,0],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1], 2, ".#")
  [
    [0,1],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1], 2, "??")
  [
    [1,0],
    [0,1],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1,1,1], 6, "??????")
  [
    [1,0,1,0,1,0],
    [1,0,1,0,0,1],
    [1,0,0,1,0,1],
    [0,1,0,1,0,1],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1,1,1], 6, "?#????")
  [
    [0,1,0,1,0,1],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1,1,1], 6, "?????#")
  [
    [1,0,1,0,0,1],
    [1,0,0,1,0,1],
    [0,1,0,1,0,1],
  ] |> Enum.count()
  iex> Year2023.Day12.nums([1,1,1], 6, "#????#")
  [
    [1,0,1,0,0,1],
    [1,0,0,1,0,1],
  ] |> Enum.count()
  """
  def nums(groups, length, spring, recurse \\ &nums/4)

  def nums([g], length, spring, _) do
    if required_space([g]) > length do
      []
    else
      # IO.inspect({g, length, spring})
      available_space = length

      positions(g, available_space)
      |> Enum.map(fn {_, tail} -> pad_right(tail, available_space, 0) end)
      |> Enum.filter(&fits(to_int(&1), spring, length))
      # |> IO.inspect
      |> Enum.count()
    end
  end

  def nums([g | gs], length, spring, recurse) do
    if required_space([g | gs]) > length do
      []
    else
      # -1 for 0 separator
      available_space = length - required_space(gs) - 1

      # IO.inspect({g, length, spring})
      # [{s, group}]
      new_heads = positions(g, available_space)
        |> Enum.filter(fn {s, arr} ->
          n = to_int(Enum.concat(arr, [0]))
          fits(n, spring, s+1)
        end)
      # |> IO.inspect

      # num_matches = new_heads |> Enum.count()

      num_tail_matches =
        new_heads
        |> Enum.map(fn {s, _} ->
          remainder = length - s - 1
          recurse.(gs, remainder, String.slice(spring, -remainder, remainder), recurse)
        end)
        |> Enum.sum()

      num_tail_matches


      # debug: build valid spring arrangements
      # num_matches
      #   |> Enum.flat_map(fn {s, head} ->
      #     remainder = length - s
      #     tails = recurse.(gs, remainder, String.slice(spring, -remainder, remainder), recurse)
      #     Enum.map(tails, fn tail -> Enum.concat(head, tail) end)
      #   end)
    end
  end

  def cached_nums(gorups, length, spring, recurse \\ &cached_nums/4)

  def cached_nums([g], length, spring, _) do
    Memo.get_or_set(:memo, {g, length, spring}, fn -> nums([g], length, spring) end)
  end

  def cached_nums([g | gs], length, spring, recurse) do
    Memo.get_or_set(:memo, {g, gs, length, spring}, fn -> nums([g | gs], length, spring, recurse) end)
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

  def spring_broken(string), do: spring_to_int(string, ~r/[.?]/, "#")
  def spring_puzzle(string), do: spring_to_int(string, ".", ~r/[#?]/)

  def spring_to_int(s, p0, p1) do
    s
    |> String.replace(p0, "0")
    |> String.replace(p1, "1")
    |> String.to_integer(2)
  end

  @spec to_int(any()) :: integer()
  # |> String.to_integer(2)
  def to_int(arrangement), do: Enum.join(arrangement)

  def fits(x, spring, size) do
    x = "1" <> String.pad_trailing(x, size, "0")

    n = String.to_integer(x, 2)
    broken = ("1" <> String.slice(spring, 0, size)) |> spring_broken()
    puzzle = ("1" <> String.slice(spring, 0, size)) |> spring_puzzle()

    # IO.inspect(%{
    #   x: x, n: n,
    #   spring: String.slice(spring, 0, size),
    #   broken: broken,
    #   puzzle: puzzle,
    #   broken_n: (broken &&& n),
    #   puzzle_n: (puzzle &&& n),
    #   size: size
    # })

    (broken &&& n) == broken and (puzzle &&& n) == n
  end
end
