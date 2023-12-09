defmodule Year2023.Day09 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 09)
  ...> |> Year2023.Day09.p1
  18 + 28 + 68
  """
  def p1(lines) do
    lines
    |> Stream.map(&(Elf.parse_ints(&1, :-)))
    |> Stream.map(&extend_series/1)
    |> Enum.reduce(0, fn {_, last}, acc -> acc + last end)
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 09)
  ...> |> Year2023.Day09.p2
  0
  """
  def p2(lines) do
    0
  end

  @doc ~S"""
  # iex> Year2023.Day09.difference_list([1,2,3])
  # {[1,1], true, 1}
  iex> Year2023.Day09.difference_list([1,1,2,3,5,8,13])
  {[0,1,1,2,3,5], false, 5}
  """
  def difference_list(xs) do
    {diff, is_stable} = difference_list({[], true}, xs)
    {Enum.reverse(diff), is_stable, hd(diff)}
  end

  def difference_list({acc, is_stable}, [a, b | []]) do
    {[b - a | acc], is_stable}
  end

  def difference_list({[c | acc], is_stable}, [a, b | xs]) do
    {[b - a, c | acc], is_stable and b - a == c}
    |> difference_list([b | xs])
  end

  def difference_list({[], _}, [a, b | xs]) do
    {[b - a], true}
    |> difference_list([b | xs])
  end

  @doc ~S"""
  # iex> Year2023.Day09.extend_series([1,2,3])
  # {[1,2,3,4], 4}
  iex> Year2023.Day09.extend_series([1,3,6,10,15,21])
  {[1,3,6,10,15,21,28], 28}
  """
  def extend_series(xs) do
    {diff, is_stable, last} = difference_list(xs)

    if is_stable do
      append_add(xs, last)
    else
      {_, new_last} = extend_series(diff)
      append_add(xs, new_last)
    end
  end

  def append_add([x], last), do: {[x, x + last], x + last}

  def append_add([x | xs], last) do
    {ys, n} = append_add(xs, last)
    {[x | ys], n}
  end
end
