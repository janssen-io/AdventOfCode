defmodule Year2023.Day09 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 09)
  ...> |> Year2023.Day09.p1
  18 + 28 + 68
  """
  def p1(lines) do
    lines
    |> Stream.map(&Elf.parse_ints(&1, :-))
    |> Stream.map(&extend_series(&1, fn xs, inc -> List.last(xs) + inc end))
    |> Enum.sum()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 09)
  ...> |> Year2023.Day09.p2
  2
  """
  def p2(lines) do
    lines
    |> Stream.map(&Elf.parse_ints(&1, :-))
    |> Stream.map(&extend_series(&1, fn xs, inc -> hd(xs) - inc end))
    |> Enum.sum()
  end

  @doc ~S"""
  iex> Year2023.Day09.difference_list([1,2,3])
  {[1,1], true}
  iex> Year2023.Day09.difference_list([1,1,2,3,5,8,13])
  {[0,1,1,2,3,5], false}
  """
  def difference_list(xs) do
    {diff, is_stable} = difference_list({[], true}, xs)
    {Enum.reverse(diff), is_stable}
  end

  def difference_list({[c | acc], is_stable}, [a, b | xs]) do
    {[b - a, c | acc], is_stable and b - a == c}
    |> difference_list([b | xs])
  end

  def difference_list({[], _}, [a, b | xs]) do
    {[b - a], true}
    |> difference_list([b | xs])
  end

  def difference_list(result, [_]), do: result

  @doc ~S"""
  iex> Year2023.Day09.extend_series([1,2,3])
  4
  iex> Year2023.Day09.extend_series([1,3,6,10,15,21])
  28
  iex> Year2023.Day09.extend_series([1,1,2,3,5])
  11
  """
  def extend_series(xs, f) do
    {diff, is_stable} = difference_list(xs)

    increment = if is_stable do
      hd(diff)
    else
      extend_series(diff, f)
    end

    f.(xs, increment)
  end
end
