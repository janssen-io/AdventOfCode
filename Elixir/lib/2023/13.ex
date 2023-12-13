import Bitwise

defmodule Year2023.Day13 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 13)
  ...> |> Year2023.Day13.p1
  405
  """
  def p1(lines) do
    score(lines, &is_exact/2, 0)
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 13)
  ...> |> Year2023.Day13.p2
  400
  """
  def p2(lines) do
    score(lines, &is_either/2, 1)
  end

  def score(lines, is_mirror, num_smudges) do
    lines
    |> Stream.chunk_by(fn l -> l == "" end)
    |> Stream.reject(fn chunk -> chunk == [""] end)
    |> Stream.map(fn grid ->
      h = find_reflection(grid, is_mirror, num_smudges)
      v = find_reflection(transpose(grid), is_mirror, num_smudges)
      100 * h + v
    end)
    |> Enum.sum()
  end

  def find_reflection(rows, is_mirror, total_smudges \\ 0, i \\ 1) do
    {l, r} = Enum.split(rows, i)

    cond do
      i >= Enum.count(rows) ->
        0

      Enum.count(l) == 0 or Enum.count(r) == 0 ->
        find_reflection(rows, is_mirror, total_smudges, i + 1)

      true ->
        {smudges, is_reflected} = count_smudges(Enum.reverse(l), r, is_mirror)

        if is_reflected and smudges == total_smudges do
          i
        else
          find_reflection(rows, is_mirror, total_smudges, i + 1)
        end
    end
  end

  def count_smudges(l, r, is_mirror) do
    Enum.zip(l, r)
    |> Enum.reduce({0, true}, fn {row_left, row_right}, {smudges, is_reflected} ->
      {bin_left, bin_right} = {to_int(row_left), to_int(row_right)}

      {
        smudges + ((is_smudged(bin_left, bin_right) && 1) || 0),
        is_reflected and is_mirror.(bin_left, bin_right)
      }
    end)
  end

  def transpose(lines) do
    lines
    |> Enum.map(&String.codepoints/1)
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&Enum.join/1)
  end

  def to_int(row) do
    bits =
      row
      |> String.replace(".", "0")
      |> String.replace("#", "1")

    String.to_integer("1" <> bits, 2)
  end

  def is_exact(left, right), do: left - right == 0

  @doc """
  iex> Year2023.Day13.is_smudged(0b1000, 0b1001)
  true
  iex> Year2023.Day13.is_smudged(0b1001, 0b1000)
  true
  iex> Year2023.Day13.is_smudged(0b1100, 0b1011)
  false
  """
  def is_smudged(left, right) do
    diff = bxor(left, right)
    !is_exact(left, right) and (diff &&& diff - 1) == 0
  end

  def is_either(left, right), do: is_exact(left, right) or is_smudged(left, right)
end
