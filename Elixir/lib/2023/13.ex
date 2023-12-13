import Bitwise

defmodule Year2023.Day13 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 13)
  ...> |> Year2023.Day13.p1
  405
  """
  def p1(lines) do
    lines
    |> Stream.chunk_by(fn l -> l == "" end)
    |> Stream.reject(fn chunk -> chunk == [""] end)
    |> Stream.map(fn grid ->
      h = find_reflection(grid, &is_exact/2, 0)
      v = find_reflection(transpose(grid), &is_exact/2, 0)
      100 * h + v
    end)
    |> Enum.sum()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 13)
  ...> |> Year2023.Day13.p2
  400
  """
  def p2(lines) do
    lines
    |> Stream.chunk_by(fn l -> l == "" end)
    |> Stream.reject(fn chunk -> chunk == [""] end)
    |> Stream.map(fn grid ->
      h = find_reflection(grid, &is_either/2, 1)
      v = find_reflection(transpose(grid), &is_either/2, 1)
      IO.inspect({h, v})
      100 * h + v
    end)
    |> Enum.sum()
  end

  def find_reflection(rows, is_mirror, total_smudges \\ 0, i \\ 1) do
    if i >= Enum.count(rows) do
      0
    else
      {l, r} = Enum.split(rows, i)
      {lc, rc} = {Enum.count(l), Enum.count(r)}

      if lc == 0 or rc == 0 do
        find_reflection(rows, is_mirror, total_smudges, i + 1)
      else
        l = Enum.reverse(l)

        {l, r} =
          if lc > rc do
            {Enum.take(l, rc), r}
          else
            {l, Enum.take(r, lc)}
          end

        {smudges, is_reflected} =
          Enum.zip(l, r)
          |> Enum.reduce({0, true}, fn {rl, rr}, {smudges, is_reflected} ->
            {bl, br} = {to_int(rl), to_int(rr)}
            smudges = if is_smudged(bl, br), do: smudges + 1, else: smudges
            {smudges, is_reflected and is_mirror.(bl, br)}
          end)

        if is_reflected and smudges == total_smudges do
          i
        else
          find_reflection(rows, is_mirror, total_smudges, i + 1)
        end
      end
    end
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
  def is_smudged(left, right) when left < right do
    is_smudged(right, left)
  end

  def is_smudged(left, right) do
    diff = bxor(left, right)
    !is_exact(left, right) and (diff &&& diff - 1) == 0
  end

  def is_either(left, right), do: is_exact(left, right) or is_smudged(left, right)
end
