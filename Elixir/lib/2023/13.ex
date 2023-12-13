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
      h = find_reflection(grid)
      v = find_reflection(transpose(grid))
      100 * h + v
    end)
    |> Enum.sum()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 13)
  ...> |> Year2023.Day13.p2
  0
  """
  def p2(lines) do
    0
  end

  def find_reflection(rows, i \\ 1) do
    if i >= Enum.count(rows) do
      0
    else
      {l, r} = Enum.split(rows, i)
      {lc, rc} = { Enum.count(l), Enum.count(r) }
      if lc == 0 or rc == 0 do
        find_reflection(rows, i + 1)
      else
        l = Enum.reverse(l)
        {l, r} =
          if lc > rc do
            {Enum.take(l, rc), r}
          else
            {l, Enum.take(r, lc)}
          end

        # IO.inspect({i, l, r, Enum.zip(l, r)})

        is_reflected =
          Enum.zip(l, r)
          |> Enum.reduce(true, fn {rl, rr}, is_reflected ->
            is_reflected and rl == rr
          end)

        if is_reflected do
          i
        else
          find_reflection(rows, i + 1)
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
end
