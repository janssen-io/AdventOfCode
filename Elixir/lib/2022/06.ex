defmodule Year2022.Day06 do
  @doc ~S"""
  iex> ["bvwbjplbgvbhsrlpgdmjqwftvncz"]
  ...> |> Year2022.Day06.p1
  5
  iex> ["nppdvjthqldpwncqszvftbrmjlhg"]
  ...> |> Year2022.Day06.p1
  6
  iex> ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]
  ...> |> Year2022.Day06.p1
  10
  iex> ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]
  ...> |> Year2022.Day06.p1
  11
  """
  def p1(lines) do
    lines = Enum.take(lines, 1)
    solver(hd(lines), 4) + 4
  end

  @doc ~S"""
  iex> ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]
  ...> |> Year2022.Day06.p2
  19
  iex> ["bvwbjplbgvbhsrlpgdmjqwftvncz"]
  ...> |> Year2022.Day06.p2
  23
  iex> ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]
  ...> |> Year2022.Day06.p2
  29
  iex> ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]
  ...> |> Year2022.Day06.p2
  26
  """
  def p2(lines) do
    lines = Enum.take(lines, 1)
    solver(hd(lines), 14) + 14
  end

  defp solver(instructions, size) do
    instructions
    |> String.graphemes()
    |> findMarker(size)
  end

  defp findMarker(signal, size) do
    signal
    |> Enum.with_index()
    |> Enum.map(fn {char, idx} -> Enum.slice(signal, idx, size) end)
    |> Enum.find_index(fn marker -> Enum.uniq(marker) |> Enum.count() == size end)
  end
end
