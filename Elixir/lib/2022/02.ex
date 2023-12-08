defmodule Year2022.Day02 do
  @doc ~S"""
  iex> AdventOfCode.example(2022, 02)
  ...> |> Year2022.Day02.p1
  8 + 1 + 6
  """
  def p1(instructions) do
    instructions
    |> Enum.map(&instructions_to_moves/1)
    |> Enum.map(&data_to_points/1)
    |> Enum.sum()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2022, 02)
  ...> |> Year2022.Day02.p2
  12
  """
  def p2(instructions) do
    instructions
    |> Enum.map(&instructions_to_moves/1)
    |> Enum.map(&moves_to_strategy/1)
    |> Enum.map(&data_to_points/1)
    |> Enum.sum()
  end

  defp instructions_to_moves(instructions) do
    String.split(instructions)
    |> Enum.map(&letter_to_move/1)
  end

  defp letter_to_move("A"), do: :rock
  defp letter_to_move("B"), do: :paper
  defp letter_to_move("C"), do: :scissors
  defp letter_to_move("X"), do: :rock
  defp letter_to_move("Y"), do: :paper
  defp letter_to_move("Z"), do: :scissors

  defp move_to_score(:rock), do: 1
  defp move_to_score(:paper), do: 2
  defp move_to_score(:scissors), do: 3

  defp get_move(start, distance) do
    moves = [:rock, :paper, :scissors]
    {_, index} = Enum.with_index(moves) |> Enum.find(fn {m, _} -> m == start end)

    Integer.mod(index + distance, 3)
    |> then(fn idx -> Enum.at(moves, idx) end)
  end

  defp lose_from(move), do: get_move(move, 2)
  defp win_from(move), do: get_move(move, 1)

  defp moves_to_score([a, b]) do
    cond do
      a == lose_from(b) -> 6
      a == b -> 3
      true -> 0
    end
  end

  defp moves_to_strategy([a, :paper]), do: [a, a]
  defp moves_to_strategy([a, :rock]), do: [a, lose_from(a)]
  defp moves_to_strategy([a, :scissors]), do: [a, win_from(a)]

  defp data_to_points([a, b]) do
    move_to_score(b) + moves_to_score([a, b])
  end
end
