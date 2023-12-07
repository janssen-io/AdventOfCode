Code.require_file("../aoc.exs")

defmodule Puzzle do
  def solve(instructions) do
    instructions
    |> Stream.reject(&(&1 == ""))
    |> Enum.reduce({[], %{}}, flip(&read/2))
    |> then(fn {_, fs} -> Map.to_list fs end)
    |> Enum.filter(fn {_, size} -> size <= 100_000 end)
    # |> Enum.sort_by(fn {k, _} -> k end)
    |> IO.inspect
    |> Enum.map(&snd/1)
    |> Enum.sum
  end

  def snd({_, x}) do x end

  def flip(f) do
    fn x, y -> f.(y, x) end
  end

  def read({[_|t], fs}, "$ cd ..") do
    {t, fs}
  end

  def read({pwd, fs}, "$ cd " <> dir) do
    {[dir|pwd], fs}
  end

  def read({pwd, fs}, "$ ls" <> _) do
    {pwd, fs}
  end

  def read({pwd, fs}, "dir " <> _) do
    {pwd, fs}
  end

  def read({pwd, fs}, size_spec) do
    size = String.split(size_spec, " ")
    |> hd
    |> String.to_integer

    updated_fs = Enum.reduce(pwd, fs, fn dir, acc ->
      Map.update(acc, dir, size, &(&1 + size))
    end)

    {pwd, updated_fs}
  end
end

Aoc.readAndSolve("07.example.input", &Puzzle.solve/1, ["\r\n", "\n"], trim: true)
|> IO.inspect(label: "Part 1")

Aoc.readAndSolve("07.input", &Puzzle.solve/1, ["\r\n", "\n"], trim: true)
|> IO.inspect(label: "Part 1")
