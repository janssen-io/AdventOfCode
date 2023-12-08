defmodule Year2022.Day07 do
  @doc ~S"""
  iex> AdventOfCode.example(2022, 07)
  ...> |> Year2022.Day07.p1
  94853 + 584
  """
  def p1(lines) do
    lines
    |> Stream.reject(&(&1 == ""))
    |> Enum.reduce({[], %{}}, flip(&read/2))
    |> then(fn {_, fs} -> Map.to_list fs end)
    |> Enum.filter(fn {_, size} -> size <= 100_000 end)
    # |> Enum.sort_by(fn {k, _} -> k end)
    |> Enum.map(&snd/1)
    |> Enum.sum
  end

  @doc ~S"""
  iex> AdventOfCode.example(2022, 07)
  ...> |> Year2022.Day07.p2
  0
  """
  def p2(lines) do
    0
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
