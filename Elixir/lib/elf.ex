defmodule Elf do
  defguard is_digit(c) when c in ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

  @doc ~S"""
    ## Example
    iex> Elf.get_digits("Part 1: 10 12 14 -15")
    ["1", "10", "12", "14", "15"]
  """
  def get_digits(string) do
    Regex.scan(~r/\d+/, string)
    |> List.flatten
  end

  @doc ~S"""
    ## Example
    iex> Elf.get_numbers("Part 1: 10.33 14 -15")
    ["1", "10.33", "14", "-15"]
  """
  def get_numbers(string) do
    Regex.scan(~r/-?\d+(?:\.\d+)?/, string)
    |> List.flatten
  end

  def fst([a, _]), do: a
  def fst({a, _}), do: a
  def snd([_, b]), do: b
  def snd({_, b}), do: b
end
