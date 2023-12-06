defmodule Elf do
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
end
