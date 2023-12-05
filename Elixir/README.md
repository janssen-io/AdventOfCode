# AdventOfCode

## Running the code
mix solve defaults to running the current day, part 1 for the input file.

```
$ mix solve [--year 2023] [--day 5] [--part 1] [--example]
$ mix solve [-y 2023] [-d 5] [-p 1] [-x]
```

## Adding new solutions
1. Add a directory with the year in lib: `./lib/2023`
2. Add a file with the name of the day for the example input: `01.example`
3. Add a file with the name of the day for the puzzle input: `01.input`
4. Add a file with the name of the day for your solution: `01.ex`
5. Add a module with the name: `Year2023.Day01` and the methods `part_1/1` and `part_2/1`.  
   Both methods must accept a File.Stream (lines) as the first and only argument. 

`lib/20xx` shows an example:
```elixir
defmodule Year20xx.Day01 do
  @doc ~S"""
  ## Examples

    iex> Year20xx.Day01.part_1(["123"])
    ["123"]
  """
  def part_1(input) do
    input
    |> Enum.into([])
  end

  def part_2(input) do
    input
    |> Enum.into([])
  end
```

## Testing the code
Run `mix test` to run the tests.

To add a test:

1. Add a directory with the year in lib: `./test/2023`
2. Add a file with the name of the day: `01_test.exs`  
   The `_test.exs` suffix is required for Mix to detect the tests.
3. Add a module with tests and/or use doctests:  

`test/20xx` shows an example:
```elixir
    defmodule Year20xx.Day01Tests do
      use ExUnit.Case
      doctest Year20xx.Day01
    end
```

