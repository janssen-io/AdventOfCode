Code.require_file("../aoc.exs")

defmodule DayTwo do
    def solve(instructions) do
        instructions
        |> Enum.map(&get_compartments/1)
        |> score
    end

    def solve2(instructions) do
        instructions
        |> Enum.chunk_every(3)
        |> score
    end

    defp score(groups) do
        groups
        |> Enum.map(fn group ->
            group
            |> Enum.map(&to_charlist/1)
            |> Enum.map(&MapSet.new/1)
            |> get_duplicates
            |> get_priority
        end)
        |> Enum.sum
    end

    defp get_compartments(rucksack) do
        size = Kernel.div(String.length(rucksack), 2)
        [
            String.slice(rucksack, 0, size),
            String.slice(rucksack, size, size),
        ]
    end

    defp get_duplicates(compartments) do
        compartments
        |> Enum.reduce(&MapSet.intersection(&1, &2))
    end

    defp get_priority(items) do
        items
        |> MapSet.to_list
        |> hd
        |> case do
          p when p >= ?a -> p - ?a + 1
          p -> p - ?A + 27
        end
    end

end

Aoc.readAndSolve("03.input", &DayTwo.solve/1)
|> IO.inspect(label: "Part 1")

Aoc.readAndSolve("03.input", &DayTwo.solve2/1)
|> IO.inspect(label: "Part 2")
