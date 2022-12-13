Code.require_file("../aoc.exs")

defmodule Puzzle do
    def solve(instructions) do
        solver(hd(instructions), 4) + 4
    end

    def solve2(instructions) do
        solver(hd(instructions), 14) + 14
    end

    defp solver(instructions, size) do
        instructions
        |> String.graphemes()
        |> findMarker(size)
    end

    defp findMarker(signal, size) do
        signal
        |> Enum.with_index
        |> Enum.map(fn {char, idx} -> Enum.slice(signal, idx, size) end)
        |> Enum.find_index(fn marker -> (Enum.uniq(marker) |> Enum.count) == size end)
    end

end

Aoc.readAndSolve("06.input", &Puzzle.solve/1)
|> IO.inspect(label: "Part 1")

Aoc.readAndSolve("06.input", &Puzzle.solve2/1)
|> IO.inspect(label: "Part 2")
