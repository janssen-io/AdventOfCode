Code.require_file("../aoc.exs")

defmodule Puzzle do
    def solve(instructions) do
        solver(instructions, &isWhollyContained/2)
    end

    def solve2(instructions) do
        solver(instructions, &isPartiallyContained/2)
    end

    defp solver(instructions, check) do
        instructions
        |> Enum.map(fn pair ->
            [left, right] = String.split(pair, ",")
            left = String.split(left, "-") |> Enum.map(&String.to_integer/1)
            right = String.split(right, "-") |> Enum.map(&String.to_integer/1)
            check.(left, right)
        end)
        |> Enum.count(fn x -> x end)
    end

    def isWhollyContained(left, right) do
        [l0, l1] = left
        [r0, r1] = right
        (l0 >= r0 && l1 <= r1) || (r0 >= l0 && r1 <= l1);
    end

    def isPartiallyContained(left, right) do
        [l0, l1] = left
        [r0, r1] = right
         (l0 >= r0 && l0 <= r1)
            || (l1 >= r0 && l1 <= r1)
            || (r0 >= l0 && r0 <= l1)
            || (r1 >= l0 && r1 <= l1)
    end

end

Aoc.readAndSolve("04.input", &Puzzle.solve/1)
|> IO.inspect(label: "Part 1")

Aoc.readAndSolve("04.input", &Puzzle.solve2/1)
|> IO.inspect(label: "Part 2")
