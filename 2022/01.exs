Code.require_file("../aoc.exs")

defmodule DayOne do
    def solve(elves) do
        elves
        |> Enum.map(fn elf -> Enum.map(String.split(elf, ["\n"]), &String.to_integer/1) end)
        |> Enum.map(&Enum.sum/1)
        |> Enum.sort(&>=/2)
        # part 1
        |> (tap(&(&1 |> hd |> IO.inspect(label: "p1"))))
        # part 2
        |> Enum.take(3)
        |> Enum.sum
        |> IO.inspect(label: "p2")
    end
end

Aoc.readAndSolve("01.input", &DayOne.solve/1, ["\r\n\r\n", "\n\n"])
