defmodule DayOne do
    def readAndSolve(filename, solve, delims \\ ["\r\n", "\n"], trim \\ false) do
        File.open! filename, [:read], fn(file) ->
            IO.read(file, :all)
            |> then(fn s -> String.split(s, delims, trim: trim) end)
            |> solve.()
        end
    end

    def solve(elves) do
        elves 
        |> Enum.map(fn elf -> Enum.map(String.split(elf, ["\n"], trim: true), &String.to_integer/1) end) 
        |> Enum.map(&Enum.sum/1) 
        |> Enum.sort(&>=/2)
    end

end
    
DayOne.readAndSolve("01.input", &DayOne.solve/1, ["\r\n\r\n", "\n\n"])
|> Enum.take(3)
|> IO.inspect
|> Enum.sum
|> IO.inspect