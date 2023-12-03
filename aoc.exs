defmodule Aoc do
    defguard is_digit(c) when c in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    def readAndSolve(filename, solve, delims \\ ["\r\n", "\n"], trim \\ false) do
        File.open! filename, [:read], fn(file) ->
            IO.read(file, :all)
            |> then(fn s -> String.split(s, delims, trim: trim) end)
            |> solve.()
        end
    end
end
