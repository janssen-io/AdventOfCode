defmodule Aoc do
    def readAndSolve(filename, solve, delims \\ ["\r\n", "\n"], trim \\ false) do
        File.open! filename, [:read], fn(file) ->
            IO.read(file, :all)
            |> then(fn s -> String.split(s, delims, trim: trim) end)
            |> solve.()
        end
    end
end
