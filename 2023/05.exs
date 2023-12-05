Code.require_file("../aoc.exs")

defmodule DayFive do
  import Aoc, only: [get_digits: 1, is_digit: 1]

  def solve(lines) do
    p1(lines) |> IO.inspect(label: 'p1', charlists: :as_lists)
  end

  def p1(lines) do
    parts = Stream.chunk_by(lines, fn x -> x == "\n" end)
    seeds = Enum.take(parts, 1) 
    |> hd |> hd 
    |> get_digits
    |> Enum.map(&String.to_integer/1)

    maps = parts
    |> Stream.drop(1)
    |> Stream.reject(fn lines -> lines == ["\n"] end)
    |> Enum.map(&parse_map/1)

    seeds
    |> Stream.map(fn s -> walk_map(maps, s, 0) end)
    |> Enum.min

  end

  def parse_map([_ | ranges]) do
    Enum.map(ranges, &get_digits/1)
    |> Enum.map(&Enum.map(&1, fn n -> String.to_integer n end))
  end

  def walk_map([], seed, _), do: seed
  def walk_map([map | maps], seed, n) do
    d = map_seed(map, seed)
    walk_map(maps, d, n + 1)
  end

  def map_seed([], seed), do: seed
  def map_seed([range | ranges], seed) do
    d = destination(seed, range)
    if d != nil do d else map_seed(ranges, seed) end
  end

  def destination(seed, [d, s, size]) do
    if (s <= seed and seed < s + size) do
      d + (seed - s)
    else
      nil
    end
  end
end

# Aoc.readAndSolve("02.input", &DayTwo.solve/1, ["\r\n", "\n"], true)
File.stream!("05.example.input", [], :line) |> DayFive.solve
File.stream!("05.input", [], :line) |> DayFive.solve

