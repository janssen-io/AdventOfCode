Code.require_file("../aoc.exs")

defmodule DayThree do
  import Aoc, only: [is_digit: 1]

  def solve(lines) do
    p1(lines) |> IO.inspect(label: 'p1')
    p2(lines) |> IO.inspect(label: 'p2')
  end

  def p1(lines) do
    create_schematic(lines)
    Schematic.coordinates(:symbols)
    |> Stream.flat_map(fn {x, y} ->
      for dx <- -1..1, dy <- -1..1 do
        Schematic.get_number(x + dx, y + dy)
      end
      |> Stream.reject(&is_nil/1)
      |> Enum.uniq
    end)
    |> Enum.sum
  end

  def p2(_lines) do
    0
  end

  def create_schematic(lines) do
    Schematic.new()
    lines
    |> Stream.with_index
    |> Stream.each(fn {line, y} ->
        line
        |> String.trim
        |> String.codepoints
        |> Stream.with_index
        |> Stream.each(fn {c, x} -> Schematic.put(x, y, c) end)
        |> Stream.run
      end)
    |> Stream.run
  end


end

defmodule Schematic do
  import Aoc, only: [is_digit: 1]
  def new() do 
    Agent.start_link(fn -> %{} end, name: :parts)
    Agent.start_link(fn -> %{} end, name: :symbols)
  end

  def put(x, y, value) do
    case value do
      "." -> nil
      "\n" -> nil
      c when is_digit(c) -> _put(x, y, c)
      _ -> _put(x, y, :symbol)
    end
  end

  def _put(x, y, :symbol) do
    Agent.update(:symbols, &Map.put(&1, {x, y}, true))
  end

  def _put(x, y, value) do
    Agent.update(:parts, &Map.put(&1, {x, y}, value))
  end

  def is_symbol(x, y) do
    Agent.get(:symbols, &Map.get(&1, {x, y}, false))
  end

  def get_number(x, y) do
    digit = Agent.get(:parts, &Map.get(&1, {x, y}, nil))
    case digit do
      nil -> nil
      _ -> (get_left(x - 1, y) <> digit <> get_right(x + 1, y))
        |> String.to_integer
    end
  end

  def get_left(x, y) do
    digit = Agent.get(:parts, &Map.get(&1, {x, y}, nil))
    case digit do
      nil -> ""
      _ -> get_left(x - 1, y) <> digit
    end
  end

  def get_right(x, y) do
    digit = Agent.get(:parts, &Map.get(&1, {x, y}, nil))
    case digit do
      nil -> ""
      _ -> digit <> get_right(x + 1, y)
    end
  end

  def coordinates(map) do
    Agent.get(map, &Map.keys(&1))
  end
end

# Aoc.readAndSolve("02.input", &DayTwo.solve/1, ["\r\n", "\n"], true)
File.stream!("03.input", [], :line)
|> DayThree.solve

