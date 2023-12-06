import Elf, only: [is_digit: 1]

defmodule Year2023.Day03 do
  alias(Year2023.Day03.Schematic, as: Schematic)
  def p1(lines) do
    create_schematic(lines)
    get_parts(:symbols)
    |> Stream.flat_map(&(&1))
    |> Enum.sum
  end

  def p2(lines) do
    create_schematic(lines)
    get_parts(:gears)
    |> Stream.filter(&(Enum.count(&1) == 2))
    |> Stream.map(&Enum.product/1)
    |> Enum.sum
  end

  def get_parts(map) do
    Schematic.coordinates(map)
    |> Stream.map(fn {x, y} ->
      for dx <- -1..1, dy <- -1..1 do
        Schematic.get_number(x + dx, y + dy)
      end
      |> Stream.reject(&is_nil/1)
      |> Enum.uniq
    end)
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

defmodule Year2023.Day03.Schematic do
  def new() do
    Agent.start_link(fn -> %{} end, name: :parts)
    Agent.start_link(fn -> %{} end, name: :symbols)
    Agent.start_link(fn -> %{} end, name: :gears)
  end

  def put(_, _, value) when value in [".", "\n"] do
    nil
  end

  def put(x, y, value) when is_digit(value) do
    Agent.update(:parts, &Map.put(&1, {x, y}, value))
  end

  # p2: keep track of gears `*` separately
  def put(x, y, "*") do
    Agent.update(:symbols, &Map.put(&1, {x, y}, true))
    Agent.update(:gears, &Map.put(&1, {x, y}, true))
  end

  def put(x, y, _) do
    Agent.update(:symbols, &Map.put(&1, {x, y}, true))
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
