defmodule Year2023.Day14 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 14)
  ...> |> Year2023.Day14.p1
  136
  """
  def p1(lines) do
    grid =
      lines
      |> parse_grid()

    spin_cycle(grid, [&north/1], 2)
    |> score()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 14)
  ...> |> Year2023.Day14.p2
  64
  """
  def p2(lines) do
    grid =
      lines
      |> parse_grid()

    spin_cycle(grid, [&north/1, &west/1, &south/1, &east/1], 1_000_000_000)
    |> score()
  end

  def spin_cycle(map, directions, times) do
    y = Map.get(map, :max_y)
    x = Map.get(map, :max_x)

    max_tilts = %{
      &north/1 => y,
      &south/1 => y,
      &west/1 => x,
      &east/1 => x
    }

    {current_cycle, cycle_size, state} = spin(map, directions, max_tilts, %{})

    if times == current_cycle do
      state
    else
      fastforward = div(times - current_cycle, cycle_size)
      remaining_cycles = times - (current_cycle + fastforward * cycle_size)

      Enum.reduce(1..(remaining_cycles), state, fn _, result ->
        do_cycle(result, directions, max_tilts)
      end)
    end
  end

  def spin(map, directions, max_tilts, results, i \\ 1) do
    cycled = do_cycle(map, directions, max_tilts)
    last_seen = Map.get(results, cycled, nil)

    if last_seen == nil do
      spin(cycled, directions, max_tilts, Map.put(results, cycled, i), i + 1)
    else
      {i, i - last_seen, cycled}
    end
  end

  def do_cycle(map, directions, max_tilts) do
    Enum.reduce(directions, map, fn current_direction, cycle_grid ->
      Enum.reduce(0..Map.get(max_tilts, current_direction), cycle_grid, fn _i, new_grid ->
        tilt(new_grid, current_direction)
      end)
    end)
  end

  def parse_grid(lines) do
    lines
    |> Stream.with_index()
    |> Enum.reduce(%{}, fn {line, y}, final_map ->
      String.codepoints(line)
      |> Enum.with_index()
      |> Enum.reduce(final_map, fn {c, x}, map ->
        cond do
          c == "#" || c == "O" -> Map.put(map, {x, y}, c)
          true -> map
        end
        |> Map.update(:max_x, 0, fn mx -> if x > mx, do: x, else: mx end)
      end)
      |> Map.update(:max_y, 0, fn my -> if y > my, do: y, else: my end)
    end)
  end

  def tilt(map, direction) do
    map
    |> Map.keys()
    |> Enum.reduce(map, fn key, new_map ->
      value = Map.get(new_map, key)

      cond do
        key == :max_x || :max_y == key ->
          Map.put(new_map, :max_x, value)

        is_coord(key) ->
          move(new_map, key, direction)

        true ->
          raise "Key '#{key}' is neither max nor coord"
      end
    end)
  end

  def north({x, y}), do: {x, y - 1}
  def south({x, y}), do: {x, y + 1}
  def west({x, y}), do: {x - 1, y}
  def east({x, y}), do: {x + 1, y}

  def move(map, {x, y}, direction) do
    c = Map.get(map, {x, y})
    next = direction.({x, y})

    cond do
      c == "O" && is_valid(map, next) ->
        Map.delete(map, {x, y})
        |> Map.put(next, "O")

      c == "O" || c == "#" ->
        map

      true ->
        raise "unexpected #{c} at #{{x, y}}"
    end
  end

  def is_coord({_, _}), do: true
  def is_coord(_), do: false

  def is_valid(map, {x, y}) do
    in_bounds(map, {x, y}) &&
      Map.get(map, {x, y}) == nil
  end

  def in_bounds(map, {x, y}) do
    x >= 0 && x <= Map.get(map, :max_x) &&
      y >= 0 && y <= Map.get(map, :max_y)
  end

  def score(grid) do
    max_score = Map.get(grid, :max_y) + 1

    Map.filter(grid, fn {_k, v} -> v == "O" end)
    |> Map.keys()
    |> Enum.reduce(0, fn {_, y}, sum ->
      sum + (max_score - y)
    end)
  end
end
