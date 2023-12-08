defmodule Year2023.Day08 do
  alias Year2023.Day08.Nav, as: Nav
  alias Year2023.Day08.Ghost, as: Ghost
  @doc ~S"""
  iex> ["LR", "", "AAA = BBB BBB", "BBB = AAA ZZZ", "ZZZ = ZZZ ZZZ"]
  ...> |> Year2023.Day08.p1
  2
  iex> ["LLR", "", "AAA = BBB BBB", "BBB = AAA ZZZ", "ZZZ = ZZZ ZZZ"]
  ...> |> Year2023.Day08.p1
  6
  """
  def p1(lines) do
    is_start = fn start -> start == "AAA" end
    go_while = fn loc -> loc != "ZZZ" end
    act(lines, is_start, go_while)
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 08)
  ...> |> Year2023.Day08.p2
  6
  """
  def p2(lines) do
    is_start = fn start -> String.ends_with?(start, "A") end
    go_while = fn loc -> !String.ends_with?(loc, "Z") end
    act(lines, is_start, go_while)
  end

  def act(lines, is_start, go_while) do
    steps = Enum.take(lines, 1)
      |> hd
      |> String.trim()
      |> String.codepoints()

    Nav.new()

    starts = Stream.drop(lines, 2)
      |> Stream.map(fn line ->
        [[from], [left], [right]] = Regex.scan(~r/[A-Z]+/, line)
        Nav.set(from, left, right)
        from
      end)
      |> Enum.filter(is_start)

    r = Enum.map(starts, fn start ->
      Task.async(fn -> walk(start, steps, go_while) end)
    end)
      |> Task.await_many(10_000)
      |> Enum.reduce(fn n, lcm -> Elf.lcm(n, lcm) end)

    Enum.map(starts, &Ghost.stop/1)
    r
  end

  def walk(start, steps, stop_fn) do
    Ghost.new(start)

    steps
    |> Stream.cycle()
    |> Stream.map(&(Ghost.walk(start, &1)))
    |> Stream.take_while(stop_fn)
    |> Stream.run()

    Ghost.distance_travelled(start)
  end
end

defmodule Year2023.Day08.Ghost do
  alias Year2023.Day08.Nav, as: Nav

  def new(start) do
    Agent.start_link(fn -> Map.new() end, name: :"#{start}meta")
    set_position(start, start)
  end

  def stop(start) do
    Agent.stop(:"#{start}meta")
  end

  @spec walk(any(), <<_::8>>) :: any()
  def walk(start, step) do
    current_loc = current_position(start)
    increment_distance(start)
    next_loc = Nav.go(current_loc, step)
    set_position(start, next_loc)
    next_loc
  end

  def current_position(start) do
    Agent.get(:"#{start}meta", &(Map.get(&1, :location)))
  end

  def set_position(start, location) do
    Agent.update(:"#{start}meta", &(Map.put(&1, :location, location)))
  end

  def distance_travelled(start) do
    Agent.get(:"#{start}meta", &(Map.get(&1, :distance, 0)))
  end

  def increment_distance(start) do
    Agent.update(:"#{start}meta", &(Map.update(&1, :distance, 1, fn d -> d + 1 end)))
  end
end

defmodule Year2023.Day08.Nav do
  def new() do
    Agent.start_link(fn -> Map.new() end, name: :nav)
  end

  def go(loc, "L") do
    [left, _] = get(loc)
    left
  end

  def go(loc, "R") do
    [_, right] = get(loc)
    right
  end


  def set(loc, left, right) do
    Agent.update(:nav, &(Map.put(&1, loc, [left, right])))
  end

  defp get(loc) do
    Agent.get(:nav, &(Map.get(&1, loc)))
  end
end
