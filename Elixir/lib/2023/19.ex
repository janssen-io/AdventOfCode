defmodule Year2023.Day19 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 19)
  ...> |> Year2023.Day19.p1
  19114
  """
  def p1(lines) do
    [instructions, parts] =
      lines
      |> Enum.chunk_by(&(&1 == ""))
      |> Enum.reject(&(&1 == [""]))

    instructions = Enum.map(instructions, &parse_instruction/1)

    machine =
      Enum.reduce(instructions, %{}, fn {name, eval}, tree ->
        Map.put(tree, name, eval)
      end)

    parts = parts |> Enum.map(&parse_part/1)

    Enum.filter(parts, fn part -> is_accepted(machine, part) end)
    |> Enum.map(&score/1)
    |> Enum.sum()
  end

  def score(%{x: x, m: m, a: a, s: s}), do: x + m + a + s

  @doc ~S"""
  iex> AdventOfCode.example(2023, 19)
  ...> |> Year2023.Day19.p2
  167409079868000
  """
  def p2(lines) do
    instructions = Enum.take_while(lines, &(&1 != ""))

    xmas = %{
      x: %{min: 1, max: 4000},
      m: %{min: 1, max: 4000},
      a: %{min: 1, max: 4000},
      s: %{min: 1, max: 4000}
    }

    valid_ranges =
      find_ranges(xmas, instructions, "in")
      |> Enum.filter(fn xmas ->
        Enum.all?(Map.keys(xmas), fn prop ->
          %{min: min, max: max} = Map.get(xmas, prop)
          min <= max
        end)
      end)

    Enum.map(valid_ranges, fn xmas ->
      Enum.reduce(Map.keys(xmas), 1, fn prop, product ->
        %{min: min, max: max} = Map.get(xmas, prop)
        product * (max - min + 1)
      end)
    end)
    |> Enum.sum()
  end

  def find_ranges(ranges, _instructions, "A"), do: [ranges]
  def find_ranges(_ranges, _instructions, "R"), do: []

  def find_ranges(ranges, instructions, name) do
    line = Enum.find(instructions, fn line -> String.starts_with?(line, "#{name}{") end)
    [_, right] = String.trim(line, "}") |> String.split("{")
    if_elses = String.split(right, ",")

    {children, _negations} =
      Enum.reduce(if_elses, {[], []}, fn c, {children, previous_conditions} ->
        inverted_conditions = Enum.map(previous_conditions, fn f -> fn xmas -> f.(xmas, true) end end)
        case String.split(c, ":") do
          [condition, state] ->
            shrink_range = create_range_modifier(condition)
            apply_condition = fn xmas -> shrink_range.(xmas, false) end
            next_children =
              Enum.reduce([ apply_condition | inverted_conditions], ranges, fn f, new_ranges -> f.(new_ranges) end)
              |> find_ranges(instructions, state)
            {next_children ++ children, [shrink_range | previous_conditions]}

          [state] ->
            next_children =
              Enum.reduce(inverted_conditions, ranges, fn f, new_ranges -> f.(new_ranges) end)
              |> find_ranges(instructions, state)
            {next_children ++ children, previous_conditions}
        end
      end)

    children
  end

  def create_range_modifier(condition) do
    property = String.slice(condition, 0, 1) |> String.to_atom()
    op = String.slice(condition, 1, 1) |> String.to_atom()
    rhs = Elf.get_ints(condition) |> hd() |> String.to_integer()

    filter = fn xmas, negated ->
      %{min: min, max: max} = Map.get(xmas, property)

      updated_range =
        case {op, negated} do
          {:>, false} -> %{min: max(rhs + 1, min), max: max}
          {:>, true} -> %{min: min, max: min(rhs, max)}
          {:<, false} -> %{min: min, max: min(rhs - 1, max)}
          {:<, true} -> %{min: max(rhs, min), max: max}
        end

      Map.put(xmas, property, updated_range)
    end

    filter
  end

  def parse_part(part) do
    [x, m, a, s] = Elf.get_ints(part, :-) |> Enum.map(&String.to_integer/1)
    %{x: x, m: m, a: a, s: s}
  end

  def parse_instruction(line) do
    [left, right] = String.split(line, "{")
    if_elses = String.split(right, ",") |> parse_if_elses()
    {left, if_elses}
  end

  @doc """
  iex> Enum.map(Year2023.Day19.parse_if_elses(["A}"]), & &1.(%{ x: 1}))
  ["A"]
  iex> Enum.map(Year2023.Day19.parse_if_elses(["x<2006:R","A}"]), & &1.(%{ x: 1}))
  ["R", "A"]
  iex> Enum.map(Year2023.Day19.parse_if_elses(["x>2006:R","A}"]), & &1.(%{ x: 1}))
  [false, "A"]
  """
  def parse_if_elses([otherwise]) do
    state = String.trim(otherwise, "}")
    [fn _ -> state end]
  end

  def parse_if_elses([condition | cases]) do
    [comp, state] = String.split(condition, ":")
    [property, value] = String.splitter(comp, ["<", ">"]) |> Enum.take(2)
    property = String.to_atom(property)
    int = String.to_integer(value)
    op = String.slice(comp, 1, 1)

    fun =
      case op do
        ">" -> fn p -> Map.get(p, property) > int and state end
        "<" -> fn p -> Map.get(p, property) < int and state end
      end

    [fun | parse_if_elses(cases)]
  end

  def is_accepted(machine, part, workflow \\ "in")
  def is_accepted(_machine, _part, "R"), do: false
  def is_accepted(_machine, _part, "A"), do: true

  def is_accepted(machine, part, workflow) do
    funs = Map.get(machine, workflow)

    next =
      Enum.reduce(funs, false, fn f, acc ->
        if acc == false, do: f.(part), else: acc
      end)

    is_accepted(machine, part, next)
  end
end
