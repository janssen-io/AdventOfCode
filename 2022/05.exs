Code.require_file("../aoc.exs")

defmodule Puzzle do
  def solve([stacks, instructions]) do
    crates = parse(stacks)
    Mover.new()
    instructions
    |> String.split("\n", trim: true)
    |> then(fn ins -> [
          process(crates, ins, &Enum.reverse/1),
          process(crates, ins),
        ] end)
    |> Enum.map(fn part -> Enum.map(part, &hd/1) |> Enum.join end)
    |> IO.inspect
  end

  def process(crates, instructions) do
    process(crates, instructions, fn x -> x end)
  end

  def process(crates, instructions, f) do
    instructions 
    |> Enum.reduce(crates, fn (instruction, cs) ->
      [[n], [a], [b]] = Regex.scan(~r/\d+/, instruction)
      [amount, from, to] = Enum.map([n, a, b], &String.to_integer/1)
      Mover.take(cs, amount, from - 1)
      |> Mover.place(to - 1, f)
    end)
  end

  def parse(stacks) do
    stacks
      |> String.split(["\r\n", "\n"])
      |> Enum.take_while(&(!Regex.match?(~r/\d/, &1)))
      |> Enum.map(&(String.split(&1, "", trim: true)))
      # Transpose
      |> Enum.zip_with(&id/1)
      # Remove invalid values
      |> Enum.map(&filter_letters/1)
      |> Enum.reject(&empty?/1)
  end

  def id(x) do x end
  def filter_letters(xs) do Enum.filter(xs, &letter?/1) end
  def letter?(c) do Regex.match?(~r/[A-Z]/, c) end
  def empty?(xs) do Enum.count(xs) == 0 end
end

defmodule Mover do
  def new do
    Agent.start_link(fn -> [] end, name: :mover)
  end

  def take(stacks, amount, from) do
    for {stack, index} <- Enum.with_index(stacks) do
      if index == from do
        h = Enum.take(stack, amount)
        Agent.update(:mover, fn list -> h ++ list end)
        Enum.drop(stack, amount)
      else
        stack
      end
    end
  end

  def place(stacks, to) do
    place(stacks, to, fn x -> x end)
  end

  def place(stacks, to, f) do
    cargo = Agent.get(:mover, fn list -> list end)
    Agent.update(:mover, fn _ -> [] end)
    for {stack, index} <- Enum.with_index(stacks) do
      if index == to do
        f.(cargo) ++ stack
      else
        stack
      end
    end
  end

end

Aoc.readAndSolve("05.input", &Puzzle.solve/1, ["\r\n\r\n", "\n\n"])

