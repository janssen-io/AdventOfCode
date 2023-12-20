defmodule Year2023.Day20 do
  defmodule Start do
    defstruct id: nil, outputs: [], signals: []
  end

  defmodule Con do
    defstruct id: nil, outputs: [], signals: [], inputs: Map.new()
  end

  defmodule Flip do
    defstruct id: nil, outputs: [], signals: [], state: :low
  end

  defmodule Output do
    defstruct id: nil, signals: [], state: :low
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 20)
  ...> |> Year2023.Day20.p1
  11687500
  """
  def p1(lines) do
    parse_machine(lines)
    |> simulate(1000)
    |> count_signals()
    |> then(fn %{high: h, low: l} -> h * l end)
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 20)
  ...> |> Year2023.Day20.p2
  0
  """
  def p2(lines) do
    0
  end

  def parse_machine(lines) do
    machine =
      Enum.reduce(lines, %{}, fn line, machine ->
        {id, module} = parse_module(line)
        Map.put(machine, id, module)
      end)

    modules = Map.keys(machine) |> MapSet.new()

    Map.values(machine)
    |> Enum.reduce(machine, fn %{id: id, outputs: outputs}, init_machine ->
      outputs
      |> Enum.reduce(init_machine, fn out, m ->
        if out in modules do
          m
        else
          Map.put(m, out, %Output{id: out})
        end
        |> Map.update!(out, register_input(id))
      end)
    end)
  end

  @doc """
      iex> Year2023.Day20.parse_module("%a -> inv, con")
      { "a", %Year2023.Day20.Flip{
        id: "a",
        outputs: ["inv", "con"],
        signals: [],
        state: :low
      }}
  """
  def parse_module(line) do
    [lhs, rhs] = String.split(line, " -> ")
    {type, name} = String.split_at(lhs, 1)
    outputs = String.split(rhs, ", ")

    module =
      case type do
        "b" -> %Start{id: name, outputs: outputs}
        "&" -> %Con{id: name, outputs: outputs}
        "%" -> %Flip{id: name, outputs: outputs}
      end

    {name, module}
  end

  def register_input(id) do
    fn mod ->
      case mod do
        %Con{inputs: inputs} -> %{mod | inputs: Map.put(inputs, id, :low)}
        _ -> mod
      end
    end
  end

  def simulate(machine, n) do
    1..n
    |> Enum.reduce(machine, fn _, new_machine -> run(new_machine) end)
  end

  def run(machine, queue \\ [{"roadcaster", {:button, :low}}])
  def run(machine, []), do: machine
  def run(machine, [{receiver, msg = {_sender, _signal}} | xs]) do
    {next_states, intermediate_machine} = process(machine, Map.get(machine, receiver), msg)

    next_machine =
      Map.update!(intermediate_machine, receiver, fn mod ->
        %{mod | signals: [msg | mod.signals]}
      end)

    run(next_machine, xs ++ next_states)
  end

  @doc """
  Processes a signal in a module with the name `name` given the state in the map `machine`.
  """
  def process(machine, module = %Start{outputs: outputs}, {_sender, signal}) do
    {Enum.map(outputs, fn id -> {id, {module.id, signal}} end), machine}
  end

  def process(machine, module = %Flip{outputs: outputs}, {_sender, :low}) do
    flop = %{module | state: flip(module.state)}
    {Enum.map(outputs, fn id -> {id, {module.id, flip(module.state)}} end), set(machine, flop)}
  end

  def process(machine, _module = %Flip{}, {_sender, :high}) do
    {[], machine}
  end

  def process(machine, module = %Con{outputs: outputs, inputs: inputs}, {sender, signal}) do
    next_con = %Con{module | inputs: Map.put(inputs, sender, signal)}
    on? = Map.values(next_con.inputs) |> Enum.all?(&(&1 == :high))
    forwarded_signal = if on?, do: :low, else: :high
    {Enum.map(outputs, fn id -> {id, {module.id, forwarded_signal}} end), set(machine, next_con)}
  end

  def process(machine, module = %Output{}, {_sender, signal}) do
    next_output = %Output{module | state: signal}
    {[], set(machine, next_output)}
  end

  def flip(:low), do: :high
  def flip(:high), do: :low

  def set(machine, module = %{id: id}) do
    Map.put(machine, id, module)
  end

  def count_signals(machine) do
    Map.values(machine)
    |> Enum.flat_map(fn %{ signals: s } -> Enum.map(s, fn { _, signal } -> signal end) end)
    |> Enum.frequencies()
  end
end
