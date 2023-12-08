defmodule Mix.Tasks.Benchmark do
  use Mix.Task

  def run(args) do
    {year, days, input} = parse_args!(args)
    benchmarks = Enum.reduce(days, %{}, fn day, acc ->
      Map.put(acc, "#{year}.#{day}.1", fn -> AdventOfCode.run_part(year, day, 1, input) end)
      |> Map.put("#{year}.#{day}.2", fn -> AdventOfCode.run_part(year, day, 2, input) end)
    end)
    Benchee.run(benchmarks)
  end

  def parse_args!(args) do
    switches = [year: :integer, days: :string]
    aliases = [y: :year, d: :days]

    opts =
      case OptionParser.parse(args, aliases: aliases, strict: switches) do
        {opts, [], []} -> opts
        {_, [], any} -> Mix.raise("Invalid option(s): #{inspect(any)}")
        {_, any, _} -> Mix.raise("Unexpected argument(s): #{inspect(any)}")
      end

    %{ year: year, day: day } = DateTime.utc_now()

    days = if (opts[:days]) do
      String.split(opts[:days]) |> Enum.map(&String.to_integer/1)
    else
      1..Enum.min([day - 1, 25])
    end

    input = if opts[:example], do: :example, else: :input

    {
      opts[:year] || year,
      days,
      input
    }
  end
end
