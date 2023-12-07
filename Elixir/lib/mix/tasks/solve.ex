defmodule Mix.Tasks.Solve do
  use Mix.Task

  def run(args) do
    {year, day, part, input} = parse_args!(args)

    case AdventOfCode.run_part(year, day, part, input) do
      {:ok, value} -> if is_binary(value), do: IO.puts(value), else: IO.inspect(value, charlists: :as_lists)
      {:error, reason} -> IO.warn(reason)
    end
  end

  def parse_args!(args) do
    switches = [year: :integer, day: :integer, part: :integer, example: :boolean]
    aliases = [y: :year, d: :day, p: :part, x: :example]

    opts =
      case OptionParser.parse(args, aliases: aliases, strict: switches) do
        {opts, [], []} -> opts
        {_, [], any} -> Mix.raise("Invalid option(s): #{inspect(any)}")
        {_, any, _} -> Mix.raise("Unexpected argument(s): #{inspect(any)}")
      end

    %{ year: year, day: day } = DateTime.utc_now()

    input =
      if opts[:example] do
        :example
      else
        :input
      end

    {
      opts[:year] || year,
      opts[:day] || day,
      opts[:part] || 1,
      input
    }
  end
end
