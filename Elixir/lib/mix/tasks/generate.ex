defmodule Mix.Tasks.Generate do
  use Mix.Task

  def run(args) do
    {year, day} = parse_args!(args)

    generate_solver(year, day)
    generate_tester(year, day)
    touch(year, day, "input")
    touch(year, day, "example")
    :ok
  end

  def parse_args!(args) do
    switches = [year: :integer, day: :integer]
    aliases = [y: :year, d: :day]

    opts =
      case OptionParser.parse(args, aliases: aliases, strict: switches) do
        {opts, [], []} -> opts
        {_, [], any} -> Mix.raise("Invalid option(s): #{inspect(any)}")
        {_, any, _} -> Mix.raise("Unexpected argument(s): #{inspect(any)}")
      end

    %{ year: year, day: day } = DateTime.utc_now()

    {
      opts[:year] || year,
      opts[:day] || day,
    }
  end

  def file_path(type, year, day, extension \\ "ex")
  def file_path("lib", year, day, extension) do
    Path.join(["lib", to_string(year), "#{width(day, 2)}.#{extension}"])
  end

  def file_path("test", year, day, extension) do
    Path.join(["test", to_string(year), "#{width(day, 2)}_test.#{extension}"])
  end

  defp width(number, field_width) do
    String.pad_leading("#{number}", field_width, "0")
  end

  def generate_solver(year, day) do
    template_dir = Path.join(["lib", "mix", "tasks"])
    template_loc = Path.join([template_dir, "solver.eex"])
    output_loc = file_path("lib", year, day)
    quoted = EEx.compile_file(template_loc)
    { content, _ } = Code.eval_quoted(quoted, year: year, day: width(day, 2))
    File.write("#{output_loc}", content)
    :ok
  end

  def generate_tester(year, day) do
    template_dir = Path.join(["lib", "mix", "tasks"])
    template_loc = Path.join([template_dir, "tester.eex"])
    output_loc = file_path("test", year, day, "exs")
    quoted = EEx.compile_file(template_loc)
    { content, _ } = Code.eval_quoted(quoted, year: year, day: width(day, 2))
    File.write("#{output_loc}", content)
    :ok
  end

  def touch(year, day, extension) do
    output_loc = file_path("lib", year, day, extension)
    File.write(output_loc, "")
  end
end
