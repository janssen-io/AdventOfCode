defmodule AdventOfCode do
  def input(year, day, input \\ :input) do
    input_path(year, day, input)
    |> File.stream!()
    |> Stream.map(&(String.trim(&1, "\n")))
  end

  def example(year, day), do: input(year, day, :example)

  @spec run_part(integer(), integer(), integer(), :example | :input) :: {:error, String.t()} | {:ok, String.t()}
  def run_part(year, day, part, input \\ :example) do
    module = module_name(year, day)
    input = input(year, day, input)

    case part do
      1 -> {:ok, module.p1(input)}
      2 -> {:ok, module.p2(input)}
      _ -> {:error, "no such part_#{part}"}
    end
  end

  defp input_path(year, day, input) do
    Path.join(["lib", "#{year}", "#{width(day, 2)}.#{input}"])
  end

  defp module_name(year, day) do
    Module.concat("Year#{year}", "Day#{width(day, 2)}")
  end

  defp width(number, field_width) do
    String.pad_leading("#{number}", field_width, "0")
  end
end
