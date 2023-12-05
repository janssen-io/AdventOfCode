defmodule AdventOfCode do
  @spec input(integer(), integer(), :example | :input) :: File.Stream.t()
  def input(year, day, input) do
    input_path(year, day, input)
    |> File.stream!()
  end

  @spec run_part(integer(), integer(), integer(), :example | :input) :: {:error, String.t()} | {:ok, String.t()}
  def run_part(year, day, part, input \\ :example) do
    module = module_name(year, day)
    input = input(year, day, input)

    case part do
      1 -> {:ok, module.part_1(input)}
      2 -> {:ok, module.part_2(input)}
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
