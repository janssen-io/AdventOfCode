defmodule Year2023.Day15 do
  @doc ~S"""
  iex> AdventOfCode.example(2023, 15)
  ...> |> Year2023.Day15.p1
  1320
  """
  def p1(lines) do
    lines
    |> Enum.into([])
    |> hd()
    |> String.split(",")
    |> Enum.map(&hash/1)
    |> Enum.sum()
  end

  @doc ~S"""
  iex> AdventOfCode.example(2023, 15)
  ...> |> Year2023.Day15.p2
  145
  """
  def p2(lines) do
    lines
    |> Enum.into([])
    |> hd()
    |> String.split(",")
    |> Enum.reduce(%{}, &box/2)
    |> focusing_power()
  end

  @doc ~S"""
  iex> Year2023.Day15.hash(?H, 0)
  200
  iex> Year2023.Day15.hash(?A, 200)
  153
  iex> Year2023.Day15.hash("HASH")
  52
  """
  def hash(string) do
    String.to_charlist(string)
    |> Enum.reduce(0, &hash/2)
  end

  def hash(c, current_value) do
    current_value + c
    |> then(&(&1 * 17))
    |> rem(256)
  end

  def box(instruction, boxes) do
    [[_instruction, label, _op | opt_focal_length ]] = Regex.scan(~r/([a-z]+)(=|-)(\d+)?/, instruction)
    id = hash(label)

    case opt_focal_length do
      [focal_length] ->
        Map.get(boxes, id, [])
        |> alt_put_lense(label, focal_length |> String.to_integer())
        |> then(&Map.put(boxes, id, &1))

      [] ->
        Map.get(boxes, id, [])
        |> Enum.reject(fn {type, _} -> type == label end)
        |> then(&Map.put(boxes, id, &1))
    end
  end

  @doc """
  iex> Year2023.Day15.alt_put_lense([], "a", 1)
  [{"a", 1}]
  iex> Year2023.Day15.alt_put_lense([{"a", 1}], "a", 2)
  [{"a", 2}]
  iex> Year2023.Day15.alt_put_lense([{"a", 1}, {"b", 2}], "a", 2)
  [{"a", 2}, {"b", 2}]
  iex> Year2023.Day15.alt_put_lense([{"a", 1}, {"b", 2}], "b", 3)
  [{"a", 1}, {"b", 3}]
  iex> Year2023.Day15.alt_put_lense([{"a", 1}, {"b", 2}], "c", 3)
  [{"a", 1}, {"b", 2}, {"c", 3}]
  """
  def alt_put_lense(lenses, label, focal_length) do
    if label in (for {type, _} <- lenses, do: type) do
      for {type, fl} <- lenses, do: {type, (if type == label, do: focal_length, else: fl)}
    else
      lenses ++ [ {label, focal_length} ]
    end
  end

  @doc """
  iex> Year2023.Day15.put_lense([], "a", 1)
  [{"a", 1}]
  iex> Year2023.Day15.put_lense([{"a", 1}], "a", 2)
  [{"a", 2}]
  iex> Year2023.Day15.put_lense([{"a", 1}, {"b", 2}], "a", 2)
  [{"a", 2}, {"b", 2}]
  iex> Year2023.Day15.put_lense([{"a", 1}, {"b", 2}], "b", 3)
  [{"a", 1}, {"b", 3}]
  iex> Year2023.Day15.put_lense([{"a", 1}, {"b", 2}], "c", 3)
  [{"a", 1}, {"b", 2}, {"c", 3}]
  """
  def put_lense(lenses, label, focal_length) do
    {was_replaced, new_lenses} =
      Enum.reduce(lenses, {false, []}, fn {type, _focal_length} = current_lense, {was_replaced, new_lenses} ->
        cond do
          type == label -> {true, [{type, focal_length} | new_lenses]}
          true -> {was_replaced, [current_lense | new_lenses ]}
        end
      end)

    if was_replaced do
      Enum.reverse(new_lenses)
    else
      Enum.reverse([{label, focal_length} | new_lenses])
    end
  end

  def focusing_power({id, lenses}) do
    Enum.with_index(lenses, 1)
    |> Enum.map(fn {{_type, focal_length}, slot} -> focal_length * slot * (id + 1) end)
    |> Enum.sum()
  end

  def focusing_power(boxes) do
    Map.to_list(boxes)
    |> Enum.map(&focusing_power/1)
    |> Enum.sum()
  end
end
