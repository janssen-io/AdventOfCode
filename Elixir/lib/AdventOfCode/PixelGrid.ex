defmodule AdventOfCode.PixelGrid do
  def new(name) do
    Agent.start_link(fn -> %{} end, name: name)
  end

  def clear(name), do: Agent.update(name, fn _ -> %{} end)

  def set(name, {x, y}, rgb = [_,_,_]) do
    color = ansi_rgb(rgb)
    Agent.update(name, &Map.put(&1, {x, y}, color))
  end

  def set(name, {x, y}, %{rgb: rgb}) do
    set(name, {x, y}, rgb)
  end

  def set(name, {x, y}, %{heatmap: depth, max: max }) do
    set(name, {x, y}, %{hsl: heatmap_hsl(depth, max)})
  end

  def set(name, {x, y}, %{heatmap: depth }) do
    set(name, {x, y}, %{hsl: heatmap_hsl(depth, 125)})
  end

  def set(name, {x, y}, %{hsl: hsl}) do
    set(name, {x, y}, %{rgb: hsl_to_rgb(hsl)})
  end

  def ansi_rgb([r, g, b]), do: [div(r, 51), div(g, 51), div(b, 51)]

  def heatmap_hsl(depth, max_depth) do
    [(1.0 - rem(depth, max_depth) / max_depth) * 240 / 360, 1, 0.5]
  end

  def print(name, character_map \\ %{}) do
    map = Agent.get(name, & &1)
    coords = MapSet.new(Map.keys(map))
    {max_x, _} = Enum.max_by(coords, fn {x, _} -> x end)
    {_, max_y} = Enum.max_by(coords, fn {_, y} -> y end)

    IO.inspect({max_x, max_y}, label: :drawing)

    for y <- 0..max_y do
      for x <- 0..max_x do
        [r,g,b] = Map.get(map, {x, y}, [5,5,5])
        IO.write(IO.ANSI.color_background(r, g, b))
        IO.write(Map.get(character_map, {x, y}, " "))
        IO.write(IO.ANSI.reset())
      end

      IO.write("\n")
    end
    :ok
  end

  # hsl in [0,1] to rgb in [0,255]
  def hsl_to_rgb([h, s, l]) do
    [r, g, b] =
      if s == 0 do
        # achromatic
        [l, l, l]
      else
        q = if l < 0.5, do: l * (1 + s), else: l + s - l * s
        p = 2 * l - q
        r = hue_to_rgb(p, q, h + 1 / 3)
        g = hue_to_rgb(p, q, h)
        b = hue_to_rgb(p, q, h - 1 / 3)
        [r, g, b]
      end

    [r,g,b] |> Enum.map(&to_255/1)
  end

  def to_255(num), do: Enum.min([255, Float.round(256.0 * num) |> trunc])

  defp hue_to_rgb(p, q, t) do
    t = if t < 0, do: t + 1, else: t
    t = if t > 1, do: t - 1, else: t

    cond do
      t < 1 / 6 -> p + (q - p) * 6 * t
      t < 1 / 2 -> q
      t < 2 / 3 -> p + (q - p) * (2 / 3 - t) * 6
      true -> p
    end
  end
end
