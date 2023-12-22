defmodule Year2023.Day22 do
  # Supports
  #
  # AAABBB
  #   CC D
  # ======
  # C supports A, B
  # D supports B
  #
  # A supported by C
  # B supported by C, D
  #
  # Solution => Keep a list of per brick.
  #             Count the number unique supports that occur in singletons.
  #          => Total - unique singletons = p1 answer

  @doc ~S"""
      iex> AdventOfCode.example(2023, 22)
      ...> |> Year2023.Day22.p1
      5
  """
  def p1(lines) do
    bricks =
      lines
      |> Stream.map(&parse_brick/1)
      |> Enum.sort_by(fn {a, _} -> a.z end)

    fallen_bricks =
      bricks
      |> Enum.reduce([], fn {a, b}, fallen_bricks ->
        z = find_floor(fallen_bricks, {a, b})

        dz = a.z - (z + 1)

        fallen_bricks ++ [{%{a | z: a.z - dz}, %{b | z: b.z - dz}}]
      end)

    # |> Enum.sort_by(fn {a, _} -> a.z end)

    supporters =
      Enum.map(fallen_bricks, fn candidate -> list_supports(fallen_bricks, candidate) end)
      |> Enum.filter(&(Enum.count(&1) == 1))
      |> Enum.flat_map(& &1)
      |> MapSet.new()

    Enum.count(bricks) - MapSet.size(supporters)
  end

  @doc ~S"""
      iex> AdventOfCode.example(2023, 22)
      ...> |> Year2023.Day22.p2
      0
  """
  def p2(lines) do
    Enum.count(lines)
  end

  def xy(%{x: x, y: y}), do: {x, y}

  @doc """
      iex> c = {%{ x: 2, y: 0, z: 1}, %{x: 3, y: 0, z: 1}}
      ...> d = {%{ x: 5, y: 0, z: 1}, %{x: 5, y: 0, z: 1}}
      ...> a = {%{ x: 0, y: 0, z: 2}, %{x: 2, y: 0, z: 2}}
      ...> b = {%{ x: 3, y: 0, z: 2}, %{x: 5, y: 0, z: 2}}
      ...> bricks = [c,d,a,b]
      ...> ls = &Year2023.Day22.list_supports/3
      ...> Enum.map(bricks, fn brick -> ls.(bricks, brick, []) end)
      [[], [], [c], [d, c]]
  """
  def list_supports(bricks, supported, supports \\ [])
  def list_supports([], _, supports), do: supports

  def list_supports([supporter | bricks], supported, supports) when supporter == supported,
    do: list_supports(bricks, supported, supports)

  def list_supports([{_, b} | bricks], {p, q}, supports) when p.z - b.z <= 0,
    do: list_supports(bricks, {p, q}, supports)

  def list_supports([{_, b} | bricks], {p, q}, supports) when p.z - b.z > 1,
    do: list_supports(bricks, {p, q}, supports)

  def list_supports([brick | bricks], supported, supports) do
    if overlaps?(brick, supported) do
      list_supports(bricks, supported, [brick | supports])
    else
      list_supports(bricks, supported, supports)
    end
  end

  @doc """
  Find the lowest z-coordinate where the given brick is supported.
  The given list of bricks must be sorted by height (`z`) ascendingly.
  
  ## Examples
      iex> Year2023.Day22.find_floor([], {%{ x: 1, y: 2, z: 3}, %{ x: 1, y: 2, z: 3}})
      0
  
      iex> Year2023.Day22.find_floor(
      ...>    [{%{ x: 1, y: 2, z: 3}, %{ x: 1, y: 2, z: 3}}],
      ...>    {%{ x: 2, y: 3, z: 8}, %{ x: 2, y: 3, z: 8}})
      0
  
      iex> Year2023.Day22.find_floor(
      ...>    [{%{ x: 1, y: 2, z: 3}, %{ x: 1, y: 20, z: 3}}],
      ...>    {%{ x: 2, y: 3, z: 8}, %{ x: 1, y: 3, z: 8}})
      3
  
      iex> Year2023.Day22.find_floor(
      ...>    [{%{ x: 1, y: 5, z: 3}, %{ x: 1, y: 5, z: 3}}],
      ...>    {%{ x: 0, y: 3, z: 8}, %{ x: 2, y: 30, z: 8}})
      3
  """
  def find_floor(bricks, brick, lowest_support \\ 0)
  def find_floor([], _, support), do: support

  def find_floor([%{z: z1} | _], {%{z: z2}, _}, support) when z1 >= z2,
    do: support

  def find_floor([{a, b} | bricks], {p, q}, support) do
    if overlaps?({a, b}, {p, q}) do
      # {a, b} is always sorted by their `z` value
      # so `b.z` is the same as `max(a.z, b.z)`
      # keep recursing, to find closest support
      find_floor(bricks, {p, q}, b.z)
    else
      find_floor(bricks, {p, q}, support)
    end
  end

  @doc """
      iex> Year2023.Day22.overlaps?(
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 0}},
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 1}})
      true
  
      iex> Year2023.Day22.overlaps?(
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 0}},
      ...>  {%{ x: 3, y: 0 }, %{ x: 8, y: 1}})
      true
  
      iex> Year2023.Day22.overlaps?(
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 0}},
      ...>  {%{ x: 0, y: 0 }, %{ x: 3, y: 1}})
      true
  
      iex> Year2023.Day22.overlaps?(
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 0}},
      ...>  {%{ x: 2, y: 0 }, %{ x: 4, y: 1}})
      true
  
      iex> Year2023.Day22.overlaps?(
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 0}},
      ...>  {%{ x: 0, y: 0 }, %{ x: 6, y: 1}})
      true
  
      iex> Year2023.Day22.overlaps?(
      ...>  {%{ x: 1, y: 0 }, %{ x: 5, y: 0}},
      ...>  {%{ x: 6, y: 0 }, %{ x: 9, y: 1}})
      false
  """
  def overlaps?(b1, b2) do
    overlaps?(b1, b2, :x) && overlaps?(b1, b2, :y)
  end

  def overlaps?({a, b}, {p, q}, :x) when a.x > b.x, do: overlaps?({b, a}, {p, q}, :x)
  def overlaps?({a, b}, {p, q}, :x) when p.x > q.x, do: overlaps?({a, b}, {q, p}, :x)

  def overlaps?({a, b}, {p, q}, :x) do
    (a.x >= p.x && a.x <= q.x) ||
      (b.x >= p.x && b.x <= q.x) ||
      (p.x >= a.x && p.x <= b.x) ||
      (q.x >= a.x && q.x <= b.x)
  end

  def overlaps?({a, b}, {p, q}, :y) when a.y > b.y, do: overlaps?({b, a}, {p, q}, :y)
  def overlaps?({a, b}, {p, q}, :y) when p.y > q.y, do: overlaps?({a, b}, {q, p}, :y)

  def overlaps?({a, b}, {p, q}, :y) do
    (a.y >= p.y && a.y <= q.y) ||
      (b.y >= p.y && b.y <= q.y) ||
      (p.y >= a.y && p.y <= b.y) ||
      (q.y >= a.y && q.y <= b.y)
  end

  def parse_brick(line) do
    Elf.get_ints(line)
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(3)
    |> Enum.map(fn [x, y, z] -> %{x: x, y: y, z: z} end)
    |> Enum.sort_by(fn brick -> brick.z end, :asc)
    |> then(fn [a, b] -> {a, b} end)
  end

  def b([x, y, z]), do: %{x: x, y: y, z: z}
end
