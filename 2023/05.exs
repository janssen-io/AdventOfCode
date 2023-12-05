Code.require_file("../aoc.exs")

defmodule DayFive do
  import Aoc, only: [get_digits: 1, is_digit: 1]

  def solve(lines) do
    # p1(lines) |> IO.inspect(label: 'p1', charlists: :as_lists)
    p2(lines) |> IO.inspect(label: 'p2', charlists: :as_lists)
  end

  def p1(lines) do
    parts = Stream.chunk_by(lines, fn x -> x == "\n" end)
    seeds = Enum.take(parts, 1) 
    |> hd |> hd 
    |> get_digits
    |> Enum.map(&String.to_integer/1)
    |> Enum.map(fn n -> [n, n] end)

    maps = parts
    |> Stream.drop(1)
    |> Stream.reject(fn lines -> lines == ["\n"] end)
    |> Enum.map(&parse_map/1)

    act(seeds, maps)

    # seeds
    # |> Stream.map(fn s -> {s, walk_map(maps, s, 0)} end)
    # |> Enum.min_by(fn {_s, d} -> d end)

  end

  def p2(lines) do
    parts = Stream.chunk_by(lines, fn x -> x == "\n" end)
    seeds = Enum.take(parts, 1) 
    |> hd |> hd 
    |> get_digits
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(2)
    |> Enum.map(fn [a, b] -> [a, a+b-1] end)

    maps = parts
    |> Stream.drop(1)
    |> Stream.reject(fn lines -> lines == ["\n"] end)
    |> Enum.map(&parse_map/1)

    act(seeds, maps)
  end

  def act(seeds, maps) do
    seed = seeds
    |> Enum.take(1)
    |> Enum.map(fn s -> {s, walk_map2(maps, {s, []}, 0)} end)
    |> Enum.reduce([], fn {s, d}, acc -> [{s, Enum.min(d)} | acc] end)
    |> Enum.sort_by(fn {_, loc} -> loc end)
  end

  def parse_map([_ | ranges]) do
    Enum.map(ranges, &get_digits/1)
    |> Enum.map(&Enum.map(&1, fn n -> String.to_integer n end))
  end

#p2 below here
  def walk_map2([], {[a,_b], _crumbs}, depth) do
    IO.inspect({depth, [a, _b], crumbs: _crumbs}, charlists: :as_lists)
    [a] 
  end
  def walk_map2([map | maps], {seed_range, crumbs}, depth) do
    if (depth == 0) do
      IO.inspect("---")
      IO.inspect(seed_range)
    end
    ds = map_seed2(map, seed_range)
    IO.inspect({depth, seed_range, ds}, label: "mapped", charlists: :as_lists)
    Enum.flat_map(ds, fn d -> walk_map2(maps, {d, [seed_range | crumbs]}, depth + 1) end)
  end

  def map_seed2(ranges, seeds) do
    [seed_lower, seed_upper] = seeds
    mappings = ranges
    |> Enum.map(fn r -> destination2(seeds, r) end)
    |> Enum.reject(&is_nil/1)

    # if nothing got mapped, just map from source to destination plainly
    mappings =
      if (Enum.empty? mappings) do
        [{seeds, seeds}]
      else
        mappings
      end

    # find unmapped ranges
    mappings = fill_gaps(mappings, seeds)

    #! sanity check
    {[lowerbound, _], _} = Enum.min_by(
      mappings, fn {[sl, _], _} -> sl end)

    #s/min/max : copy paste mistake!
    {[_, upperbound], _} = Enum.max_by(
      mappings, fn {[_, su], _} -> su end)

    if (lowerbound != seed_lower && upperbound != seed_upper) do
      raise "#{lowerbound} != #{seed_lower} && #{upperbound} != #{seed_upper}"
    end

    rb = seed_upper - seed_lower + 1
    ra = mappings
    |> Enum.map(fn {[a, b], _} -> b-a+1 end)
    |> Enum.sum
    if (rb != ra) do
      IO.inspect({seed_upper, seed_lower})
      IO.inspect(mappings)
      raise "#{rb} != #{ra}"
    end

    # only return destinations
    mappings
    |> Enum.map(fn {_source, destination} -> destination end)
  end

  def fill_gaps(mappings, [seed_lower, seed_upper]) do
    {[lowerbound, _], _} = Enum.min_by(
      mappings, &source_start/1)

    #s/min/max : copy paste mistake!
    {[_, upperbound], _} = Enum.max_by(
      mappings, &source_end/1)

    mappings = 
      if lowerbound > seed_lower do
        range = [seed_lower, lowerbound - 1]
        [{range, range} | mappings]
      else
        mappings
      end

    mappings = 
      if upperbound < seed_upper do
        range = [upperbound + 1, seed_upper]
        [{range, range} | mappings]
      else
        mappings
      end

    {mappings, _} = mappings
      |> Enum.sort_by(&source_start/1)
      |> Enum.reduce(
          {[], seed_lower},
          fn map, {ms, up} ->
            {[ss, se], _} = map
            if (ss > up + 1) do
              range = [up + 1, ss - 1]
              {[ {range, range}, map | ms], se }
            else
              {[ map | ms], se }
            end
          end)
      mappings
  end

  def source_start({[x, _end], _}), do: x
  def source_end({[_start, x], _}), do: x

  # {source seed range, destination seed range}
  def destination2([lower, upper], [d, s, size]) do
    cond do
      # seed range entirely before source
      upper < s -> nil

      # seed range entirely after source
      lower >= s + size -> nil

      # seed range entirely inside source
      lower >= s and upper < s + size ->
        {[lower, upper], [d + (lower - s), d + (upper - s)]}

      # seed range entirely around source
      lower < s and upper >= s + size ->
        {[s, s + size - 1], [d, d + size - 1] }

      # seed range overlapping with start
      lower < s and upper < s + size ->
        {[s, upper], [d, d + (upper - s)] }

      # seed range overlapping with end
      lower >= s and upper >= s + size ->
        {[lower, s + size - 1], [d + (lower - s), d + (upper - s)]}
    end
  end

#p1 below here


  def walk_map([], seed, _), do: seed
  def walk_map([map | maps], seed, n) do
    d = map_seed(map, seed)
    walk_map(maps, d, n + 1)
  end

  def map_seed([], seed), do: seed
  def map_seed([range | ranges], seed) do
    d = destination(seed, range)
    if d != nil do d else map_seed(ranges, seed) end
  end

  def destination(seed, [d, s, size]) do
    if (s <= seed and seed < s + size) do
      d + (seed - s)
    else
      nil
    end
  end
end

# Aoc.readAndSolve("02.input", &DayTwo.solve/1, ["\r\n", "\n"], true)
File.stream!("05.example.input", [], :line) |> DayFive.solve
# File.stream!("05.input", [], :line) |> DayFive.solve

