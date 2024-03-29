defmodule Year2023.Day05 do
  def p1(lines) do
    parts = Stream.chunk_by(lines, fn x -> x == "\n" end)

    seeds =
      Enum.take(parts, 1)
      |> hd
      |> hd
      |> Elf.get_ints
      |> Enum.map(&String.to_integer/1)
      |> Enum.map(fn n -> [n, n] end)

    maps =
      parts
      |> Stream.drop(1)
      |> Stream.reject(fn lines -> lines == ["\n"] end)
      |> Enum.map(&parse_map/1)

    act(seeds, maps) |> hd |> Elf.snd
  end

  def p2(lines) do
    parts = Stream.chunk_by(lines, fn x -> x == "\n" end)

    seeds =
      Enum.take(parts, 1)
      |> hd
      |> hd
      |> Elf.get_ints
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(2)
      |> Enum.map(fn [a, b] -> [a, a + b - 1] end)

    maps =
      parts
      |> Stream.drop(1)
      |> Stream.reject(fn lines -> lines == ["\n"] end)
      |> Enum.map(&parse_map/1)

    act(seeds, maps) |> hd |> Elf.snd
  end

  def test do
    act([[0, 10]], [[[100, 0, 11]]]) ++
      act([[0, 10]], [[[200, -5, 11]]]) ++
      act([[0, 10]], [[[300, 5, 11]]])
  end

  def act(seeds, maps) do
    seeds
    |> Enum.map(fn s -> {s, walk_map2(maps, {s, []}, 0)} end)
    |> Enum.map(fn {s, d} -> {s, Enum.min(d)} end)
    |> Enum.sort_by(fn {_, loc} -> loc end)
  end

  def parse_map([_ | ranges]) do
    Enum.map(ranges, &Elf.get_ints/1)
    |> Enum.map(&Enum.map(&1, fn n -> String.to_integer(n) end))
  end

  # p2 below here
  def walk_map2([], {[a, _b], _crumbs}, _depth), do: [a]

  def walk_map2([map | maps], {seed_range, crumbs}, depth) do
    ds = map_seed2(map, seed_range)
    Enum.flat_map(ds, fn d -> walk_map2(maps, {d, [seed_range | crumbs]}, depth + 1) end)
  end

  def map_seed2(ranges, seeds) do
    mappings =
      ranges
      |> Enum.map(fn r -> destination2(seeds, r) end)
      |> Enum.reject(&is_nil/1)

    # if nothing got mapped, just map from source to destination plainly
    mappings =
      if Enum.empty?(mappings) do
        [{seeds, seeds}]
      else
        mappings
      end

    # find unmapped ranges
    mappings = fill_gaps(mappings, seeds)
    ensure_source_range_mapped!(mappings, seeds) # sanity check

    # only return destinations
    mappings
    |> Enum.map(fn {_source, destination} -> destination end)
  end

  def ensure_source_range_mapped!(mappings, [seed_lower, seed_upper]) do
    {[lowerbound, _], _} = Enum.min_by(mappings, fn {[sl, _], _} -> sl end)

    # s/min/max : copy paste mistake!
    {[_, upperbound], _} = Enum.max_by(mappings, fn {[_, su], _} -> su end
      )

    if lowerbound != seed_lower && upperbound != seed_upper do
      raise "#{lowerbound} != #{seed_lower} && #{upperbound} != #{seed_upper}"
    end

    rb = seed_upper - seed_lower + 1
    ra =
      mappings
      |> Enum.map(fn {[a, b], _} -> b - a + 1 end)
      |> Enum.sum()

    if rb != ra do
      IO.inspect({seed_lower, seed_upper}, label: :seed)
      IO.inspect(mappings |> Enum.sort_by(fn {s, _} -> Elf.fst(s) end), label: :mappings)
      raise "#{rb} != #{ra}"
    end
  end

  def fill_gaps(mappings, [seed_lower, seed_upper]) do
    {[lowerbound, _], _} = Enum.min_by(mappings, &source_start/1)

    # s/min/max : copy paste mistake!
    {[_, upperbound], _} = Enum.max_by(mappings, &source_end/1)

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

    {mappings, _} =
      mappings
      |> Enum.sort_by(fn {source, _dest} -> Elf.fst(source) end)
      |> Enum.reduce(
        {[], seed_lower},
        fn map, {newMappings, last_upper} ->
          {[source_lower, source_upper], _} = map

          if source_lower > last_upper + 1 do
            range = [last_upper + 1, source_lower - 1]
            {[{range, range}, map | newMappings], source_upper}
          else
            {[map | newMappings], source_upper}
          end
        end
      )

    mappings
  end

  def source_start({[x, _end], _}), do: x
  def source_end({[_start, x], _}), do: x

  def destination2([lower, upper], [destination_lowerbound, source_lowerbound, size]) do
    source_upperbound = source_lowerbound + size - 1
    destination_upperbound = destination_lowerbound + size - 1
    mapped_lowerbound = destination_lowerbound - source_lowerbound + lower
    mapped_upperbound = destination_lowerbound - source_lowerbound + upper

    cond do
      # seed range entirely before source
      upper < source_lowerbound ->
        nil

      # seed range entirely after source
      lower > source_upperbound ->
        nil

      # seed range entirely inside source
      lower >= source_lowerbound and upper <= source_upperbound ->
        {[lower, upper], [mapped_lowerbound, mapped_upperbound]}

      # seed range entirely around source
      lower < source_lowerbound and upper > source_upperbound ->
        {[source_lowerbound, source_upperbound], [destination_lowerbound, destination_upperbound]}

      # seed range overlapping with start
      lower < source_lowerbound and upper <= source_upperbound ->
        {[source_lowerbound, upper], [destination_lowerbound, mapped_upperbound]}

      # seed range overlapping with end
      lower >= source_lowerbound and upper > source_upperbound ->
        {[lower, source_upperbound], [mapped_lowerbound, destination_upperbound]}
    end
  end

  # p1 below here
  def walk_map([], seed, _), do: seed

  def walk_map([map | maps], seed, n) do
    d = map_seed(map, seed)
    walk_map(maps, d, n + 1)
  end

  def map_seed([], seed), do: seed

  def map_seed([range | ranges], seed) do
    d = destination(seed, range)

    if d != nil do
      d
    else
      map_seed(ranges, seed)
    end
  end

  def destination(seed, [d, s, size]) do
    if s <= seed and seed < s + size do
      d + (seed - s)
    else
      nil
    end
  end
end
