defmodule Year2023.Day04 do
  def p1(lines) do
    lines
    |> Stream.map(&calculate/1)
    |> Stream.map(&points/1)
    |> Enum.sum()
  end

  def p2(lines) do
    lines
    |> Enum.map(&calculate/1)
    |> Enum.reduce({ 0, [] }, &win_cards/2)
    |> then(fn { answer, _} -> answer end)
  end

  def calculate(line) do
    [left, right] = String.split(line, "|")
    winning = Elf.get_digits(left) |> tl |> MapSet.new
    hand = Elf.get_digits(right) |> MapSet.new
    MapSet.intersection(winning, hand)
    |> MapSet.size
  end

  def points(size) do
    case size do
      0 -> 0
      n -> 2 ** (n - 1)
    end
  end

  # base cases
  def win_cards(0, { acc, [] }) do { acc + 1, [] } end
  def win_cards(n, { acc, [] }) do { acc + 1, [{n, 1}] } end

  # tracker: [{n, w}] => this and next n - 1 cards, we win w copies
  def win_cards(0, { total, tracker }) do
    num_copies = Enum.reduce(tracker, 1, &sum_w/2) # start sum with 1 to include original card
    decremented_tracker = Enum.map(tracker, &dec/1) |> Enum.reject(&zero?/1)
    { total + num_copies, decremented_tracker }
  end

  def win_cards(n, { total, tracker }) do
    num_copies = Enum.reduce(tracker, 1, &sum_w/2)
    decremented_tracker = Enum.map(tracker, &dec/1) |> Enum.reject(&zero?/1)
    { total + num_copies, [{n, num_copies} | decremented_tracker] }
  end

  def dec({n, w}) do {n - 1, w} end
  def zero?({n, _w}) do n == 0 end
  def sum_w({_, w}, acc) do acc + w end

end
