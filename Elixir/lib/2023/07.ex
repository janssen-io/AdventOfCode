defmodule Year2023.Day07 do
  @doc ~S"""
  ## Examples

    iex> Year2023.Day07.p1(AdventOfCode.input(2023, 7, :example))
    6440
  """
  def p1(input) do
    run(input)
  end

  @doc ~S"""
  ## Examples

    iex> Year2023.Day07.p2(AdventOfCode.input(2023, 7, :example))
    5905
  """
  def p2(input) do
    input
    |> Stream.map(fn line -> String.replace(line, "J", "0") end)
    |> run
  end

  def run(input) do
    input
    |> Stream.map(&String.split/1)
    |> Stream.map(fn [hand, bid] -> %{
        cards: hand_value(hand),
        bid: String.to_integer(bid),
        type: type(hand)
      } end)
    |> Enum.sort(&ranksort/2)
    |> Enum.with_index(1)
    |> Enum.map(&winnings/1)
    |> Enum.sum
  end

  def card_value("A"), do: 14
  def card_value("K"), do: 13
  def card_value("Q"), do: 12
  def card_value("J"), do: 11
  def card_value("T"), do: 10
  def card_value(n), do: String.to_integer(n)

  def hand_value(hand) do
    String.codepoints(hand)
    |> Enum.map(&card_value/1)
  end

  @doc ~S"""
  ## Examples
    iex> Year2023.Day07.type("AKKKQ")
    4
    iex> Year2023.Day07.type("AKKKA")
    5
    iex> Year2023.Day07.type("AKKAK")
    5
    iex> Year2023.Day07.type("AKK12")
    2
    iex> Year2023.Day07.type("2KK12")
    3
    iex> Year2023.Day07.type("12345")
    1
  """
  def type(hand) do
    hand
    |> hand_value
    |> Enum.sort(:desc)
    |> sorted_type()
  end

  # p2: 0 = joker
  def sorted_type([b, b, b, b, 0]), do: 7
  def sorted_type([a, b, b, b, 0]) when a != b and a != 0 and b != 0, do: 6
  def sorted_type([a, a, a, b, 0]) when a != b and a != 0 and b != 0, do: 6
  def sorted_type([a, a, b, b, 0]) when a != b and a != 0 and b != 0, do: 5

  def sorted_type(hand = [_, _, _, _, 0]) do
    num_jokers = Enum.count(hand, fn c -> c == 0 end)
    count_uniq = Enum.uniq(hand) |> Enum.count(fn c -> c != 0 end)
    case {num_jokers, count_uniq} do
      {_, 0} -> 7 # j...
      {_, 1} -> 7 # a..., 0...
      {3, 2} -> 6 # a, b, 0, 0, 0 = high card(1) + 3 --> four of a kind(6)
      {2, 2} -> 6 # a, a, b, 0, 0 OR a, b, b, 0, 0 --> pair(2) + 2 = four of a kind(6)
      {2, 3} -> 4 # a, b, c, 0, 0 --> high card(1) + 2 = three of a kind(4)
      # should already be matched {1, 2} -> sorted_type(hand) # a, a, b, b, 0 OR # a, b, b, b, 0 OR # a, a, a, b, 0 --> 5 or 6
      {1, 3} -> 4 # exactly one pair = three of a kind
      {1, 4} -> 2 # high card + joker = one pair
    end
  end

  @doc ~S"""
  ## Examples

    iex> Year2023.Day07.sorted_type([0, 0, 0, 0, 0])
    7
    iex> Year2023.Day07.sorted_type([1, 1, 0, 0, 0])
    7
    iex> Year2023.Day07.sorted_type([1, 1, 1, 1, 1])
    7
    iex> Year2023.Day07.sorted_type([1, 1, 1, 1, 0])
    7
    iex> Year2023.Day07.sorted_type([5, 1, 1, 1, 1])
    6
    iex> Year2023.Day07.sorted_type([5, 5, 1, 1, 1])
    5
    iex> Year2023.Day07.sorted_type([5, 6, 1, 1, 1])
    4
    iex> Year2023.Day07.sorted_type([6, 1, 1, 1, 0])
    6
    iex> Year2023.Day07.sorted_type([5, 6, 6, 1, 1])
    3
    iex> Year2023.Day07.sorted_type([6, 6, 1, 1, 0])
    5
    iex> Year2023.Day07.sorted_type([5, 6, 7, 1, 1])
    2
    iex> Year2023.Day07.sorted_type([6, 7, 1, 1, 0])
    4
    iex> Year2023.Day07.sorted_type([5, 6, 7, 2, 1])
    1
    iex> Year2023.Day07.sorted_type([5, 6, 7, 2, 0])
    2
  """
  def sorted_type([a, a, a, a, a]), do: 7 # five
  def sorted_type([a, a, a, a, _]), do: 6 # four
  def sorted_type([_, a, a, a, a]), do: 6
  def sorted_type([b, b, a, a, a]), do: 5 # full house
  def sorted_type([a, a, a, b, b]), do: 5
  def sorted_type([a, a, a | _]), do: 4   # three
  def sorted_type([_, a, a, a, _]), do: 4
  def sorted_type([_, _, a, a, a]), do: 4
  def sorted_type(hand) do # two pair, one pair or high card
    count_uniq = Enum.uniq(hand) |> Enum.count
    case count_uniq do
      3 -> 3
      4 -> 2
      5 -> 1
    end
  end

  @doc ~S"""
  ## Examples
    iex> Year2023.Day07.ranksort(%{ cards: [], type: 1}, %{ cards: [], type: 2})
    true
    iex> Year2023.Day07.ranksort(%{ cards: [], type: 2}, %{ cards: [], type: 1})
    false
    iex> Year2023.Day07.ranksort(%{ cards: [1,2,3,4,5], type: 1}, %{ cards: [2,3,4,5,6], type: 1})
    true
    iex> Year2023.Day07.ranksort(%{ cards: [5,2,3,4,1], type: 1}, %{ cards: [2,3,4,5,6], type: 1})
    false
  """
  def ranksort(%{ cards: hand_left, type: type_left }, %{ cards: hand_right, type: type_right }) do
    { type_left, hand_left } < { type_right, hand_right }
  end

  def winnings({hand, rank}), do: hand.bid * rank
end
