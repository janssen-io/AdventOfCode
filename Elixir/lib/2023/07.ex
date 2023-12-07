defmodule Year2023.Day07 do
  @doc ~S"""
  ## Examples

    iex> Year2023.Day07.p1(AdventOfCode.input(2023, 7, :example))
    6440
  """
  def p1(input) do
    input
    |> Stream.map(&String.split/1)
    |> Stream.map(fn [hand, bid] -> %{
        cards: hand_value(hand),
        bid: String.to_integer(bid),
        type: type(hand)
      } end)
    |> Enum.sort(&ranksort/2)
    |> Enum.with_index
    |> Enum.map(&winnings/1)
    |> Enum.sum
  end

  @doc ~S"""
  ## Examples

    iex> Year2023.Day07.p2(AdventOfCode.input(2023, 7, :example))
    0
  """
  def p2(input) do
    input
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
    |> Enum.sort
    |> sorted_type
  end

  @doc ~S"""
  ## Examples

    iex> Year2023.Day07.sorted_type(["A", "A", "A", "A", "A"])
    7
    iex> Year2023.Day07.sorted_type(["X", "A", "A", "A", "A"])
    6
    iex> Year2023.Day07.sorted_type(["X", "X", "A", "A", "A"])
    5
    iex> Year2023.Day07.sorted_type(["X", "Y", "A", "A", "A"])
    4
    iex> Year2023.Day07.sorted_type(["X", "Y", "Y", "A", "A"])
    3
    iex> Year2023.Day07.sorted_type(["X", "Y", "Z", "A", "A"])
    2
    iex> Year2023.Day07.sorted_type(["X", "Y", "Z", "B", "A"])
    1
  """
  def sorted_type([a, a, a, a, a]), do: 7 # five
  def sorted_type([a, a, a, a, _]), do: 6 # four
  def sorted_type([_, a, a, a, a]), do: 6
  def sorted_type([b, b, a, a, a]), do: 5 # fullh house
  def sorted_type([a, a, a, b, b]), do: 5
  def sorted_type([a, a, a | _]), do: 4   # three
  def sorted_type([_, a, a, a, _]), do: 4
  def sorted_type([_, _, a, a, a]), do: 4
  def sorted_type(hand) do # two pair, one pair or high card
    count_uniq = Enum.uniq(hand) |> Enum.count
    case count_uniq do
      5 -> 1
      4 -> 2
      3 -> 3
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
    if (type_left == type_right) do
      handsort(hand_left, hand_right)
    else
      type_left < type_right
    end
  end

  @doc ~S"""
  ## Examples
    iex> Year2023.Day07.handsort([1, 2, 3, 4, 5], [1, 2, 3, 4, 6])
    true
    iex> Year2023.Day07.handsort([1, 2, 3, 4, 9], [1, 2, 3, 5, 6])
    true
    iex> Year2023.Day07.handsort([1, 2, 3, 9, 9], [1, 2, 4, 5, 6])
    true
    iex> Year2023.Day07.handsort([1, 2, 9, 9, 9], [1, 3, 4, 5, 6])
    true
    iex> Year2023.Day07.handsort([1, 9, 9, 9, 9], [2, 3, 4, 5, 6])
    true
    iex> Year2023.Day07.handsort([9, 0, 0, 0, 0], [2, 3, 4, 5, 6])
    false
  """
  def handsort([a,b,c,d,e], [a,b,c,d,z]), do: e < z
  def handsort([a,b,c,d,_], [a,b,c,y,_]), do: d < y
  def handsort([a,b,c,_,_], [a,b,x | _]), do: c < x
  def handsort([a,b,_,_,_], [a,w   | _]), do: b < w
  def handsort([a,_,_,_,_], [v     | _]), do: a < v

  def winnings({hand, index}), do: hand.bid * (index + 1)
end
