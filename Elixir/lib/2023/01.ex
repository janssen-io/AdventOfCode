defmodule Year2023.Day01 do
    def p1(lines) do
      lines
      |> Stream.map(&Elf.get_ints/1)
      |> Stream.map(&Enum.join/1)
      |> solve
    end

    def p2(lines) do
      solve(lines)
    end

    def solve(lines) do
        lines
        |> Stream.map(&String.codepoints/1)
        |> Stream.map(fn l -> find(l, {nil, nil}) end)
        |> Stream.map(fn [l ,r] -> l <> r end)
        |> Stream.map(&Integer.parse/1)
        |> Stream.map(&elem(&1, 0))
        |> Enum.sum
    end

    # p1: digit cases
    ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    |> Enum.each(fn int ->
      def find([unquote(int) | rest], {nil, _}) do
        find(rest, { unquote(int), nil })
      end

      def find([unquote(int) | rest], {f, _}) do
        find(rest, {f, unquote(int) })
      end
    end)

    # p2: word cases
    [["1", "one"],
     ["2", "two"],
     ["3", "three"],
     ["4", "four"],
     ["5", "five"],
     ["6", "six"],
     ["7", "seven"],
     ["8", "eight"],
     ["9", "nine"]]
    |> Enum.each(fn [int, name] ->
      parts = String.codepoints(name)
      last_parts = Enum.slice(parts, 1, Enum.count parts)
      def find([unquote_splicing(parts) | rest], {nil, _}) do
        find([unquote_splicing(last_parts) | rest], { unquote(int), nil })
      end

      def find([unquote_splicing(parts) | rest], {f, _}) do
        find([unquote_splicing(last_parts) | rest], { f, unquote(int) })
      end
    end)

    # p1/p2: no match cases
    def find([_ | rest], {f, l}) do
      find(rest, { f, l })
    end

    def find([], {f, nil}) do
      [f, f]
    end

    def find([], {f, l}) do
      [f, l]
    end
end
