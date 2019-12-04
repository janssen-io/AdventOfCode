namespace AdventOfCode.Event2019

module Utils =

    let rec digits (number:int) =
        match number with
        | n when n < 10 -> [n]
        | n -> n % 10 :: (digits (n / 10))

    let splitGroup x xs = (List.takeWhile ((=) x) xs, List.skipWhile ((=) x) xs)

    let group xs =
        let rec group' ys groups =
            match ys with
            | [] -> groups
            | (z::zs) ->
                let (g, rest) = splitGroup z ys
                group' rest (g :: groups)

        group' xs []


