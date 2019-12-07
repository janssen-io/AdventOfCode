namespace AdventOfCode.Event2019

module Utils =
    let curry2 f a b = f (a, b)
    let curry3 f a b c = f (a, b, c)
    let uncurry2 f (a,b) = f a b
    let uncurry3 f (a,b,c) = f a b c

    let readlines filename =
        System.IO.File.ReadLines filename
        |> List.ofSeq

    let rec digits (number:int) =
        List.ofSeq (string number)
        |> List.map (fun c -> int c - (int '0'))

    // e.g. splitGroup 1 [1,1,2,3,3,3,2,2] -> ([1,1],[2,3,3,3,2,2])
    // e.g. splitGroup 2 [1,1,2,3,3,3,2,2] -> ([], [1,1,2,3,3,3,2,2])
    let splitGroup x xs = (List.takeWhile ((=) x) xs, List.skipWhile ((=) x) xs)

    let group xs =
        let rec group' ys groups =
            match ys with
            | [] -> groups
            | (z::zs) ->
                let (g, rest) = splitGroup z ys
                group' rest (g :: groups)

        group' xs []

    let rec distribute e ys =
        match ys with
        | [] -> [[e]]
        | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permutations xs =
        match xs with
        | [] -> [[]]
        | (y::ys) -> List.collect (distribute y) (permutations ys)
    
    let cartesianProduct xs ys : (('a * 'b) list) =
        let rec cart xs' acc =
            match xs' with
            | [] -> acc
            | (x::xs') ->
                List.map (fun y -> (x, y)) ys
                |> List.append (cart xs' acc)
        cart xs []

    let doWhile f predicate seed =
        let rec doWhile' input acc =
            let result = f input
            if predicate input then
                doWhile' result (result :: acc)
            else
                acc
        doWhile' seed []

    let rec factorial n =
        match n with
        | m when n >= 1 -> m * (factorial (m - 1))
        | _ when n = 0 -> 1
        | _ -> failwith "Cannot calculate factorial of n < 1"
