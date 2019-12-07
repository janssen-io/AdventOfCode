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

    let splitGroup x xs = (List.takeWhile ((=) x) xs, List.skipWhile ((=) x) xs)

    let group xs =
        let rec group' ys groups =
            match ys with
            | [] -> groups
            | (z::zs) ->
                let (g, rest) = splitGroup z ys
                group' rest (g :: groups)

        group' xs []

    
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

