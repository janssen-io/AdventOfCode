namespace AdventOfCode.Event2019

module Day4 =

    open Xunit
    open Swensen.Unquote
    open Utils

    let hasAtleastOneGroupOf2 num =
        digits num
        |> group
        |> List.fold (fun isValid group -> isValid || group.Length = 2) false

    let rec hasDoubleDigit num =
        digits num
        |> group
        |> List.exists (fun g -> List.length g >= 2)

    let hasNoDecreasingDigit num =
        digits num
        |> fun ds -> List.sort ds = ds

    let isPassword num =
        (hasDoubleDigit num && hasNoDecreasingDigit num)

    let isPassword2 num =
        isPassword num && hasAtleastOneGroupOf2 num

    let run filter lower upper =
        [lower .. upper]
        |> List.filter filter
        |> List.length

    let runPartOne = run isPassword
    let runPartTwo = run isPassword2

    [<Fact>]
    let ``Part one: basic correct password`` () =
        test <@ isPassword 111111 @>

    [<Fact>]
    let ``Part one: incorrect password (decreasing)`` () =
        test <@ (not << isPassword) 223450 @>
        
    [<Fact>]
    let ``Part one: incorrect password (no double)`` () =
        test <@ (not << isPassword) 123789 @>

    [<Fact>]
    let ``Part one: answer`` () =
        test <@ runPartOne 158126 624574 = 1665 @>

    [<Fact>]
    let ``Part two: incorrect password (uneven group)`` () =
        test <@ (not << isPassword2) 123444 @>

    [<Fact>]
    let ``Part two: correct password (bigger even group)`` () =
        test <@ isPassword2 111122 @>

    [<Fact>]
    let ``Part two: basic incorrect password (no group of 2)`` () =
        test <@ (not << isPassword2) 111111 @>

    [<Fact>]
    let ``Part two: incorrect password (decreasing)`` () =
        test <@ (not << isPassword2) 223450 @>
        
    [<Fact>]
    let ``Part two: incorrect password (no double)`` () =
        test <@ (not << isPassword2) 123789 @>

    [<Fact>]
    let ``Part two: answer should be smaller than part one`` () =
        test <@ runPartTwo 158126 624574 < 1665 @>

    [<Fact>]
    let ``Part two: answer`` () =
        test <@ runPartTwo 158126 624574 <> 1375 @>
        test <@ runPartTwo 158126 624574 = 1131 @>

