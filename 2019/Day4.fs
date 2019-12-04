namespace AdventOfCode.Event2019

module Day4 =

    open Xunit
    open Swensen.Unquote

    let hasAtleastOneGroupOf2 num =
        let rec groups num lastDigit acc =
            if num = 0 then
                acc
            else
                let digit = num % 10
                let rem = num / 10
                if digit = lastDigit then
                    match acc with
                    | []  -> failwith "Unexpected empty accumulator"
                    | (group::gs) ->
                        groups rem digit ((digit :: group) :: gs)
                else
                    groups rem digit ([digit] :: acc)
        
        groups num 10 []
        |> List.fold (fun isValid group -> isValid || group.Length = 2) false

    let rec hasDoubleDigit num =
        let rec isValid lastNum remaining acc =
            if remaining = 0 then
                acc
            else
                let digit = remaining % 10
                let rem = remaining / 10
                isValid digit rem (acc || digit = lastNum)
            
        isValid 10 num false

    let hasNoDecreasingDigit num =
        let rec isValid lastNum remaining acc =
            if remaining = 0 then
                acc
            else
                let digit = remaining % 10
                let rem = remaining / 10
                isValid digit rem (acc && digit <= lastNum)
            
        isValid 10 num true

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

