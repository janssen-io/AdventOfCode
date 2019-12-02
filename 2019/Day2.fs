namespace AdventOfCode2019

open System

module Day2 = 

    open Xunit
    open FsCheck
    open FsCheck.Xunit

    let maybeDo f a b =
        match f with
        | Some op -> Some <| op a b
        | None    -> None

    let getOp i =
        match i with
        |  1 -> Some (+)
        |  2 -> Some ((*) : int -> int -> int)
        | 99 -> None
        |  x -> failwithf "Unexpected operation: %i" x

    let run (s:int array) =
        let rec run' (stack:int array) pc =
            let op = getOp stack.[pc]
            match op with
            | None -> stack
            | Some f ->
                let  a = stack.[pc + 1]
                let  b = stack.[pc + 2]
                let  p = stack.[pc + 3]
                stack.[p] <- f stack.[a] stack.[b]
                run' stack (pc + 4)
        run' s 0

    let input = [|1;12;2;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;10;19;1;19;6;23;2;23;13;27;1;27;5;31;2;31;10;35;1;9;35;39;1;39;9;43;2;9;43;47;1;5;47;51;2;13;51;55;1;55;9;59;2;6;59;63;1;63;5;67;1;10;67;71;1;71;10;75;2;75;13;79;2;79;13;83;1;5;83;87;1;87;6;91;2;91;13;95;1;5;95;99;1;99;2;103;1;103;6;0;99;2;14;0;0|]

    [<Property>]
    let ``Part one: Add`` (a:PositiveInt) (b:PositiveInt) (p:PositiveInt) =
        let x = a.Get % 5
        let y = b.Get % 5
        let input = [|1; x; y; p.Get % 3; 99|]
        let expected = input.[x] + input.[y]
        let actual = (run input).[p.Get % 3]
        expected = actual

    [<Property>]
    let ``Part one: Multiply`` (a:PositiveInt) (b:PositiveInt) (p:PositiveInt) =
        let x = a.Get % 5
        let y = b.Get % 5
        let input = [|2; x; y; p.Get % 3; 99|]
        let expected = input.[x] * input.[y]
        let actual = (run input).[p.Get % 3]
        expected = actual

    [<Fact>]
    let ``Part one: example`` () =
        let output = run [|1;9;10;3;2;3;11;0;99;30;40;50|]
        Assert.True([|3500;9;10;70;2;3;11;0;99;30;40;50|] = output)

    [<Fact>]
    let ``Part one: answer`` () =
        Assert.Equal(3790645, (run input).[0])
