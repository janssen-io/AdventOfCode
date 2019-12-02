﻿namespace AdventOfCode2019

module Day2 = 

    open Xunit
    open FsCheck
    open FsCheck.Xunit

    type Memory = int array

    type Operation =
        | Sum
        | Product
        | NoOp

    type Instruction = Operation * int list

    type State = {
        Memory : Memory
        InstructionPointer : int
    }

    let parseInstruction instruction =
        match instruction with
        | 1     -> Sum
        | 2     -> Product
        | 99    -> NoOp
        | _     -> failwithf "Unknown instruction %i" instruction

    let arity instruction =
        match instruction with
        | Sum | Product -> 2
        | NoOp  -> 0

    let getParameters instruction (memory : Memory) ip =
        seq { for p in [1..arity instruction] do yield memory.[ip + p] }
        |> Seq.map (fun i -> memory.[i])
        |> List.ofSeq

    let perform (instruction, parameters) =
        parameters
        |> match instruction with
            | Sum     -> List.fold (+) 0 
            | Product -> List.fold (*) 1
            | NoOp    -> failwithf "Cannot perform %A" instruction

    let run'' (input:int array) noun verb =
        let memory' = Array.copy input
        memory'.[1] <- noun
        memory'.[2] <- verb
        let rec computer state =
            let instruction = parseInstruction state.Memory.[state.InstructionPointer]
            match instruction with
            | NoOp -> state
            | _    ->
                let ip = state.InstructionPointer 
                let parameters = getParameters instruction state.Memory ip
                let position = state.Memory.[ip + arity instruction + 1]
                state.Memory.[position] <- perform (instruction, parameters)
                { state with
                    Memory = state.Memory 
                    InstructionPointer = state.InstructionPointer + arity instruction + 2 }
                |> computer
        computer { Memory = memory'; InstructionPointer = 0 }
        |> (fun s -> s.Memory)

    let input = [|1;12;2;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;10;19;1;19;6;23;2;23;13;27;1;27;5;31;2;31;10;35;1;9;35;39;1;39;9;43;2;9;43;47;1;5;47;51;2;13;51;55;1;55;9;59;2;6;59;63;1;63;5;67;1;10;67;71;1;71;10;75;2;75;13;79;2;79;13;83;1;5;83;87;1;87;6;91;2;91;13;95;1;5;95;99;1;99;2;103;1;103;6;0;99;2;14;0;0|]

    let runPartTwo (mem:Memory) =
        seq {
            for verb in [0..99] do 
                for noun in [0..99] do
                    yield (verb,noun)
            }
        |> Seq.fold (fun solution (noun, verb) -> 
            if (run'' mem noun verb).[0] = 19690720 then 
                (100 * noun + verb) 
            else 
                solution) 0 

    [<Property>]
    let ``Part one: Add`` (a:PositiveInt) (b:PositiveInt) (p:PositiveInt) =
        let x = a.Get % 5
        let y = b.Get % 5
        let input = [|1; x; y; p.Get % 4; 99|]
        let output = run'' input x y
        let expected = input.[x] + input.[y]
        let actual = output.[p.Get % 4]
        expected = actual

    [<Property>]
    let ``Part one: Multiply`` (a:PositiveInt) (b:PositiveInt) (p:PositiveInt) =
        let x = a.Get % 5
        let y = b.Get % 5
        let input = [|2; x; y; p.Get % 3; 99|]
        let expected = input.[x] * input.[y]
        let actual = (run'' input x y).[p.Get % 3]
        expected = actual

    [<Fact>]
    let ``Part one: multiple operations`` () =
        let output = run'' [|1;9;10;3;2;3;11;0;99;30;40;50|] 9 10
        Assert.True([|3500;9;10;70;2;3;11;0;99;30;40;50|] = output)

    [<Fact>]
    let ``Part one: answer`` () =
        Assert.Equal(3790645, (run'' input 12 2).[0])

    [<Fact>]
    let ``Part two: answer`` () =
        Assert.Equal(6577, (runPartTwo input))
