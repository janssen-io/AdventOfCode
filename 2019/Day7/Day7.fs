namespace AdventOfCode.Event2019

open System.Diagnostics


module Day7 =

    open Utils
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Swensen.Unquote

    type Memory = int array

    type Operation =
        | Add
        | Multiply
        | Input
        | Output
        | JumpNotZero
        | JumpZero
        | LessThan
        | Equals
        | Halt

    type Parameter =
        | Position of int
        | Immediate of int

    type ParameterConstructor = int -> Parameter

    type Instruction = Operation * Parameter list

    type State = {
        Memory : Memory
        InstructionPointer : int
        HasHalted : bool
        Input : int list
        Output : int list
    }

    let startState memory =
        { Memory = memory 
          InstructionPointer = 0
          HasHalted = false 
          Input = []
          Output = []
        }

    let arity instruction =
        match instruction with
        | Add | Multiply 
        | LessThan | Equals
        | JumpNotZero | JumpZero -> 2
        | Input | Output | Halt -> 0

    let parseMode mode =
        match mode with
        | 0 -> Position
        | 1 -> Immediate
        | _ -> failwithf "Unknown parameter mode %i" mode

    let parseOperation opcode =
        match opcode with
        | 1     -> Add
        | 2     -> Multiply
        | 3     -> Input
        | 4     -> Output
        | 5     -> JumpNotZero
        | 6     -> JumpZero
        | 7     -> LessThan
        | 8     -> Equals
        | 99     -> Halt
        | _     -> failwithf "Unknown instruction %i" opcode

    let parseInstruction instruction =
        match instruction with
        | opcode when opcode < 100 ->
            let operation = parseOperation opcode
            let modes = List.replicate (arity operation) Position
            operation, modes
        | _ ->
            let opcode = instruction % 100
            let modes = (digits >> List.rev >> List.map parseMode) (instruction / 100)
            let operation = parseOperation opcode
            
            if arity operation > 0 then
                let fillerModes = (List.replicate (arity operation - List.length modes) Position)
                operation, modes @ fillerModes
            else
                operation, modes

    let getParameters instruction (modes : ParameterConstructor list) (memory : Memory) ip =
        seq { for p in [1..arity instruction] do yield memory.[ip + p] }
        |> Seq.mapi (fun i address -> modes.[i] address)
        |> List.ofSeq

    let evalParameter (memory:Memory) p =
        match p with
        | Immediate p -> p
        | Position  p -> memory.[p]

    let evalInstruction (instruction, (parameters:Parameter list)) state =
        let position = 
            state.Memory.[state.InstructionPointer + arity instruction + 1]
            |> Position

        let nextIp = state.InstructionPointer + arity instruction + 2

        let writeMem state param value = 
            match param with
            | Position pos -> 
                let memory = Array.copy state.Memory
                memory.[pos] <- value
                { state with Memory = memory}
            | Immediate _ -> failwithf "Cannot write to an immediate value"

        let readMem state param =
            match param with
            | Position pos ->
                { state with Output = state.Memory.[pos] :: state.Output }
            | Immediate _ -> failwithf "Cannot read from an immediate value"

        parameters
        |> match instruction with
            | Add -> 
                List.map (evalParameter state.Memory)
                >> List.fold (+) 0
                >> writeMem state position
                >> fun s -> { s with InstructionPointer = nextIp }
            | Multiply ->
                List.map (evalParameter state.Memory)
                >> List.fold (*) 1
                >> writeMem state position
                >> fun s -> { s with InstructionPointer = nextIp }
            | Input -> 
                fun _ ->
                    let value,tail = List.head state.Input, List.tail state.Input
                    Position (state.InstructionPointer + 1)
                    |> evalParameter state.Memory 
                    |> fun p -> writeMem state (Position p) value
                    |> fun s -> { s with Input = tail; InstructionPointer = nextIp }
            | Output -> 
                fun _ ->
                    Position (state.InstructionPointer + 1)
                    |> evalParameter state.Memory
                    |> Position
                    |> readMem state
                    |> fun s -> { s with InstructionPointer = nextIp }
            | JumpNotZero ->
                List.map (evalParameter state.Memory)
                >> (fun [value;jmp] -> if value <> 0 then jmp else nextIp - 1)
                >> fun jmp -> { state with InstructionPointer = jmp }
            | JumpZero ->
                List.map (evalParameter state.Memory)
                >> (fun [value;jmp] -> if value = 0 then jmp else nextIp - 1)
                >> fun jmp -> { state with InstructionPointer = jmp }
            | LessThan ->
                List.map (evalParameter state.Memory)
                >> (fun [a;b] -> if a < b then 1 else 0)
                >> fun b -> writeMem state position b
                >> fun s -> { s with InstructionPointer = nextIp }
            | Equals ->
                List.map (evalParameter state.Memory)
                >> (fun [a;b] -> if a = b then 1 else 0)
                >> fun b -> writeMem state position b
                >> fun s -> { s with InstructionPointer = nextIp }
            | Halt -> failwithf "Cannot perform %A" instruction

    let computer state =
        let (instruction, paramModes) = parseInstruction state.Memory.[state.InstructionPointer]
        match instruction with
        | Halt -> { state with HasHalted = true }
        | _    ->
            let ip = state.InstructionPointer 
            let parameters = getParameters instruction paramModes state.Memory ip
            (evalInstruction (instruction, parameters) state)

    let run =
        doWhile
            computer
            (fun s -> not s.HasHalted)

    let amplifier memory phase signal =
        { startState memory with Input = [phase; signal] }
        |> run
        |> List.head

    let sendSignal input a b c d e =
        let A = amplifier (Array.copy input) a 0
        let B = amplifier (Array.copy input) b (List.head A.Output)
        let C = amplifier (Array.copy input) c (List.head B.Output)
        let D = amplifier (Array.copy input) d (List.head C.Output)
        let E = amplifier (Array.copy input) e (List.head D.Output)
        E.Output |> List.head

    let sendSignal' input phases =
        List.map (fun p -> amplifier (Array.copy input) p) phases
        |> List.fold (fun o amp -> (amp o).Output.Head) 0

    let runPartOne (input:int array) =
        permutations [0..4]
        |> List.map (sendSignal' input)
        |> List.max

    let run2 =
        doWhile
            computer
            (fun s -> (not << List.isEmpty) s.Output || s.HasHalted)

    let amplifier2 memory phase signal =
        { startState memory with Input = [phase; signal] }
        |> run2

    let sendSignal2 memory a b c d e = ()

    let runPartTwo (input:int array) =
        permutations [5..9]
        |> List.map (fun [a;b;c;d;e] -> sendSignal input a b c d e)
        |> List.max

    let readinput () =
        System.IO.File.ReadAllText "Day7/input"
        |> (fun s -> s.Split ",")
        |> Array.map int

    [<Fact>]
    let ``Part one: basic example`` () =
        let input = [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]
        let signal = sendSignal input 4 3 2 1 0
        test <@ 43210 = signal @>

    [<Fact>]
    let ``Part one: answer`` () =
        let answer = runPartOne (readinput ())
        test <@ 24405 = answer @>
