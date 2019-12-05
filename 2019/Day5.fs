namespace AdventOfCode.Event2019

open System.Diagnostics


module Day5 =

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

    type Instruction = Operation * int list

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
        |> Seq.mapi (fun i address -> 
            modes.[i] address
        )
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

    let oldComputer = computer

    let run =
        doWhile
            computer
            (fun s -> not s.HasHalted)

    let runPartOne (input:int array) =
        let memory' = Array.copy input
        let rec compute state =
            if state.HasHalted then
                state
            else
                (computer >> compute) state
        compute ( { startState memory' with Input = [1] })

    [<Property>]
    let ``Part one: Add`` (a:PositiveInt) (b:PositiveInt) (p:PositiveInt) =
        let x = a.Get % 5
        let y = b.Get % 5
        let input = [|1; x; y; p.Get % 4; 99|]
        let expected = input.[x] + input.[y]
        let output = (oldComputer (startState input)).Memory
        let actual = output.[p.Get % 4]
        test <@ expected = actual @>

    [<Property>]
    let ``Part one: Multiply`` (a:PositiveInt) (b:PositiveInt) (p:PositiveInt) =
        let x = a.Get % 5
        let y = b.Get % 5
        let input = [|2; x; y; p.Get % 4; 99|]
        let expected = input.[x] * input.[y]
        let output = (oldComputer (startState input)).Memory
        let actual = output.[p.Get % 4]
        test <@ expected = actual @>

    [<Fact>]
    let ``Part one: example day 2`` () =
        let output = runPartOne [|1;9;10;3;2;3;11;0;99;30;40;50|]
        Assert.True([|3500;9;10;70;2;3;11;0;99;30;40;50|] = output.Memory)

    [<Fact>]
    let ``Part one: simple example`` () =
        let state  = startState [|1002;4;3;4;33|]
        let state' = oldComputer state
        test <@ [|1002;4;3;4;99|] = state'.Memory @>
        test <@ 4 = state'.InstructionPointer @>
        test <@ (oldComputer state').HasHalted @>

    [<Fact>]
    let ``Part one: input operation`` () =
        let state  = { startState [|3;0;99|] with Input = [1;2] }
        let state' = computer state
        test <@ [|1;0;99|] = state'.Memory @>
        test <@ 2 = state'.InstructionPointer @>
        test <@ [2] = state'.Input @>
        test <@ (computer state').HasHalted @>

    [<Fact>]
    let ``Part one: output operation`` () =
        let state  = startState [|4;0;99|]
        let state' = computer state
        test <@ [4] = state'.Output @>

    [<Fact>]
    let ``run equals recursive computer`` () =
        let state  = { startState [|3;0;99|] with Input = [1] }
        test <@ runPartOne state.Memory = (List.head <| run state) @>

    [<Fact>]
    let ``Part two: equals in position mode`` () =
        let state = { startState [|3;9;8;9;10;9;4;9;99;-1;8|] with Input = [8] }
        let output = (List.head (run state)).Output
        test <@ output = [1] @>

        let state = { startState [|3;9;8;9;10;9;4;9;99;-1;8|] with Input = [2] }
        let output = (List.head (run state)).Output
        test <@ output = [0] @>

    [<Fact>]
    let ``Part two: less than in position mode`` () =
        let state = { startState [|3;9;7;9;10;9;4;9;99;-1;8|] with Input = [2] }
        let output = (List.head (run state)).Output
        test <@ output = [1] @>

        let state = { startState [|3;9;7;9;10;9;4;9;99;-1;8|] with Input = [8] }
        let output = (List.head (run state)).Output
        test <@ output = [0] @>

    [<Fact>]
    let ``Part two: equals in immediate mode`` () =
        let state = { startState [|3;3;1108;-1;8;3;4;3;99|] with Input = [8] }
        let output = (List.head (run state)).Output
        test <@ output = [1] @>

        let state = { startState [|3;3;1108;-1;8;3;4;3;99|] with Input = [2] }
        let output = (List.head (run state)).Output
        test <@ output = [0] @>

    [<Fact>]
    let ``Part two: less than in immediate mode`` () =
        let state = { startState [|3;3;1107;-1;8;3;4;3;99|] with Input = [2] }
        let output = (List.head (run state)).Output
        test <@ output = [1] @>

        let state = { startState [|3;3;1107;-1;8;3;4;3;99|] with Input = [8] }
        let output = (List.head (run state)).Output
        test <@ output = [0] @>

    [<Fact>]
    let ``Part two: jump not zero`` () =
        let state = { startState [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|] with Input = [0] }
        let output = (List.head (run state)).Output
        test <@ output = [0] @>

        let state = { startState [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|] with Input = [20] }
        let output = (List.head (run state)).Output
        test <@ output = [1] @>
    

    [<Fact>]
    let ``Part two: jump zero`` () =
        let state = { startState [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|] with Input = [0] }
        let output = (List.head (run state)).Output
        test <@ output = [0] @>

        let state = { startState [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|] with Input = [20] }
        let output = (List.head (run state)).Output
        test <@ output = [1] @>
    
    [<Fact>]
    let ``Part one: answer`` () =
        let input = [|3;225;1;225;6;6;1100;1;238;225;104;0;1101;37;34;224;101;-71;224;224;4;224;1002;223;8;223;101;6;224;224;1;224;223;223;1002;113;50;224;1001;224;-2550;224;4;224;1002;223;8;223;101;2;224;224;1;223;224;223;1101;13;50;225;102;7;187;224;1001;224;-224;224;4;224;1002;223;8;223;1001;224;5;224;1;224;223;223;1101;79;72;225;1101;42;42;225;1102;46;76;224;101;-3496;224;224;4;224;102;8;223;223;101;5;224;224;1;223;224;223;1102;51;90;225;1101;11;91;225;1001;118;49;224;1001;224;-140;224;4;224;102;8;223;223;101;5;224;224;1;224;223;223;2;191;87;224;1001;224;-1218;224;4;224;1002;223;8;223;101;4;224;224;1;224;223;223;1;217;83;224;1001;224;-124;224;4;224;1002;223;8;223;101;5;224;224;1;223;224;223;1101;32;77;225;1101;29;80;225;101;93;58;224;1001;224;-143;224;4;224;102;8;223;223;1001;224;4;224;1;223;224;223;1101;45;69;225;4;223;99;0;0;0;677;0;0;0;0;0;0;0;0;0;0;0;1105;0;99999;1105;227;247;1105;1;99999;1005;227;99999;1005;0;256;1105;1;99999;1106;227;99999;1106;0;265;1105;1;99999;1006;0;99999;1006;227;274;1105;1;99999;1105;1;280;1105;1;99999;1;225;225;225;1101;294;0;0;105;1;0;1105;1;99999;1106;0;300;1105;1;99999;1;225;225;225;1101;314;0;0;106;0;0;1105;1;99999;7;226;226;224;102;2;223;223;1005;224;329;101;1;223;223;108;677;226;224;102;2;223;223;1005;224;344;1001;223;1;223;1108;226;677;224;102;2;223;223;1005;224;359;1001;223;1;223;8;677;226;224;102;2;223;223;1006;224;374;1001;223;1;223;107;226;226;224;102;2;223;223;1006;224;389;101;1;223;223;1108;677;226;224;1002;223;2;223;1005;224;404;1001;223;1;223;108;677;677;224;102;2;223;223;1005;224;419;101;1;223;223;7;226;677;224;1002;223;2;223;1006;224;434;1001;223;1;223;107;226;677;224;102;2;223;223;1005;224;449;101;1;223;223;1108;677;677;224;1002;223;2;223;1006;224;464;101;1;223;223;7;677;226;224;102;2;223;223;1006;224;479;101;1;223;223;1007;677;677;224;1002;223;2;223;1005;224;494;101;1;223;223;1008;226;226;224;102;2;223;223;1006;224;509;1001;223;1;223;107;677;677;224;102;2;223;223;1006;224;524;1001;223;1;223;8;226;226;224;1002;223;2;223;1005;224;539;1001;223;1;223;1007;677;226;224;102;2;223;223;1006;224;554;1001;223;1;223;1007;226;226;224;1002;223;2;223;1005;224;569;1001;223;1;223;8;226;677;224;1002;223;2;223;1006;224;584;101;1;223;223;108;226;226;224;1002;223;2;223;1006;224;599;101;1;223;223;1107;677;226;224;1002;223;2;223;1005;224;614;1001;223;1;223;1107;226;677;224;102;2;223;223;1006;224;629;1001;223;1;223;1008;226;677;224;102;2;223;223;1005;224;644;101;1;223;223;1107;226;226;224;102;2;223;223;1006;224;659;1001;223;1;223;1008;677;677;224;102;2;223;223;1006;224;674;1001;223;1;223;4;223;99;226|]
        let states = run { startState input with Input = [5] }
        let output = (List.head states).Output
        test <@ List.forall ((=) 0) (List.tail output) @>

    [<Fact>]
    let ``Part two: answer`` () =
        let input = [|3;225;1;225;6;6;1100;1;238;225;104;0;1101;37;34;224;101;-71;224;224;4;224;1002;223;8;223;101;6;224;224;1;224;223;223;1002;113;50;224;1001;224;-2550;224;4;224;1002;223;8;223;101;2;224;224;1;223;224;223;1101;13;50;225;102;7;187;224;1001;224;-224;224;4;224;1002;223;8;223;1001;224;5;224;1;224;223;223;1101;79;72;225;1101;42;42;225;1102;46;76;224;101;-3496;224;224;4;224;102;8;223;223;101;5;224;224;1;223;224;223;1102;51;90;225;1101;11;91;225;1001;118;49;224;1001;224;-140;224;4;224;102;8;223;223;101;5;224;224;1;224;223;223;2;191;87;224;1001;224;-1218;224;4;224;1002;223;8;223;101;4;224;224;1;224;223;223;1;217;83;224;1001;224;-124;224;4;224;1002;223;8;223;101;5;224;224;1;223;224;223;1101;32;77;225;1101;29;80;225;101;93;58;224;1001;224;-143;224;4;224;102;8;223;223;1001;224;4;224;1;223;224;223;1101;45;69;225;4;223;99;0;0;0;677;0;0;0;0;0;0;0;0;0;0;0;1105;0;99999;1105;227;247;1105;1;99999;1005;227;99999;1005;0;256;1105;1;99999;1106;227;99999;1106;0;265;1105;1;99999;1006;0;99999;1006;227;274;1105;1;99999;1105;1;280;1105;1;99999;1;225;225;225;1101;294;0;0;105;1;0;1105;1;99999;1106;0;300;1105;1;99999;1;225;225;225;1101;314;0;0;106;0;0;1105;1;99999;7;226;226;224;102;2;223;223;1005;224;329;101;1;223;223;108;677;226;224;102;2;223;223;1005;224;344;1001;223;1;223;1108;226;677;224;102;2;223;223;1005;224;359;1001;223;1;223;8;677;226;224;102;2;223;223;1006;224;374;1001;223;1;223;107;226;226;224;102;2;223;223;1006;224;389;101;1;223;223;1108;677;226;224;1002;223;2;223;1005;224;404;1001;223;1;223;108;677;677;224;102;2;223;223;1005;224;419;101;1;223;223;7;226;677;224;1002;223;2;223;1006;224;434;1001;223;1;223;107;226;677;224;102;2;223;223;1005;224;449;101;1;223;223;1108;677;677;224;1002;223;2;223;1006;224;464;101;1;223;223;7;677;226;224;102;2;223;223;1006;224;479;101;1;223;223;1007;677;677;224;1002;223;2;223;1005;224;494;101;1;223;223;1008;226;226;224;102;2;223;223;1006;224;509;1001;223;1;223;107;677;677;224;102;2;223;223;1006;224;524;1001;223;1;223;8;226;226;224;1002;223;2;223;1005;224;539;1001;223;1;223;1007;677;226;224;102;2;223;223;1006;224;554;1001;223;1;223;1007;226;226;224;1002;223;2;223;1005;224;569;1001;223;1;223;8;226;677;224;1002;223;2;223;1006;224;584;101;1;223;223;108;226;226;224;1002;223;2;223;1006;224;599;101;1;223;223;1107;677;226;224;1002;223;2;223;1005;224;614;1001;223;1;223;1107;226;677;224;102;2;223;223;1006;224;629;1001;223;1;223;1008;226;677;224;102;2;223;223;1005;224;644;101;1;223;223;1107;226;226;224;102;2;223;223;1006;224;659;1001;223;1;223;1008;677;677;224;102;2;223;223;1006;224;674;1001;223;1;223;4;223;99;226|]
        let states = run { startState input with Input = [5] }
        let output = (List.head states).Output
        test <@ output.Length = 1 @>

