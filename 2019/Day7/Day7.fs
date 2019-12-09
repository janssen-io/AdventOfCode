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
        Name : string
    }

    let startState memory =
        { Memory = memory 
          InstructionPointer = 0
          HasHalted = false 
          Input = []
          Output = []
          Name = "Untitled"
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
            lazy (
                state.Memory.[state.InstructionPointer + arity instruction + 1]
                |> Position
            )

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

        match instruction with
        | Halt ->
            Debug.WriteLine (sprintf "%s.%2i do %A" state.Name state.InstructionPointer instruction)
        | _ ->            
            Debug.WriteLine (sprintf "%s.%2i do %A with %A on %A" state.Name state.InstructionPointer instruction parameters (position.Force ()))
       
        let f () = 
            parameters
            |> match instruction with
                | Add -> 
                    List.map (evalParameter state.Memory)
                    >> List.fold (+) 0
                    >> writeMem state (position.Force())
                    >> fun s -> { s with InstructionPointer = nextIp }
                | Multiply ->
                    List.map (evalParameter state.Memory)
                    >> List.fold (*) 1
                    >> writeMem state (position.Force())
                    >> fun s -> { s with InstructionPointer = nextIp }
                | Input -> 
                    fun _ ->
                        let value,tail = List.head state.Input, List.tail state.Input
                        position
                        |> fun p -> writeMem state  (p.Force()) value
                        |> fun s -> { s with Input = tail; InstructionPointer = nextIp }
                | Output -> 
                    fun _ ->
                        (position.Force())
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
                    >> fun b -> writeMem state (position.Force()) b
                    >> fun s -> { s with InstructionPointer = nextIp }
                | Equals ->
                    List.map (evalParameter state.Memory)
                    >> (fun [a;b] -> if a = b then 1 else 0)
                    >> fun b -> writeMem state (position.Force()) b
                    >> fun s -> { s with InstructionPointer = nextIp }
                | Halt -> fun _ -> { state with HasHalted = true }

        let result = f ()
        result

    let computer state =
        let (instruction, paramModes) = parseInstruction state.Memory.[state.InstructionPointer]
        let ip = state.InstructionPointer 
        let parameters = getParameters instruction paramModes state.Memory ip
        evalInstruction (instruction, parameters) state

    let rec run state =
        let (instruction, _) = parseInstruction state.Memory.[state.InstructionPointer]
        if List.isEmpty state.Input && instruction = Input || state.HasHalted then
            state
        else
            (computer >> run) state

    let amplifier memory phase signal =
        { startState memory with Input = [phase; signal] }
        |> run

    let sendSignal' input phases =
        List.map (amplifier (Array.copy input)) phases
        |> List.fold (fun o amp -> List.head (amp o).Output) 0

    let runPartOne (input:int array) =
        permutations [0..4]
        |> List.map (sendSignal' input)
        |> List.max

    let rec run2 =
        doWhile computer (fun state ->
            let (instruction, _) = parseInstruction state.Memory.[state.InstructionPointer]
            not (instruction = Input && List.isEmpty state.Input)
            && not state.HasHalted)

    let initializeAmp memory phase signal =
        { startState memory with Input = [phase; signal]; Name = string phase }
        |> run2
        |> List.head

    let rec updateAmp state signal =
        match signal with
            | None -> { state with Input = []; Output = [] }
            | Some sign -> { state with Input = [sign]; Output = [] }
        |> run2
        |> List.head

    let sendSignal2 input phases =
        let amplifiers =
            List.scan
                (fun prevAmp phase ->
                    initializeAmp (Array.copy input) phase prevAmp.Output.Head
                )
                { startState [||] with Output = [0] }
                phases
            |> List.tail

        let run amps =
            amps
            |> List.scan
                (fun prevAmp currAmp -> updateAmp currAmp (List.tryHead prevAmp.Output))
                (List.last amps)
            |> List.tail

        let rec calcSignal amps acc =
            if (List.last amps).HasHalted then
                List.head acc
            else
                let updatedAmps = run amps
                let output = List.head (List.last updatedAmps).Output
                calcSignal updatedAmps (output :: acc)

        calcSignal amplifiers []


    let runPartTwo (input:int array) =
        permutations [5..9]
        |> List.map (sendSignal2 input)
        |> List.max

    let readinput () =
        System.IO.File.ReadAllText "Day7/input"
        |> (fun s -> s.Split ",")
        |> Array.map int

    [<Fact>]
    let ``Part one: basic example`` () =
        let input = [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]
        let signal = sendSignal' input [4;3;2;1;0]
        test <@ 43210 = signal @>

    [<Fact>]
    let ``Part one: answer`` () =
        let answer = runPartOne (readinput ())
        test <@ 24405 = answer @>

    [<Fact>]
    let ``Part two: basic example`` () =
        let input = [|3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5|]
        let result = sendSignal2 input [9;8;7;6;5]
        test <@ result = 139_629_729 @>

    [<Fact>]
    let ``Part two: answer`` () =
        let answer = runPartTwo (readinput ())
        test <@ 8271623 = answer @>
