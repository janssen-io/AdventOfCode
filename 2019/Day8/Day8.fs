namespace AdventOfCode.Event2019


module Day8 =

    open Xunit
    open Swensen.Unquote
    open System.Diagnostics

    let image = System.IO.File.ReadAllText @"D:\Software\Source\AdventOfCode\2019\Day8\input"

    type Pixel =
        | Black
        | White
        | Transparent

    let parse pixel =
        match pixel with
        | '0' -> Black
        | '1' -> White
        | '2' -> Transparent
        | _ -> failwithf "Unknown pixel: %A" pixel

    let unparse pixel =
        match pixel with
        | Black -> " "
        | White -> "#"
        | Transparent -> " "

    let partOne w h image = 
        let valueCount n = Seq.filter ((=) n) >> Seq.length
        image
        |> Seq.map parse
        |> Seq.chunkBySize w 
        |> Seq.chunkBySize h
        |> Seq.map Seq.concat
        |> Seq.minBy (fun s -> Seq.fold (fun acc s -> if s = Black then acc + 1 else acc) 0 s)
        |> fun layer -> (valueCount White layer) * (valueCount Transparent layer)

    let partTwo w h image = 
        let overlay layer1 layer2 =
            Seq.map2 (fun p1 p2 -> if p2 = Transparent then p1 else p2) layer1 layer2
    
        image
        |> Seq.map parse
        |> Seq.chunkBySize w 
        |> Seq.chunkBySize h
        |> Seq.map Seq.concat
        |> Seq.rev
        |> fun s -> Seq.fold overlay (Seq.head s) s
        |> Seq.map unparse
        |> Seq.chunkBySize w
        |> Seq.map (Seq.fold (+) "")
        |> Utils.tee (Seq.iter Debug.WriteLine)

    [<Fact>]
    let ``Part one`` () =
        test <@ 1620 = partOne 25 6 image @>

    [<Fact>]
    let ``Part two`` () =
        test <@ Seq.singleton "" <> (partTwo 25 6 image) @>
