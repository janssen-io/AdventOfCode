﻿namespace AdventOfCode.Event2019

module Wire =

    open System
    open FsCheck
    open FsCheck.Xunit
    open Swensen.Unquote

    type Point = { X : int; Y : int }
    type Line = private Line of Point * Point

    let origin = { X = 0; Y = 0 }

    let distance p1 p2 =
        Math.Abs (p1.X - p2.X) + Math.Abs (p1.Y - p2.Y)

    let rec create p1 p2 =
        let { X = x1; Y = y1} = p1
        let { X = x2; Y = y2} = p2
        match x1 < x2, y1 < y2 with
        | true, false 
        | false, true  -> Line (p1, p2)
        | false, false -> create p2 p1
        | true, true   -> failwith "Unexpected diagonal line"

    let isHorizontal (Line (p1, p2)) = p1.Y = p2.Y

    let between x a b = a <= x && x <= b

    let rec intersection line1 line2 =
        match isHorizontal line1, isHorizontal line2 with
        | true, true 
        | false, false -> None
        | false, true -> intersection line2 line1
        | true, false ->
            let (Line ({ X = x1; Y = y1 }, { X = x2; Y =  _ })) = line1
            let (Line ({ X = x3; Y = y3 }, { X =  _; Y = y4 })) = line2
            if between y1 y3 y4 && between x3 x1 x2 then
                Some ({ X = x3; Y = y1})
            else
                None

    type Generators =
      static member Line() =
        { new Arbitrary<Line>() with
            override x.Generator = gen {
                let! p1 = Arb.generate : Gen<Point>
                let! p2 = 
                    (Arb.generate : Gen<Point>)
                    |> Gen.filter (fun p ->
                        (p.X = p1.X || p.Y = p1.Y)
                        && not (p.X = p1.X && p.Y = p1.Y))
                return (create p1 p2)
            }
        }

    let (=>) p q = not p || q

    let (<=>) p q = (p => q) && (q => p)

    let xor a b = (a && not b) || (b && not a)

    [<Property(MaxTest = 1000, Arbitrary = [|typeof<Generators>|])>]
    let ``intersection: lines starting in the same place intersect`` (line1:Line) (line2:Line) =
        let (Line ({ X = x1; Y = y1 }, _)) = line1
        let (Line ({ X = x3; Y = y3 }, _)) = line2
        let perpendicular = xor (isHorizontal line1) (isHorizontal line2)
        test <@ (perpendicular && x1 = x3 && y1 = y3) => (Option.isSome <| intersection line1 line2) @>

    [<Property(MaxTest = 1000, Arbitrary = [|typeof<Generators>|])>]
    let ``intersection: lines ending in the same place intersect`` (line1:Line) (line2:Line) =
        let (Line (_, { X = x2; Y = y2})) = line1
        let (Line (_, { X = x4; Y = y4 })) = line2
        let perpendicular = xor (isHorizontal line1) (isHorizontal line2)
        test <@ (perpendicular && x2 = x4 && y2 = y4) => (Option.isSome <| intersection line1 line2) @>

    [<Property(MaxTest = 1000, Arbitrary = [|typeof<Generators>|])>]
    let ``intersection: if lines intersect then they are perpendicular`` (line1:Line) (line2:Line) =
        test <@ (Option.isSome <| intersection line1 line2) => xor (isHorizontal line1) (isHorizontal line2) @>

    [<Property(MaxTest = 1000, Arbitrary = [|typeof<Generators>|])>]
    let ``intersection: intersection is inside bounding box`` (line1:Line) (line2:Line) =
        let intersection = intersection line1 line2
        if Option.isNone intersection then
            ()
        else 
            let (Line ({ X = x1; Y = y1}, { X = x2; Y = y2})) = line1
            let (Line ({ X = x3; Y = y3}, { X = x4; Y = y4 })) = line2
            let intersect = Option.get intersection

            test <@ (intersect.X <= x2) && (intersect.Y <= y2) @>
            test <@ (intersect.X <= x4) && (intersect.Y <= y4) @>
            test <@ (intersect.X >= x1) && (intersect.Y >= y1) @>
            test <@ (intersect.X >= x3) && (intersect.Y >= y3) @>

    [<Property(MaxTest = 1000, Arbitrary = [|typeof<Generators>|])>]
    let ``intersection: is commutative`` (line1 : Line) (line2 : Line) =
        test <@ intersection line2 line1 = intersection line1 line2 @>

    [<Property(MaxTest = 1000)>]
    let ``distance: is zero iff points are identical`` (p1 : Point) (p2 : Point) =
        test <@ distance p1 p2 = 0 <=> (p1 = p2) @>
