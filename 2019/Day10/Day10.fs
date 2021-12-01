namespace AdventOfCode.Event2019

open System.Diagnostics

module Day10 =

    open Utils
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Swensen.Unquote

    type AsteroidMap = string list

    type Asteroid = int * int

    
    module Vector =
        type Vector = {
            Length : float
            Direction : Utils.Ratio
        }

        let compareVector v1 v2 =
            let direction = Utils.compareRatio v1.Direction v2.Direction
            if direction = 0 then
                compare v1.Length v2.Length
            else
                direction

        let create (x1,y1) (x2,y2) =
            let adjacent = x2 - x1
            let opposite = y1 - y2
            { 
                Length = sqrt (float adjacent ** 2.0 + float opposite ** 2.0)
                Direction = ratio opposite adjacent
            }

    let add (x, y) ({ Length = l; Direction = (Ratio (opp, adj)) }: Vector.Vector) : Asteroid =
        // sohcahtoa
        let angle = (atan2 (float opp) (float adj)) * (180.0 / System.Math.PI)
        int ((cos angle) * l), int ((sin angle) * l)


    let readMap (lines: string list) : Asteroid seq =
        let readRow (i:int) (row:string) : Asteroid seq = 
            seq {
                for j in 0..row.Length - 1 do
                    if row.[j] = '#' then
                        yield (i, j)
            }
        (Array.mapi readRow (Array.ofList lines)) 
        |> Seq.ofArray
        |> Seq.concat


    let genFold (folder : 'State -> 'T -> 'State) (seed : 'State) (generator : 'T list Gen) : 'State Gen =
        gen {
            let! items = generator
            return (List.fold folder seed items)
        }

    let countAsteroids asteroids asteroid =
        Seq.except (Seq.singleton asteroid) asteroids
        |> Seq.map (fun other -> ((Vector.create asteroid other).Direction, other))
        |> Map.ofSeq

    let listAsteroids asteroids asteroid =
        Seq.except (Seq.singleton asteroid) asteroids
        |> Seq.map (fun other -> ((Vector.create asteroid other), other))
        |> Seq.fold 
            (fun directionAsteroid (vector, other) ->
                let seenAsteroids =
                    if Map.containsKey vector.Direction directionAsteroid then
                        directionAsteroid.[vector.Direction]
                    else
                        []
                Map.add vector.Direction (other :: seenAsteroids) directionAsteroid
               )
            Map.empty
        |> Map.map (fun _ others ->  others)
        
    let partOne (asteroids: Asteroid seq) =
        let counts = Seq.map (countAsteroids asteroids) asteroids
        let x = 1
        Seq.map (fun (m:Map<'a, 'b>) -> m.Count) counts
        |> Seq.append (Seq.singleton 0)
        |> Seq.max

    let partTwo (asteroids : Asteroid seq) =
        let visibleAsteroids = 
            Seq.fold
                (fun m asteroid ->
                    Map.add asteroid (listAsteroids asteroids asteroid) m)
                Map.empty
                asteroids
        let selectedAsteroid = asteroids |> Seq.maxBy (countAsteroids asteroids >> (fun m -> m.Count))
        test <@ visibleAsteroids.[selectedAsteroid].Count = 299 @>

        let applyToFst f ta tb = f (fst ta) (fst tb)

        let otherAsteroids = 
            visibleAsteroids.[selectedAsteroid]
            //|> Map.map (fun _ -> List.sortWith (fun v1 v2 -> compare v1 v2))
            |> Map.toSeq
            |> Seq.sortWith (applyToFst compareRatio)
            |> Seq.toArray

        let amountOfCircles = 199 / otherAsteroids.Length
        let remainder = 199 - (otherAsteroids.Length * amountOfCircles)
        Debug.WriteLine (sprintf "%i %i %i" otherAsteroids.Length amountOfCircles remainder)

        let asteroid200 = (snd otherAsteroids.[199]).[0]
        //add selectedAsteroid asteroid200
        asteroid200

        // 1828
        // 2527
        // 2228

        //1174  < answer
        //1226  < answer
        //3521  > answer
        //3422 <> answer
        // 

    
    type Generators =
        static member AsteroidMap() =
          { new Arbitrary<AsteroidMap>() with
              override x.Generator = 
                  let asteroidMap size = gen {
                      let generateSpot = Gen.oneof (Seq.ofList [Gen.constant '.'; Gen.constant '#'])
                      let generateRow = 
                        Gen.listOfLength size generateSpot
                        |> genFold (fun row c -> row + string c) ""
                      let xs = (Gen.listOfLength size generateRow)
                      return! xs
                  }
                  Gen.sized asteroidMap
              
          }

    let numberOfAsteroids =
        Seq.concat >> Seq.fold (fun acc c -> if c = '#' then acc + 1 else acc) 0
    
    [<Property(Arbitrary = [|typeof<Generators>|], EndSize = 20)>]
    let ``readMap: transforms '#' into Asteroids`` (asteroidMap : AsteroidMap) =
        Debug.WriteLine (sprintf "%A" <| Seq.concat asteroidMap)
        test <@ (numberOfAsteroids asteroidMap) = (readMap >> Seq.length) asteroidMap @>

    [<Property(Arbitrary = [|typeof<Generators>|], EndSize = 20)>]
    let ``Number of visible asteroids is smaller than the number of asteroids`` (asteroidMap:AsteroidMap) =
        let answer = (readMap >> partOne) asteroidMap
        let totalAsteroids = numberOfAsteroids asteroidMap
        test <@ totalAsteroids = 0 || answer < totalAsteroids @>

    [<Fact>]
    let ``Part one: basic example`` () =
        let a = [
            ".#..#"
            "....."
            "#####"
            "....#"
            "...##"
        ]
        let answer = (readMap >> partOne) a
        test <@ answer = 8 @>

    [<Fact>]
    let ``Part one: big example`` () =
        let a = [
            "#.#...#.#."
            ".###....#."
            ".#....#..."
            "##.#.#.#.#"
            "....#.#.#."
            ".##..###.#"
            "..#...##.."
            "..##....##"
            "......#..."
            ".####.###."
        ]
        let map = readMap a
        test <@ partOne map = 35 @>

    [<Fact>]
    let ``Part one; bigger example`` () =
        let a = [
            ".#..#..###"
            "####.###.#"
            "....###.#."
            "..###.##.#"
            "##.##.#.#."
            "....###..#"
            "..#.#..#.#"
            "#..#.#.###"
            ".##...##.#"
            ".....#.#.."
        ]
        let answer = (readMap >> partOne) a
        test <@ answer = 41 @>

    [<Fact>]
    let ``Part one: answer`` () =
        let answer = (readlines >> readMap >> partOne) "Day10/input"
        test <@ answer = 299 @>

    [<Fact>]
    let ``Part two: answer`` () =
        let answer = (readlines >> readMap >> partTwo) "Day10/input"
        test <@ answer = (0,0) @>

    [<Fact>]
    let ``Test`` () =
        test <@ Vector.compareVector { Length = 2.0; Direction = ratio 2 3 } { Length = 3.0; Direction = ratio 2 3 } = -1 @>

    [<Fact>]
    let ``Test 2`` () =
        test <@ Vector.compareVector { Length = 2.0; Direction = ratio 1 0 } { Length = 2.0; Direction = ratio 1 1 } = -1 @>

    [<Fact>]
    let ``Test 3`` () =
        test <@ compare 1 2 = -1 @>
        test <@ compareRatio (ratio 1 0) (ratio 1 1) = 1 @>
        test <@ compareRatio (ratio 1 0) (ratio 1 -1) = -1 @>
