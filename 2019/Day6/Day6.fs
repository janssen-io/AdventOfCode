namespace AdventOfCode.Event2019

open System.Diagnostics

module Tree =
    type ('a, 'b) Tree =
        | Node of 'a * (Tree<'a, 'b> list)
        | Leaf of 'b

    let rec fold fleaf fnode acc tree =
        let fold' = fold fleaf fnode
        match tree with
        | Leaf leaf ->
            fleaf acc leaf
        | Node (node, children) ->
            let acc' = fnode acc node
            Seq.fold fold' acc' children

    let rec map fleaf fnode tree =
        let map' = map fleaf fnode
        match tree with
        | Leaf leaf ->
            fleaf leaf |> Leaf
        | Node (node, children) ->
            Node (fnode node, List.map map' children)

    let rec maptree ftree tree =
        match tree with
        | Leaf leaf -> ftree (Leaf leaf) |> Leaf
        | Node (node, children) ->
            let treeNode = Node (node, children)
            Node (ftree treeNode, List.map (maptree ftree) children)

    let stringify (tree:Tree<string, string>) : string =
        let getValue c acc value = (sprintf "(%s %s)" c value) :: acc
        let values = fold (getValue "l") (getValue "n") [] tree
        values |> List.rev |> (String.concat "")

module Day6 =

    open System
    open Tree
    open Xunit
    open Swensen.Unquote

    type OrbitMap = Map<string, string list>

    let rec orbits tree =
        match tree with
        | Leaf _ -> 0
        | Node (_, children) ->
            let l =
                let directOrbits = List.length children
                let indirectOrbits =
                    (List.map orbits children)
                    |> List.sum
                directOrbits + indirectOrbits
            l

    let rec allOrbits tree =
        Tree.maptree orbits tree
        |> Tree.fold (+) (+) 0

    let sequence (xs:'a option list) =
        let rec sequence' ys acc =
            match ys with
            | [] -> acc
            | (Some y :: ys') -> y :: (sequence' ys' acc)
            | (None :: ys') -> sequence' ys' acc

        match sequence' xs [] with
        | [] -> None
        | result -> Some result

    let rec distanceTo obj tree =
        match tree with
        | Leaf name when name = obj ->
            Some -1
        | Leaf   _ -> 
            None
        | Node (name, _) when name = obj ->
            Some -1
        | Node (_, children) ->
            List.map (distanceTo obj) children
            |> List.map (Option.map ((+) 1))
            |> sequence
            |> Option.map List.min

    let orbitsBetween a b tree =
        let fleaf acc _ = acc
        let fnode acc (na, nb) =
            match acc, na, nb with
            | None, Some na', Some nb' -> Some (na', nb')
            | Some acc', Some na', Some nb' -> Some (na', nb')
            | Some acc', _, _ -> acc
            | _, _, _ -> None

        let dtree = Tree.maptree (fun t -> (distanceTo a t, distanceTo b t)) tree
        Debug.WriteLine dtree
        dtree |> Tree.fold fleaf fnode None


    let parseInput orbits =
        let addRelation left right orbitmap =
            if Map.containsKey left orbitmap then
                Map.add left (right @ orbitmap.[left]) orbitmap
            else
                Map.add left right orbitmap

        let rec parse (orbits':string list) orbitmap =
            match orbits' with
            | [] -> orbitmap
            | (o::os) ->
                let [|left; right|] = o.Split ')'
                orbitmap
                |> addRelation left [right]
                |> addRelation right []
                |> parse os
        parse orbits (Map.ofList [])

    let createTree orbitmap =
        let rec createSubtree (orbitmap:OrbitMap) currentNode =
            let orbiters = orbitmap.[currentNode]
            if List.isEmpty orbiters then
                Leaf currentNode
            else
                let children = List.map (createSubtree orbitmap) orbiters
                Node (currentNode, children)
        createSubtree orbitmap "COM"

    [<Fact>]
    let ``parseInput: Orbit map contains an entry for each node`` () =
        let orbitmap = 
            parseInput
            <| ["COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L"]
        test <@ 12 = orbitmap.Count @>

    [<Fact>]
    let ``createTree: can parse basic example part one`` () =
        let tree = 
            (parseInput >> createTree)
            <| ["COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L"]
        test 
           <@ tree =
                Node ("COM", [
                    Node ("B", [
                        Node ("G", [Leaf "H"])
                        Node ("C", [
                            Node ("D", [
                                Leaf "I"
                                Node ("E", [
                                    Node ("J", [
                                        Node ("K", [Leaf "L"])
                                    ])
                                    Leaf "F"
                                ])
                            ])
                        ])
                    ])
                ])
           @>

    [<Fact>]
    let ``orbits: simplified example`` () =
        let ds = 
            allOrbits
            <| Node ("COM", [
                Node ("B", [
                    Node ("G", [Leaf "H"])
                    Node ("C", [
                        Node ("D", [
                            Leaf "I"
                        ])
                    ])
                ])
            ])
        test <@ 15 = ds @>

    [<Fact>]
    let ``Part one: basic example`` () =
        let tree = 
            (parseInput >> createTree)
            <| ["COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L"]
        let ds = allOrbits tree
        test <@ 42 = ds @>

    [<Fact>]
    let ``Part one: answer`` () =
        let orbits =
            "Day6/input"
            |> (Utils.readlines >> parseInput >> createTree >> allOrbits)
        test <@ 151345 = orbits @>

    [<Fact>]
    let ``Part two: basic example`` () =
        let tree = 
            (parseInput >> createTree)
            <| ["COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L"; "K)YOU"; "I)SAN"]
        let ds = orbitsBetween "SAN" "YOU" tree
        test <@ (Some (1, 3)) = ds @>

    [<Fact>]
    let ``Part two: answer`` () =
        let ds = 
            "Day6/input"
            |> (Utils.readlines >> parseInput >> createTree >> orbitsBetween "SAN" "YOU")
        test <@ Option.map (Utils.uncurry2 (+)) ds = Some 391 @>

            

