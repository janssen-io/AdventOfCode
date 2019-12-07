namespace AdventOfCode.Event2019

module UtilsTests =

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Swensen.Unquote
    open System

    open Utils

    [<Property>]
    let ``digits: concatenation equals input`` (x:PositiveInt) =
        let ds = digits x.Get
        let number = String.Join ("", ds)
        test <@ string x.Get = number @>

    [<Property(EndSize=8)>]
    let ``permutations: length equals n!`` (xs:int list) =
        test <@ (permutations >> List.length) xs = (List.length >> factorial) xs @>

    [<Fact>]
    let ``factorial`` () =
        test <@ factorial 0 = 1 @>
        test <@ factorial 1 = 1 @>
        test <@ factorial 2 = 2 @>
        test <@ factorial 3 = 6 @>
        test <@ factorial 5 = 5 * 4 * 3 * 2 * 1 @>

