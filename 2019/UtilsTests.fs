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

