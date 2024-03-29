namespace AdventOfCode.Event2019

open System
open System.Diagnostics

module Day1 =

    open Utils
    open Xunit
    open FsCheck.Xunit

    let input = [
            137569
            146535
            74662
            133844
            99969
            86606
            76237
            52902
            106211
            141865
            50865
            101011
            75956
            67501
            142146
            107706
            83492
            137253
            56296
            141256
            118232
            127402
            67455
            64062
            72416
            109547
            106144
            54832
            57057
            74884
            80923
            85121
            60461
            92743
            134175
            65671
            90198
            134055
            59568
            146576
            134488
            130355
            54782
            51370
            55501
            56555
            62140
            99558
            80875
            113451
            71048
            64890
            94481
            87468
            148972
            81742
            79471
            100999
            106741
            142433
            130225
            58789
            134365
            81310
            102004
            92736
            105542
            63097
            92747
            109214
            103305
            143659
            68254
            126409
            71724
            50284
            125431
            132227
            125600
            99131
            96598
            101007
            123104
            82215
            97310
            135824
            117379
            81546
            109472
            85571
            89292
            109530
            127656
            56654
            132463
            101948
            118835
            59125
            116089
            61605
        ]

    let fuelRequirement mass =
        Math.Max(0, mass / 3 - 2)

    let answer = (List.map fuelRequirement >> List.sum)

    let rec recursiveFuelRequirement mass =
        doWhile
            fuelRequirement
            (fun m -> m > 0)
            mass
        |> List.sum

    let answer2 =
        (List.map recursiveFuelRequirement >> List.sum)

    [<Property>]
    let ``Basic example`` () =
        fuelRequirement 12 = 2

    [<Property>]
    let ``Round down example`` () =
        fuelRequirement 14 = 2

    [<Property>]
    let ``Big example`` () =
        fuelRequirement 1969 = 654
        && fuelRequirement 100756 = 33583

    [<Fact>]
    let ``Answer Part One`` () =
        Assert.Equal(3219099, answer input)

    [<Fact>]
    let ``Part two: basic example`` () =
        Assert.Equal(2, recursiveFuelRequirement 14)
        
    [<Fact>]
    let ``Part two: bigger example`` () =
        Assert.Equal(966, recursiveFuelRequirement 1969)
        Assert.Equal(50346, recursiveFuelRequirement 100756)

    [<Fact>]
    let ``Part two: answer`` () =
        Assert.Equal(4825810, answer2 input)

