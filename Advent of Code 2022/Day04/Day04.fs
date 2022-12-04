module Day04

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "Day04.csv")

type Range = {
    Low: int
    High: int
}

let intRange (range: string) =
    range.Split "-" |> Array.map int

let toRangeTuple (line: string[]) =
    let r1 = intRange line[0]
    let r2 = intRange line[1]
    ({ Low = r1[0]; High = r1[1]}, { Low = r2[0]; High = r2[1]})


module Task01 =

    let (|HighAndLowOutsideOrEqual|_|) (range1, range2) =
        if range1.High >= range2.High && range1.Low <= range2.Low then Some () else None

    let (|HighAndLowInsideOrEqual|_|) (range1, range2) =
        if range1.High <= range2.High && range1.Low >= range2.Low then Some () else None

    let fullyContainsRange (range1, range2) =
        match range1, range2 with
        | HighAndLowOutsideOrEqual  -> 1
        | HighAndLowInsideOrEqual   -> 1
        | _                         -> 0

    let solve input = 
        input
        |> toArray
        |> Array.map (split ",")
        |> Array.map toRangeTuple
        |> Array.map fullyContainsRange
        |> Array.sum
        |> string


    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let (|HighAndLowAbove|_|) (range1, range2) =
        if range1.High > range2.High && range1.Low > range2.High then Some () else None

    let (|HighAndLowBelow|_|) (range1, range2) =
        if range1.High < range2.Low && range1.Low < range2.Low then Some () else None

    let AnyOverlapRange (range1, range2) =
        match range1, range2 with
        | HighAndLowAbove   -> 0
        | HighAndLowBelow   -> 0
        | _                 -> 1

    let solve input = 
        input 
        |> toArray
        |> Array.map (split ",")
        |> Array.map toRangeTuple
        |> Array.map AnyOverlapRange
        |> Array.sum
        |> string
    
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };