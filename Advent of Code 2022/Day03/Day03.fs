module Day03

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, input)

let prioritize (char: char) =
    match char |> int with
    | x when x > 96 -> x - 96
    | x             -> x - 38

module Task01 =

    let getDuplicatedChar (row: string) = 
        let middle = row.Length  / 2
        let left = row[..middle - 1] |> Seq.toList
        let right = row[middle..] |> Seq.toList
        Set.intersect (Set.ofList left) (Set.ofList right) |> Set.toArray

    let solve input = 
        input
        |> toArray
        |> Array.map getDuplicatedChar
        |> Array.collect id
        |> Array.map prioritize
        |> Array.sum
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let getCommonChar (group: string[]) =
        group
        |> seq
        |> Seq.map Set
        |> Set.intersectMany
        |> Set.toArray

    let solve input = 
        input 
        |> toArray
        |> Array.chunkBySize 3
        |> Array.map getCommonChar
        |> Array.collect id
        |> Array.map char
        |> Array.map prioritize
        |> Array.sum
        |> string
    
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };