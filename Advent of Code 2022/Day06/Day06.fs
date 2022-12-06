module Day06

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "input.csv")

let partition setLength (str: string) =
    let arr =
        str
        |> Seq.toList
    seq {
        for n in 0..(arr.Length - setLength) - 1 do
            let set =
                arr
                |> List.skip n |> List.take setLength
                |> Set
            (set, n)
        } 
        |> Seq.filter (fun (s, n) -> s.Count = setLength)
        |> Seq.map (fun (_, n) -> n + setLength)
        |> Seq.head


module Task01 =

    let solve input = 
        input
        |> partition 4
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let solve input = 
        input 
        |> partition 14
        |> string
    
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };