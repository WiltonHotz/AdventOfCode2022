module Day12

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, input)
    

module Task01 =

    let solve input = 
        input
        
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let solve input = 
        input

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };