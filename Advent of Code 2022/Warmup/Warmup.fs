module Warmup

open System.IO
open System

open TaskData
open Common

module AoC2021 =

    module Day01 =

        let path = Path.Combine(__SOURCE_DIRECTORY__, "2021_Day01.csv")


        module Task01 =

            let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

            let solve input = 
                input 
                |> toArray
                |> Array.map Int32.Parse
                |> Array.pairwise 
                |> Array.filter(fun (a,b) -> b > a) 
                |> Array.length
                |> string

            let data = { Path = path; Solver = solve; Title = executingModule };


        module Task02 =

            let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

            let solve input =
                input
                |> toArray
                |> Array.map Int32.Parse
                |> Array.pairwise 
                |> Array.chunkBySize 2 
                |> Array.collect id 
                |> Array.pairwise 
                |> Array.map(fun ((a,_),(c,d)) -> [| a; c; d |])
                |> Array.map(Array.sum)
                |> Array.pairwise
                |> Array.filter(fun (a,b) -> b > a) 
                |> Array.length
                |> string

            let data = { Path = path; Solver = solve; Title = executingModule };