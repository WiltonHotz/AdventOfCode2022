module Day01

open TaskData
open Common

open System.IO
open System

let path = Path.Combine(__SOURCE_DIRECTORY__, "Day1.csv")

let getElves (input: string) = input.Split("\r\n\r\n")

let getTotalCaloriesfromFoods (foods: string) = 
    foods 
    |> toArray 
    |> Seq.map Int32.Parse 
    |> Seq.sum

let getTotalCaloriesPerElf elves = 
    elves 
    |> getElves 
    |> Seq.map getTotalCaloriesfromFoods


module Task01 =

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let solve input = 
        input
        |> getTotalCaloriesPerElf
        |> Seq.max
        |> string


    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let solve input = 
        input 
        |> getTotalCaloriesPerElf
        |> Seq.sortByDescending id
        |> Seq.take 3
        |> Seq.sum
        |> string

    let data = { Path = path; Solver = solve; Title = executingModule };