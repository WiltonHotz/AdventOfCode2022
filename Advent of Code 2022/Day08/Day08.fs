module Day08

open TaskData
open Common

open System.IO
open System

let path = Path.Combine(__SOURCE_DIRECTORY__, "input.csv")

let test =
    "30373" + "\r\n" +
    "25512" + "\r\n" +
    "65332" + "\r\n" +
    "33549" + "\r\n" +
    "35390"
    
type Position = {
    Value: int
    X: int
    Y: int
}

let createGrid (input: string[]) =
    input
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.mapi (fun rowIndex row ->
        row
        |>
        Array.mapi (fun colIndex value -> { Value = int (string value); X = colIndex; Y = rowIndex }))

let gridMax grid =
    let last = grid |> (Array.map Array.last) |> Array.last
    (last.X, last.Y)


let leftRightCheck position (grid: Position[][]) =
    let left  = grid[position.Y] |> Array.take position.X
    let right = grid[position.Y] |> Array.skip (position.X + 1)
    let max = gridMax grid
    if position.Y = 49 && position.X = 49
    then printf ""
    if position.X = 0 || position.Y = 0 || position.X = fst max || position.Y = snd max
    then true
    else position.Value > (left |> Array.max).Value || position.Value > (right |> Array.max).Value

let visibleFromOutside position (grid: Position[][]) = 
    grid |> leftRightCheck position ||
    Array.transpose grid |> leftRightCheck position 
    //||
    //Array.transpose (Array.transpose grid) |> leftRightCheck position ||
    //Array.transpose (Array.transpose (Array.transpose grid)) |> leftRightCheck position

let iterate (grid: Position[][]) =
    grid
    |> Array.map (fun row ->
        row 
        |> Array.sumBy (fun position -> 
            if grid |> visibleFromOutside position then 1 else 0))

module Task01 =

    let solve input = 
        input
        |> toArray
        |> createGrid
        |> iterate
        //|> Array.iter (printfn "%A")
        |> Array.sum
        |> string
        

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let solve input = 
        input

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };