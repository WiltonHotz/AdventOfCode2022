module Day08

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "input.csv")

type Position = {
    Value: int
    X: int
    Y: int
}

let createGrid (input: string[]) =
    input
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.mapi (fun rowIndex row ->
        row |> Array.mapi (fun colIndex value -> 
            { Value = int (string value); X = colIndex; Y = rowIndex }))

let gridMax grid =
    let last = grid |> (Array.map Array.last) |> Array.last
    (last.X, last.Y)

let edgePosition position grid = 
    let max = gridMax grid
    position.X = 0 || position.Y = 0 || position.X = fst max || position.Y = snd max


module Task01 =

    let leftRightCheck position (grid: Position[][]) =
        let left  = grid[position.Y] |> Array.take position.X
        let right = grid[position.Y] |> Array.skip (position.X + 1)
        if grid |> edgePosition position
        then true
        else position.Value > (left |> Array.max).Value || position.Value > (right |> Array.max).Value

    let visibleFromOutside position (grid: Position[][]) = 
        let transposed = (Array.transpose grid)
        grid |> leftRightCheck position || transposed |> leftRightCheck { position with X = position.Y; Y = position.X }

    let iterate (grid: Position[][]) =
        grid
        |> Array.map (fun row ->
            row 
            |> Array.map (fun position -> 
                if grid |> visibleFromOutside position then 1 else 0))

    let solve input = 
        input
        |> toArray
        |> createGrid
        |> iterate
        |> Array.sumBy (Array.reduce (+))
        |> string
        
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let leftRightCheck position (grid: Position[][]) =
        let left  = grid[position.Y] |> Array.take position.X
        let right = grid[position.Y] |> Array.skip (position.X + 1)
        if grid |> edgePosition position
        then 0
        else 
            let leftUntilBlocked = left |> Array.rev |> Array.takeWhile (fun tree -> position.Value > tree.Value) 
            let leftLength = leftUntilBlocked |> Array.length
            let leftLastIsEdge = if leftLength = 0 then true else grid |> edgePosition (leftUntilBlocked |> Array.last)

            let rightUntilBlocked = right |> Array.takeWhile (fun tree -> position.Value > tree.Value)
            let rightLength = rightUntilBlocked |> Array.length
            let rightLastIsEdge = if rightLength = 0 then true else grid |> edgePosition (rightUntilBlocked |> Array.last)
            
            ((leftLength + (if leftLastIsEdge then 0 else 1)) * (rightLength + (if rightLastIsEdge then 0 else 1)))

    let visibleFromInside position (grid: Position[][]) = 
        let transposed = (Array.transpose grid)
        (grid |> leftRightCheck position) * (transposed |> leftRightCheck { position with X = position.Y; Y = position.X })

    let iterate (grid: Position[][]) =
        grid
        |> Array.map (fun row ->
            row 
            |> Array.map (fun position -> grid |> visibleFromInside position))

    let solve input = 
        input
        |> toArray
        |> createGrid
        |> iterate
        |> Array.collect id
        |> Array.max
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };