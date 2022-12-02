module Day02

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "Day02.csv")

type Shape =
| Rock
| Paper
| Scissors

let scoreShape shape =
    match shape with
    | Rock          -> 1
    | Paper         -> 2
    | Scissors      -> 3

type Outcome =
| Win
| Draw
| Loss

let scoreOutcome outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

let playRound yourShape opponentShape =
    match yourShape, opponentShape with
    | Rock, Scissors -> Win
    | Rock, Paper -> Loss
    | Rock, Rock -> Draw
    | Paper, Rock -> Win
    | Paper, Scissors -> Loss
    | Paper, Paper -> Draw
    | Scissors, Paper -> Win
    | Scissors, Rock -> Loss
    | Scissors, Scissors -> Draw

let getRoundTotalScore shape outcome =
    let shapeScore = scoreShape shape
    let outcomeScore = scoreOutcome outcome
    shapeScore + outcomeScore

let playRoundAndGetScore (yourShape: Shape, opponentShape: Shape) =
    playRound yourShape opponentShape
    |> getRoundTotalScore yourShape

let getShapesFromLetter letter =
    match letter with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | _ -> Scissors


module Task01 =

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let getShapesFromRow (row: string) =
        let shapes = row.Split(" ")
        let yourShape = getShapesFromLetter shapes[1]
        let opponentShape = getShapesFromLetter shapes[0]
        (yourShape, opponentShape)

    let solve input = 
        input
        |> toArray
        |> Array.map getShapesFromRow
        |> Array.map playRoundAndGetScore
        |> Array.sum
        |> string


    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    type Need =
    | ToWin
    | ToDraw
    | ToLose

    let getNeedFromLetter letter =
        match letter with
        | "Z"   -> ToWin
        | "X"   -> ToLose
        | _     -> ToDraw

    let getNeededShape opponentShape need =
        match opponentShape, need with
        | Rock, ToWin -> Paper
        | Rock, ToLose -> Scissors
        | Rock, ToDraw -> Rock
        | Paper, ToWin -> Scissors
        | Paper, ToLose -> Rock
        | Paper, ToDraw -> Paper
        | Scissors, ToWin -> Rock
        | Scissors, ToLose -> Paper
        | Scissors, ToDraw -> Scissors

    let getShapesFromRow (row: string) =
        let shapesAndNeed = row.Split(" ")
        let opponentShape = getShapesFromLetter shapesAndNeed[0]
        let yourShape = 
            (getNeedFromLetter shapesAndNeed[1])
            |> getNeededShape opponentShape
        (yourShape, opponentShape)

    let solve input = 
        input 
        |> toArray
        |> Array.map getShapesFromRow
        |> Array.map playRoundAndGetScore
        |> Array.sum
        |> string

    let data = { Path = path; Solver = solve; Title = executingModule };