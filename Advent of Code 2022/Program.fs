open TaskData
open Reader
open Common

type Task =
| Warmup2021_1_1
| Warmup2021_1_2
| AoC2022_1_1
| AoC2022_1_2
| AoC2022_2_1
| AoC2022_2_2
| AoC2022_3_1
| AoC2022_3_2
| AoC2022_4_1
| AoC2022_4_2

let from task applyTo =
    match task with
    | Warmup2021_1_1    -> Warmup.AoC2021.Day01.Task01.data
    | Warmup2021_1_2    -> Warmup.AoC2021.Day01.Task02.data
    | AoC2022_1_1       -> Day01.Task01.data
    | AoC2022_1_2       -> Day01.Task02.data
    | AoC2022_2_1       -> Day02.Task01.data
    | AoC2022_2_2       -> Day02.Task02.data
    | AoC2022_3_1       -> Day03.Task01.data
    | AoC2022_3_2       -> Day03.Task02.data
    | AoC2022_4_1       -> Day04.Task01.data
    | AoC2022_4_2       -> Day04.Task02.data
    |> fun data -> applyTo data

let solve task =
    let task, solve, title =  from task import
    solve task

let getName (item: TaskData) = getModuleType <@ item.Solver @> |> string |> formatCaller // obviously not working.

let name task = from task getName


let solveMany tasks =
    tasks
    |> List.map (fun task -> name task, solve task)
    |> List.map (fun (title, answer) -> printfn "%s: %A" (title.PadRight 31) answer)
    |> ignore


[<EntryPoint>]
let main argv =

    let tasks = [
        Warmup2021_1_1
        Warmup2021_1_2
        AoC2022_1_1
        AoC2022_1_2
        AoC2022_2_1
        AoC2022_2_2
        AoC2022_3_1
        AoC2022_3_2        
        AoC2022_4_1
        AoC2022_4_2
    ]

    solveMany tasks

    0

    // Implement many type of solvers OK
    // Implement presentation layer
    // Implement timer hof