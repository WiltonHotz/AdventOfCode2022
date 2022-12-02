open Reader

type Task =
| Warmup2021_1_1
| Warmup2021_1_2
| AoC2022_1_1
| AoC2022_1_2
| AoC2022_2_1
| AoC2022_2_2

let from task =
    match task with
    | Warmup2021_1_1    -> Warmup.AoC2021.Day01.Task01.data
    | Warmup2021_1_2    -> Warmup.AoC2021.Day01.Task02.data
    | AoC2022_1_1       -> Day01.Task01.data
    | AoC2022_1_2       -> Day01.Task02.data
    | AoC2022_2_1       -> Day02.Task01.data
    | AoC2022_2_2       -> Day02.Task02.data

let solve task =
    let task, solve, title = import (from task)
    solve task, title

let solveMany tasks =
    tasks
    |> List.map solve
    |> List.map (fun (answer, title) -> printfn "%s: %A" (title.PadRight 31) answer)
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
    ]

    solveMany tasks

    0

    // Implement many type of solvers OK
    // Implement presentation layer
    // Implement timer hof