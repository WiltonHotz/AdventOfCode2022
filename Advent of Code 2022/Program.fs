open TaskData
open Reader
open Common
open System.IO

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
| AoC2022_5_1
| AoC2022_5_2
| AoC2022_5_Attempt2_1
| AoC2022_5_Attempt2_2
| AoC2022_6_1
| AoC2022_6_2
| AoC2022_7_1
| AoC2022_7_2
| AoC2022_8_1
| AoC2022_8_2
| AoC2022_9_1
| AoC2022_9_2
| AoC2022_10_1
| AoC2022_10_2
| AoC2022_11_1
| AoC2022_11_2
| AoC2022_12_1
| AoC2022_12_2
| AoC2022_13_1
| AoC2022_13_2

let from task applyTo =
    match task with
    | Warmup2021_1_1        -> Warmup.AoC2021.Day01.Task01.data
    | Warmup2021_1_2        -> Warmup.AoC2021.Day01.Task02.data
    | AoC2022_1_1           -> Day01.Task01.data
    | AoC2022_1_2           -> Day01.Task02.data
    | AoC2022_2_1           -> Day02.Task01.data
    | AoC2022_2_2           -> Day02.Task02.data
    | AoC2022_3_1           -> Day03.Task01.data
    | AoC2022_3_2           -> Day03.Task02.data
    | AoC2022_4_1           -> Day04.Task01.data
    | AoC2022_4_2           -> Day04.Task02.data
    | AoC2022_5_1           -> Day05.Task01.data
    | AoC2022_5_2           -> Day05.Task02.data
    | AoC2022_5_Attempt2_1  -> Day05_Attempt2.Task01.data
    | AoC2022_5_Attempt2_2  -> Day05_Attempt2.Task02.data
    | AoC2022_6_1           -> Day06.Task01.data
    | AoC2022_6_2           -> Day06.Task02.data
    | AoC2022_7_1           -> Day07.Task01.data
    | AoC2022_7_2           -> Day07.Task02.data
    | AoC2022_8_1           -> Day08.Task01.data
    | AoC2022_8_2           -> Day08.Task02.data
    | AoC2022_9_1           -> Day09.Task01.data
    | AoC2022_9_2           -> Day09.Task02.data
    | AoC2022_10_1          -> Day10.Task01.data
    | AoC2022_10_2          -> Day10.Task02.data
    | AoC2022_11_1          -> Day11.Task01.data
    | AoC2022_11_2          -> Day11.Task02.data
    | AoC2022_12_1          -> Day12.Task01.data
    | AoC2022_12_2          -> Day12.Task02.data
    | AoC2022_13_1          -> Day13.Task01.data
    | AoC2022_13_2          -> Day13.Task02.data

    |> fun data -> applyTo data

let solve task =
    let task, solve, title =  from task import
    solve task, title

let getName (item: TaskData) = getModuleType <@ item.Solver @> |> string |> formatCaller // obviously not working.

let name task = from task getName


let solveMany tasks =
    tasks
    |> List.map (fun task -> solve task) //name task, solve task)
    |> List.map (fun (answer, title) -> printfn "%s: %A" (title.PadRight 31) answer)
    |> ignore


[<EntryPoint>]
let main argv =

    let tasks = [
        //Warmup2021_1_1
        //Warmup2021_1_2
        //AoC2022_1_1
        //AoC2022_1_2
        //AoC2022_2_1
        //AoC2022_2_2
        //AoC2022_3_1
        //AoC2022_3_2        
        //AoC2022_4_1
        //AoC2022_4_2
        //AoC2022_5_1
        //AoC2022_5_2
        AoC2022_5_Attempt2_1
        AoC2022_5_Attempt2_2
        //AoC2022_6_1
        //AoC2022_6_2
        //AoC2022_7_1
        //AoC2022_7_2
        //AoC2022_8_1
        //AoC2022_8_2
        //AoC2022_9_1
        //AoC2022_9_2
        //AoC2022_10_1
        //AoC2022_10_2
        //AoC2022_11_1
        //AoC2022_11_2
        //AoC2022_12_1
        //AoC2022_12_2
        //AoC2022_13_1
        //AoC2022_13_2
    ]

    solveMany tasks

    0

    // Implement many type of solvers OK
    // Implement presentation layer
    // Implement timer hof