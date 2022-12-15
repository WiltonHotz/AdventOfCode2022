module Day10

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, input)

type State = {
    CurrentCycle: int
    LastFinishedCycleRegistryValue: int
    MilestoneSum: int
}

type Milestone = {
    Value: int
    Visited: bool
}

let milestones = [
    20
    60
    100
    140
    180
    220
]

let nearestMilestoneIndex ms currentCycle =
    ms
    |> List.tryFindIndex(fun m -> m > currentCycle)

let parseCommand (state, (milestones: int list)) command =
    let nearestMilestoneIndex = Option.defaultValue (milestones.Length - 1) (nearestMilestoneIndex milestones state.CurrentCycle)
    let ms = milestones |> List.skip nearestMilestoneIndex
    if ms = [] then state, ms 
    else 
        let nearestMilestone = ms |> List.head

        if command = "noop" then
            if state.CurrentCycle = nearestMilestone || state.CurrentCycle = nearestMilestone - 1 then
                { 
                    state with 
                        MilestoneSum = state.LastFinishedCycleRegistryValue * nearestMilestone + state.MilestoneSum
                        CurrentCycle = state.CurrentCycle + 1 
                },
                ms |> List.skip 1
            else { state with CurrentCycle = state.CurrentCycle + 1 }, ms
        else 
            let regVal =
                match command |> split " " with
                | [| _; regValue |] -> int regValue
                | _                 -> failwith "unknown regvalue"
            if state.CurrentCycle = nearestMilestone || state.CurrentCycle = nearestMilestone - 1 || state.CurrentCycle = nearestMilestone - 2 then
                {
                    state with
                        MilestoneSum = state.LastFinishedCycleRegistryValue * nearestMilestone + state.MilestoneSum
                        CurrentCycle = state.CurrentCycle + 2
                        LastFinishedCycleRegistryValue = state.LastFinishedCycleRegistryValue + regVal
                },
                ms |> List.skip 1
            else { state with CurrentCycle = state.CurrentCycle + 2; LastFinishedCycleRegistryValue = state.LastFinishedCycleRegistryValue + regVal }, ms



module Task01 =

    let sumState input = 
        let commands = input |> toArray
        let initialState = { CurrentCycle = 0; LastFinishedCycleRegistryValue = 1; MilestoneSum = 0 }
        let finalState, empty = commands |> Array.fold parseCommand (initialState, milestones)
        finalState.MilestoneSum |> string

    let solve input = 
        input
        |> sumState
        
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let flattenMoves input =
        input
        |> toArray
        |> Array.map (fun row ->
            if row = "noop" then [None]
            else
                match row |> split " " with
                | [| _; regVal |]   -> 
                    seq {
                        None
                        Some regVal
                    } |> List.ofSeq
                | err -> failwith $"Unparsable Row: {err}")
        |> List.ofArray
        |> List.collect id

    let parseLine (index, currentRegistry, drawing) (line: string option) =
        let currentRegistryRange = [currentRegistry - 1..currentRegistry + 1]
        let overlapping = currentRegistryRange |> List.contains(index % 40)
        let ch = if overlapping then '#' else '.'

        match line with
        | Some v -> (currentRegistry + int v)  
        | None -> currentRegistry
        |> fun cr -> index + 1, cr, ch :: drawing

    let print (index, currentRegistry, (line: char list)) =
        let revLine = line |> List.rev
        for y in 0..5 do
            for x in 0..39 do
                printf "%c" revLine[x + y * 40]
            printfn ""
        line
            
    let solve input = 
        input
        |> flattenMoves
        |> List.fold parseLine (0, 1, List.empty<char>)
        |> print
        |> string
            
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };
