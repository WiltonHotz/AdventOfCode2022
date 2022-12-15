module Day05_Attempt2

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, input)
let originalState = Path.Combine(__SOURCE_DIRECTORY__, day05OriginalState)

let transpose numberOfPiles input =
    input
    |> toArray
    |> Array.collect (fun row -> row |> Seq.chunkBySize 4 |> Array.ofSeq)
    |> Array.map (fun slot -> 
                    match slot[1] with
                    | ' '   -> None
                    | c     -> Some c)
    |> Array.chunkBySize numberOfPiles
    |> Array.rev
    |> Array.transpose
    |> Array.map (Array.choose id)
    
let getOriginalState piles path =
    path
    |> File.ReadAllText
    |> transpose piles

let pop (array: 'T[]) =
    let lastElement = array |> Array.rev |> Array.head
    let remaining = array |> Array.rev |> Array.tail |> Array.rev
    (remaining, Some lastElement)

let push (elem: 'T option) (array: 'T[]) =
    match elem with
    | Some e    -> [|e|] |> Array.append array
    | None      -> array

let move numberOfItems source destination =
    if numberOfItems = 1 then
        let (remainingSource, lastElement) = source |> pop
        let updatedDestination = destination |> push lastElement
        (remainingSource, updatedDestination)
    else
        let (remainingSource, elements) = 
            (source |> Array.rev |> Array.skip numberOfItems |> Array.rev,
            source |> Array.rev |> Array.take numberOfItems |> Array.rev)
        let updatedDestination = elements |> Array.append destination
        (remainingSource, updatedDestination)

let updateState (state:char[][]) command =
    let (numberOfItems, source, destination) = command
    let newSource, newDestination = move numberOfItems state[source-1] state[destination-1]
    state
    |> Array.mapi(fun i a ->
        if i = source - 1 then newSource
        elif i = destination - 1 then newDestination
        else a)

let parseCommands commands =
    commands
    |> Array.fold updateState (getOriginalState 9 originalState)
    |> Array.map Array.last
    |> Seq.ofArray
    |> join ""


module Task01 =

    let flattenCommand (command: string) =
        match command |> split " " with
        | [| _; count; _; source; _; destination|] -> 
            seq {
            for x in 1.. int count do
                (1, int source, int destination)  
            }
            |> Array.ofSeq
        | _ -> failwith "translation of commands failed"

    let solve input = 
        input
        |> toArray
        |> Array.collect flattenCommand
        |> parseCommands


    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let translateCommand (command: string) =
        match command |> split " " with
        | [| _; count; _; source; _; destination|] -> (int count, int source, int destination)
        | _ -> failwith "translation of commands failed"

    let solve input = 
        input   
        |> toArray
        |> Array.map translateCommand
        |> parseCommands

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };