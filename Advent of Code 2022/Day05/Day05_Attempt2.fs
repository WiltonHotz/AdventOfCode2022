module Day05_Attempt2

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "input.csv")
let originalState = Path.Combine(__SOURCE_DIRECTORY__, "originalState.csv")

let test = 
    "    [D]    " + 
    "\r\n" + 
    "[N] [C]    " + 
    "\r\n" + 
    "[Z] [M] [P]"

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
    
let getOriginalState path =
    path
    |> File.ReadAllText
    |> transpose 3

type State = {
    CurrentState: char[][]
}

let pop (array: 'T[]) =
    let lastElement = array |> Array.rev |> Array.head
    let remaining = array |> Array.rev |> Array.tail
    (remaining, lastElement)

let push (elem: 'T) (array: 'T[]) =
    array |> Array.append [|elem|]

let move source destination =
    let (remainingSource, lastElement) = source |> pop
    let updatedDestination = destination |> push lastElement
    (remainingSource, updatedDestination)

let moveMany numberOfItems source destination =
    let (remainingSource, elements) = (source |> Array.take numberOfItems, source |> Array.skip numberOfItems)
    let updatedDestination = destination |> Array.append elements
    (remainingSource, updatedDestination)

let translateCommand (command: string) =
    match command |> split " " with
    | [| _; count; _; source; _; destination|] -> (int count, int source, int destination)
    | _ -> failwith "translation of commands failed"

//let updateStateByCommand command (state: State) =
//    let (count, source, destination) = command
//    for x in 1..count do
        
        

//let foldCommands (state: State) commands =
//    (state, commands |> Array.map translateCommand)
//    ||> Array.fold (fun s (count, source, destination) ->
//        s.CurrentState
//        |> Array.map)
        

module Task01 =

    let solve input = 
        input

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let solve input = 
        input 

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };