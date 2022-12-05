module Day05

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "input.csv")
let originalState = Path.Combine(__SOURCE_DIRECTORY__, "originalState.csv")

type CargoPosition = {
    Cargo: char
    PileIndex: int
    HeightIndex: int
}

let originalStateArray path = File.ReadAllText path |> toArray

let getCargoPosition pileIndex heightIndex (slot: char[]) =
    let charSlot = slot[1]
    if charSlot <> ' ' 
    then Some { PileIndex = pileIndex + 1; HeightIndex = heightIndex; Cargo = charSlot }
    else None
        
let chunkRows rows =
    rows
    |> Array.map (fun row -> row |> seq |> Seq.chunkBySize 4 |> Array.ofSeq)

let mapCargoPosition chunkedRows =
    chunkedRows
    |> Array.mapi (fun heightIndex row -> row |> Array.mapi (fun pileIndex col -> col |> getCargoPosition pileIndex heightIndex))

type Cargo = {
    Pile1: char[]
    Pile2: char[]
    Pile3: char[]
    Pile4: char[]
    Pile5: char[]
    Pile6: char[]
    Pile7: char[]
    Pile8: char[]
    Pile9: char[]
}
        
let getCargo = 
    originalStateArray originalState 
    |> Array.rev
    |> chunkRows
    |> mapCargoPosition
    |> Array.collect (Array.choose id)
    |> fun cargo ->
        {
            Pile1 = cargo |> Array.filter (fun c -> c.PileIndex = 1) |> Array.map (fun c -> c.Cargo)
            Pile2 = cargo |> Array.filter (fun c -> c.PileIndex = 2) |> Array.map (fun c -> c.Cargo)
            Pile3 = cargo |> Array.filter (fun c -> c.PileIndex = 3) |> Array.map (fun c -> c.Cargo)
            Pile4 = cargo |> Array.filter (fun c -> c.PileIndex = 4) |> Array.map (fun c -> c.Cargo)
            Pile5 = cargo |> Array.filter (fun c -> c.PileIndex = 5) |> Array.map (fun c -> c.Cargo)
            Pile6 = cargo |> Array.filter (fun c -> c.PileIndex = 6) |> Array.map (fun c -> c.Cargo)
            Pile7 = cargo |> Array.filter (fun c -> c.PileIndex = 7) |> Array.map (fun c -> c.Cargo)
            Pile8 = cargo |> Array.filter (fun c -> c.PileIndex = 8) |> Array.map (fun c -> c.Cargo)
            Pile9 = cargo |> Array.filter (fun c -> c.PileIndex = 9) |> Array.map (fun c -> c.Cargo)
        }

let translateCommand (command: string) =
    match command |> split " " with
    | [| _; count; _; source; _; destination|] -> (int count, int source, int destination)
    | _ -> failwith "boop"

let move (source: char[], destination: char[], moveCount: int) =
    let takeIndex = source.Length - moveCount;
    let sourceCargo = source[takeIndex..]
    let sourceWithoutCargo = source |> (Seq.take takeIndex) |> Array.ofSeq
    let destinationWithCargo = 
        match sourceCargo.Length with
        | 1 -> ((sourceCargo |> Array.head) :: List.rev (destination |> List.ofArray)) |> List.rev |> Array.ofList
        | _ -> seq { destination; sourceCargo } |> Array.concat
    (sourceWithoutCargo, destinationWithCargo)

let moveManyOneAtATime (initialSource: char[], initialDestination: char[], numberOfMoves: int) =
    let rec loop (source, destination, remainingMoves) =
        match remainingMoves with
        | 0 -> (source, destination)
        | n -> 
            let (newSource, newDestination) = move (source, destination, 1)
            loop (newSource, newDestination, n - 1)
    loop (initialSource, initialDestination, numberOfMoves)

let moveManyEnsemble (initialSource: char[], initialDestination: char[], numberOfMoves: int) =
    let rec loop (source, destination, remainingMoves) =
        match remainingMoves with
        | 0 -> (source, destination)
        | n -> 
            let (newSource, newDestination) = move (source, destination, n)
            loop (newSource, newDestination, n - n)
    loop (initialSource, initialDestination, numberOfMoves)

let getPileFromSource source cargo =
    match source with
    | 1     -> cargo.Pile1
    | 2     -> cargo.Pile2
    | 3     -> cargo.Pile3
    | 4     -> cargo.Pile4
    | 5     -> cargo.Pile5
    | 6     -> cargo.Pile6
    | 7     -> cargo.Pile7
    | 8     -> cargo.Pile8
    | 9     -> cargo.Pile9
    |_      -> failwith "bloop"

let setPileFromSource source pile cargo =
    match source with
    | 1     -> { cargo with Pile1 = pile }
    | 2     -> { cargo with Pile2 = pile }
    | 3     -> { cargo with Pile3 = pile }
    | 4     -> { cargo with Pile4 = pile }
    | 5     -> { cargo with Pile5 = pile }
    | 6     -> { cargo with Pile6 = pile }
    | 7     -> { cargo with Pile7 = pile }
    | 8     -> { cargo with Pile8 = pile }
    | 9     -> { cargo with Pile9 = pile }
    | _     -> failwith "blup?"

let applyCommandsToCargo cargo commands =
    let rec loop currentcargo remainingCommands =
        match remainingCommands with
        | [] -> currentcargo
        | head::tail -> 
            let (count, source, destination) = head
            let sourcePile = currentcargo |> getPileFromSource source
            let destinationPile = currentcargo |> getPileFromSource destination
            let newSource, newDestination = moveManyOneAtATime (sourcePile, destinationPile, count)
            let updatedCargo = (currentcargo |> setPileFromSource source newSource) |> setPileFromSource destination newDestination
            loop updatedCargo tail
    loop cargo commands

let getCargoResults (cargo: Cargo) =
    [|
    cargo.Pile1[cargo.Pile1.Length - 1]
    cargo.Pile2[cargo.Pile2.Length - 1]
    cargo.Pile3[cargo.Pile3.Length - 1]
    cargo.Pile4[cargo.Pile4.Length - 1]
    cargo.Pile5[cargo.Pile5.Length - 1]
    cargo.Pile6[cargo.Pile6.Length - 1]
    cargo.Pile7[cargo.Pile7.Length - 1]
    cargo.Pile8[cargo.Pile8.Length - 1]
    cargo.Pile9[cargo.Pile9.Length - 1]
    |]
    |> List.ofArray
    |> join ""

module Task01 =

    let applyCommandsToCargo cargo commands =
        let rec loop currentcargo remainingCommands =
            match remainingCommands with
            | [] -> currentcargo
            | head::tail -> 
                let (count, source, destination) = head
                let sourcePile = currentcargo |> getPileFromSource source
                let destinationPile = currentcargo |> getPileFromSource destination
                let newSource, newDestination = moveManyOneAtATime (sourcePile, destinationPile, count)
                let updatedCargo = (currentcargo |> setPileFromSource source newSource) |> setPileFromSource destination newDestination
                loop updatedCargo tail
        loop cargo commands

    let solve input = 
        input
        |> toArray
        |> Array.map translateCommand |> List.ofArray
        |> applyCommandsToCargo getCargo
        |> getCargoResults
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let applyCommandsToCargo cargo commands =
        let rec loop currentcargo remainingCommands =
            match remainingCommands with
            | [] -> currentcargo
            | head::tail -> 
                let (count, source, destination) = head
                let sourcePile = currentcargo |> getPileFromSource source
                let destinationPile = currentcargo |> getPileFromSource destination
                let newSource, newDestination = moveManyEnsemble (sourcePile, destinationPile, count)
                let updatedCargo = (currentcargo |> setPileFromSource source newSource) |> setPileFromSource destination newDestination
                loop updatedCargo tail
        loop cargo commands

    let solve input = 
        input 
        |> toArray
        |> Array.map translateCommand |> List.ofArray
        |> applyCommandsToCargo getCargo
        |> getCargoResults
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };