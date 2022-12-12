module Day07

open TaskData
open Common

open System.IO
open System

let path = Path.Combine(__SOURCE_DIRECTORY__, input)

type DepthData = {
    Depth: int
    Size: int option
    Name: string
}

let (|CdRootCommand|_|) (str: string) =
    match str.StartsWith "$ cd /" with
    | true      -> Some ()
    | _         -> None

let (|CdOutCommand|_|) (str: string) =
    match str.StartsWith "$ cd .." || str.StartsWith "$ cd \\" with
    | true      -> Some ()
    | _         -> None

let (|CdInCommand|_|) (str: string) =
    match str.StartsWith "$ cd" with
    | true      -> Some ()
    | _         -> None

let (|IsNumber|_|) (str: string) =
    match Int32.TryParse str with
    | true, n   -> Some n
    | _         -> None

let (|IsDirectory|_|) (str: string) =
    match str.StartsWith "dir " with
    | true      -> Some ()
    | _         -> None

let parse (input: string[]) =
    let mutable depth = 0
    let mutable name = "/root/"

    input
    |> Array.map (fun row -> 
        match row with
        | CdRootCommand      -> 
            depth <- 0
            None

        | CdOutCommand      ->
            let nameSplit = name.Split("/")
            let nameLength = nameSplit |> Array.length
            depth <- depth - 1
            name <- (nameSplit |> Array.take (nameLength - 2) |> join "/") + "/"
            None

        | CdInCommand       -> 
            let dirName = (row |> split " ")[2]
            depth <- depth + 1
            name <- name + dirName + "/"
            Some { Depth = depth; Size = None; Name = name }

        | _      -> 
            let split = (row |> split " ")
            match split[0] with
            | IsNumber n    -> Some { Depth = depth; Size = Some n; Name = name + split[1] }
            | _             -> None)

let folderByName (parsedInput: DepthData[]) =
    let folderName = 
        parsedInput
        |> Array.choose(fun p -> 
            match p.Size with
            | Some _    -> None
            | None      -> Some p.Name)

    (folderName, parsedInput)

let sizeByName (folderNames: string[], parsedInput: DepthData[]) =
    folderNames
    |> Array.map (fun name ->
        parsedInput
        |> Array.choose(fun p -> 
            match p.Size with
            | Some size -> if p.Name.Contains name then Some size else None
            | None      -> None))
        |> Array.map Array.sum


module Task01 =

    let solve input = 
        input
        |> toArray
        |> parse
        |> Array.choose id
        |> folderByName
        |> sizeByName
        |> Array.filter(fun s -> s <= 100000)
        |> Array.sum
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let totalDiskSpace      = 70000000
    let requiredDiskSpace   = 30000000

    let currentSize (parsedInput: DepthData[]) =
        parsedInput
        |> Array.sumBy(fun p -> 
            match p.Size with
            | Some size -> size
            | None      -> 0)
    
    let remainingToRemove (parsedInput: DepthData[]) = (totalDiskSpace - (currentSize parsedInput)) - requiredDiskSpace

    let needToRemove input =
        input
        |> toArray
        |> parse
        |> Array.choose id
        |> remainingToRemove
        |> Math.Abs

    let solve input = 
        input
        |> toArray
        |> parse
        |> Array.choose id
        |> folderByName
        |> sizeByName
        |> Array.filter(fun s -> s >= needToRemove input)
        |> Array.sort
        |> Array.head
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };