module Common

open Microsoft.FSharp.Quotations.Patterns
open System

let getModuleType = function
| PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
| _ -> failwith "Expression is no property."

let input = "input.csv"

let day05OriginalState = "originalState.csv"

let formatCaller (caller: string) =
    caller.Split("+")
    |> Array.reduce (fun a b -> a + ", " + b)

let toArray (input: string) = 
    input.Split "\r\n"

let split (delim: string) (line: string) = line.Split delim

let join (delim : string) (items : 'T seq) = String.Join(delim, items)