module Common

open Microsoft.FSharp.Quotations.Patterns

let getModuleType = function
| PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
| _ -> failwith "Expression is no property."

let formatCaller (caller: string) =
    caller.Split("+")
    |> Array.reduce (fun a b -> a + ", " + b)

let toArray (input: string) = 
    input.Split "\r\n"

let split (delim: string) (line: string) = line.Split delim