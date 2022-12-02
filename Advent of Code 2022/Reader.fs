module Reader

open System.IO

open TaskData

let import data =
    let input = File.ReadAllText data.Path
    (input, data.Solver, data.Title)