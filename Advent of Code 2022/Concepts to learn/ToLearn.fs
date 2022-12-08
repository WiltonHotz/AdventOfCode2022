module ToLearn

// Ideas picked up from other Advent of Code - practicioners that I'd like to learn more about

(*
Seq.sumBy is essentially Seq.map + Seq.sum 

Seq.head to get the or first item from seq 

Compose (>>) can be used (experiment) 

function can be used in matching 

folder methods can be used to aggregate state (see saved image) 

Seq.Take + Seq.Skip for array ranges 

Seq.collect fn, to perform an operation of a subset and return to top layer. (!!) 

Figure out what ||> does here:
https://github.com/jindraivanek/adventofcode2022/blob/master/day3.fsx 

Figure out what array <_> does here:
https://github.com/jindraivanek/adventofcode2022/blob/master/day4.fsx 

Multiple DU choices to one match result instead of matching every case explicitly 

Array pattern matchings 

Set.isSuperset figure out 

seq { int from .. int until } |> Set.ofSeq 

Consider error handling when parsing data 

Seq.choose fn instead of Seq.map + match 

match input.Split("-") with 
| [|start;finish|] -> Set [int start..int finish] 
| _ -> failwith "Invalid part" 

Set.isSubset 

Set.empty 

Investigate calculate method and Seq.choose + failwith:
https://github.com/ijrussell/AdventOfCode2022/blob/main/Code/Day04/code.fsx 

let getPriority (input:char) = ['a'..'z'] @ ['A'..'Z'] 
|> List.findIndex (fun c -> c = input) 
|> (+) 1
https://github.com/ijrussell/AdventOfCode2022/blob/main/Code/Day03/code.fsx 

Tests for the example data:
https://github.com/tkshill/AdventOfCode2022/tree/main/AOC2022.Tests 

Regex
https://github.com/jindraivanek/adventofcode2022/blob/master/day5.fsx
And
https://github.com/ijrussell/AdventOfCode2022/blob/main/Code/Day05/code.fsx 

Array.transpose 

Stack 

Seq.windowed 

Seq.indexed 

Seq.find 

Seq.findIndex 

|> ( + ) 


Fold 

let updateElement key f st = st |> Array.mapi (fun i p -> if i = key then f p else p) 

let st = [("a1",(100,10)); ("a2",(50,20)); ("a3",(25,40))] 
st |> updateElement source (fun s -> pop) 

Or
Get both source and destination in and move 

let (state: char [][]) = originalState
let update state value =
   let (count, source, destination) = value

*)