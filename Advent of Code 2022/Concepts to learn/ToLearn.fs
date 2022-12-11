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

*)

// FOLD EXAMPLE 1:

// What kind of answer am I expecting? a count(of positive numbers) and a sum (of negative numbers), both integers, from a list
// let (count, sum) = ...

// give a starting value
//let (count, sum) = (0, 0)

// lets not destructure
let initial = (0,0)

// update answer based on a single input value
let update (count, sum) value =
    if value > 0 then
        (count + 1, sum)
    elif value = 0 then
        (count, sum)
    else
        (count, sum + value)

// process using input list
let runChallenge input =
    input
    |> List.fold update initial

// try it out
let input = [1;2;3;4;5;6;7;8;9;10;-11;-12;-13;-14;-15]
let (count, sum) = runChallenge input
printfn "Count: %i, Sum: %i" count sum

// FOLD EXAMPLE 2: (which ends up just being a List.map - example)

// lets try doubling every even number smaller than 10 in a list

let updateElement f list = 
    list 
    |> List.mapi (fun i value -> 
        if i < 10 && value < 10 && value % 2 = 0 then 
            f value 
        else 
            value)

let newList = input |> updateElement (fun a -> 2 * a)

newList |> List.iter (fun a -> printf "%A " a)

// lets do this for three lists
let dinput = input |> List.rev
let pinput = input @ dinput
let biginput = [input; dinput; pinput]

let ue = updateElement (fun a -> 2 * a)

let run input =
    input
    |> List.map (fun l -> l |> ue)

run biginput |> ignore



(*

Assign actions from a DU and then later match on actions
https://github.com/ijrussell/AdventOfCode2022/blob/main/Code/Day07/code.fsx

More regex matching, and everything here really
https://github.com/jindraivanek/adventofcode2022/blob/master/day7.fsx

Array2D
https://github.com/jindraivanek/adventofcode2022/blob/master/day8.fsx

List.exists id

Seq.tryFindIndex



*)