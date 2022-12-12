module Day09

open TaskData
open Common

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, input)

type HeadMovement =
| U
| D
| L
| R

type HeadRelativePosition =
| Overlapping
| Above
| Below
| Left
| Right
| UpDiagonalLeft
| UpDiagonalRight
| DownDiagonalLeft
| DownDiagonalRight

type TailMovement = 
| MoveUp
| MoveLeft
| MoveDown
| MoveRight 
| MoveUpDiagonalLeft
| MoveUpDiagonalRight
| MoveDownDiagonalLeft
| MoveDownDiagonalRight
| DontMove

let given (headmovement: HeadMovement) headRelativePosition =
    match headRelativePosition with
    | Overlapping   ->
        match headmovement with
        | U         -> (Above, DontMove)
        | D         -> (Below, DontMove)
        | L         -> (Left, DontMove)
        | R         -> (Right, DontMove)
    | Above         ->
        match headmovement with
        | U         -> (Above, MoveUp)
        | D         -> (Overlapping, DontMove)
        | L         -> (UpDiagonalLeft, DontMove)
        | R         -> (UpDiagonalRight, DontMove)
    | Below         ->
        match headmovement with
        | U         -> (Overlapping, DontMove)
        | D         -> (Below, MoveDown)
        | L         -> (DownDiagonalLeft, DontMove)
        | R         -> (DownDiagonalRight, DontMove)
    | Left          ->
        match headmovement with
        | U         -> (UpDiagonalLeft, DontMove)
        | D         -> (DownDiagonalLeft, DontMove)
        | L         -> (Left, MoveLeft)
        | R         -> (Overlapping, DontMove)
    | Right          ->
        match headmovement with
        | U         -> (UpDiagonalRight, DontMove)
        | D         -> (DownDiagonalRight, DontMove)
        | L         -> (Overlapping, DontMove)
        | R         -> (Right, MoveRight)
    | UpDiagonalLeft          ->
        match headmovement with
        | U         -> (Above, MoveUpDiagonalLeft)
        | D         -> (Left, DontMove)
        | L         -> (Left, MoveUpDiagonalLeft)
        | R         -> (Above, DontMove)
    | UpDiagonalRight          ->
        match headmovement with
        | U         -> (Above, MoveUpDiagonalRight)
        | D         -> (Right, DontMove)
        | L         -> (Above, DontMove)
        | R         -> (Right, MoveUpDiagonalRight)
    | DownDiagonalLeft         ->
        match headmovement with
        | U         -> (Left, DontMove)
        | D         -> (Below, MoveDownDiagonalLeft)
        | L         -> (Left, MoveDownDiagonalLeft)
        | R         -> (Below, DontMove)
    | DownDiagonalRight         ->
        match headmovement with
        | U         -> (Right, DontMove)
        | D         -> (Below, MoveDownDiagonalRight)
        | L         -> (Below, DontMove)
        | R         -> (Right, MoveDownDiagonalRight)

let parseHeadMovement letter =
    match letter with
    | "U"           -> U
    | "D"           -> D
    | "L"           -> L
    | "R"           -> R
    | err           -> failwith $"Unparsable input: {err}"

let flattenMoves input =
    input
    |> toArray
    |> Array.map (fun row ->
        match row |> split " " with
        | [| letter; numberOfSteps |]   -> 
            Array.create (int numberOfSteps) (parseHeadMovement letter)
        | err                           -> 
            failwith $"Unparsable Row: {err}")
    |> Array.collect id

type Position = {
    TailVisitedNumberOfTimes: int
}

let counter state headMovement =
    let up, down, left, right = state
    match headMovement with
    | U -> (up + 1, down, left, right)
    | D -> (up, down + 1, left, right)
    | L -> (up, down, left + 1, right)
    | R -> (up, down, left, right + 1)

// create grid based on Width: L + R, and Height: D + U, and Starting square: +1
let createGrid (input: HeadMovement[]) =
    let (ups, downs, lefts, rights) = input |> Array.fold counter (0,0,0,0)
    let grid = Array2D.create (lefts + rights + 1) (ups + downs + 1) { TailVisitedNumberOfTimes = 0 }

    let xStart = (lefts + rights - ([lefts; rights] |> List.max))
    let yStart = (ups + downs - ([ups; downs] |> List.min))
    grid[yStart, xStart] <- { TailVisitedNumberOfTimes = 1 }

    (yStart, xStart), grid, input
    

module Task01 =

    let step (currentTailPosition, (grid: Position[,]), headRelativePosition) headMovement =
        let y, x = currentTailPosition
        let newHeadRelativePosition, tailMovement = headRelativePosition |> given headMovement

        let newX, newY = 
            match tailMovement with
            | MoveUp                -> (0, -1)
            | MoveLeft              -> (-1, 0)
            | MoveDown              -> (0, 1)
            | MoveRight             -> (1, 0)
            | MoveUpDiagonalLeft    -> (-1, -1)
            | MoveUpDiagonalRight   -> (1, -1)
            | MoveDownDiagonalLeft  -> (-1, 1)
            | MoveDownDiagonalRight -> (1, 1)
            | DontMove              -> (0, 0)
            |> fun (xChange, yChange) -> xChange + x, yChange + y

        let tailvisitedCount = grid[newY, newX].TailVisitedNumberOfTimes
        grid[newY, newX] <- { TailVisitedNumberOfTimes = tailvisitedCount + (if (newX, newY) <> (x, y) then 1 else 0) }

        ((newY, newX), grid, newHeadRelativePosition)

    let play (startingPosition, grid, headMovements) =
        let (finalPosition, finalGrid, lastHeadRelativePosition) = 
            headMovements 
            |> Array.fold step (startingPosition, grid, Overlapping)

        finalGrid

    let sumVisitedPositions (grid: Position[,]) =
        grid
        |> Array2D.map(fun p -> if p.TailVisitedNumberOfTimes > 0 then 1 else 0)
        |> Seq.cast<int>
        |> Seq.sum

    let solve input = 
        input
        |> flattenMoves
        |> createGrid
        |> play 
        |> sumVisitedPositions
        |> string
        
    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };


module Task02 =

    let step (currentT0Position, currentTailPosition, (grid: Position[,]), (tailRelativePositions: HeadRelativePosition list), headRelativePosition) headMovement =
        let y, x = currentT0Position

        let newHeadRelativePosition, tailMovement = headRelativePosition |> given headMovement

        if headMovement = L then
            printf ""

        let newTailZeroX, newTailZeroY = 
            match tailMovement with
            | MoveUp                -> (0, -1)
            | MoveLeft              -> (-1, 0)
            | MoveDown              -> (0, 1)
            | MoveRight             -> (1, 0)
            | MoveUpDiagonalLeft    -> (-1, -1)
            | MoveUpDiagonalRight   -> (1, -1)
            | MoveDownDiagonalLeft  -> (-1, 1)
            | MoveDownDiagonalRight -> (1, 1)
            | DontMove              -> (0, 0)
            |> fun (xChange, yChange) -> xChange + x, yChange + y

        let lastTailMovement, newTailRelativePositions = 
            ((tailMovement, List.empty<HeadRelativePosition>), tailRelativePositions)
            ||> List.fold (fun (currentTailMovement, newTailRelativePositions) currentTailRelativePosition ->
                let nextTailRelativePosition, nextTailMovement =
                    match currentTailMovement, currentTailRelativePosition with
                    | MoveUp,                   Above               -> Above, MoveUp
                    | MoveUp,                   UpDiagonalLeft      -> Above, MoveUpDiagonalLeft
                    | MoveUp,                   UpDiagonalRight     -> Above, MoveUpDiagonalRight
                    | MoveUp,                   _                   -> currentTailRelativePosition |> given U
                    | MoveLeft,                 Left                -> Left, MoveLeft
                    | MoveLeft,                 UpDiagonalLeft      -> Left, MoveUpDiagonalLeft
                    | MoveLeft,                 DownDiagonalLeft    -> Left, MoveDownDiagonalLeft
                    | MoveLeft,                 _                   -> currentTailRelativePosition |> given L
                    | MoveDown,                 Below               -> Below, MoveDown
                    | MoveDown,                 DownDiagonalLeft    -> Below, MoveDownDiagonalLeft
                    | MoveDown,                 DownDiagonalRight   -> Below, MoveDownDiagonalRight
                    | MoveDown,                 _                   -> currentTailRelativePosition |> given D
                    | MoveRight,                Right               -> Right, MoveRight
                    | MoveRight,                UpDiagonalRight     -> Right, MoveUpDiagonalRight
                    | MoveRight,                DownDiagonalRight   -> Right, MoveDownDiagonalRight
                    | MoveRight,                _                   -> currentTailRelativePosition |> given R
                    | MoveUpDiagonalRight,      Above               -> fst (currentTailRelativePosition |> given R) |> given U
                    | MoveUpDiagonalRight,      UpDiagonalLeft      -> fst (currentTailRelativePosition |> given R) |> given U
                    | MoveUpDiagonalRight,      UpDiagonalRight     -> UpDiagonalRight, MoveUpDiagonalRight
                    | MoveUpDiagonalRight,      _                   -> fst (currentTailRelativePosition |> given U) |> given R
                    | MoveUpDiagonalLeft,       Above               -> fst (currentTailRelativePosition |> given L) |> given U
                    | MoveUpDiagonalLeft,       UpDiagonalRight     -> fst (currentTailRelativePosition |> given L) |> given U
                    | MoveUpDiagonalLeft,       UpDiagonalLeft      -> UpDiagonalLeft, MoveUpDiagonalLeft
                    | MoveUpDiagonalLeft,       _                   -> fst (currentTailRelativePosition |> given U) |> given L
                    | MoveDownDiagonalLeft,     Below               -> fst (currentTailRelativePosition |> given L) |> given D
                    | MoveDownDiagonalLeft,     DownDiagonalRight   -> fst (currentTailRelativePosition |> given L) |> given D
                    | MoveDownDiagonalLeft,     DownDiagonalLeft    -> DownDiagonalLeft, MoveDownDiagonalLeft
                    | MoveDownDiagonalLeft,     _                   -> fst (currentTailRelativePosition |> given D) |> given L
                    | MoveDownDiagonalRight,    Below               -> fst (currentTailRelativePosition |> given R) |> given D
                    | MoveDownDiagonalRight,    DownDiagonalLeft    -> fst (currentTailRelativePosition |> given R) |> given D
                    | MoveDownDiagonalRight,    DownDiagonalRight   -> DownDiagonalRight, MoveDownDiagonalRight
                    | MoveDownDiagonalRight,    _                   -> fst (currentTailRelativePosition |> given D) |> given R
                    | DontMove,                 _                   -> currentTailRelativePosition, DontMove

                nextTailMovement, nextTailRelativePosition::newTailRelativePositions
            )
 
        let tailY, tailX = currentTailPosition
        let lastTailZeroX, lastTailZeroY = 
            match lastTailMovement with
            | MoveUp                -> (0, -1)
            | MoveLeft              -> (-1, 0)
            | MoveDown              -> (0, 1)
            | MoveRight             -> (1, 0)
            | MoveUpDiagonalLeft    -> (-1, -1)
            | MoveUpDiagonalRight   -> (1, -1)
            | MoveDownDiagonalLeft  -> (-1, 1)
            | MoveDownDiagonalRight -> (1, 1)
            | DontMove              -> (0, 0)
            |> fun (xChange, yChange) -> xChange + tailX, yChange + tailY

        let tailvisitedCount = grid[lastTailZeroY, lastTailZeroX].TailVisitedNumberOfTimes
        grid[lastTailZeroY, lastTailZeroX] <- { TailVisitedNumberOfTimes = tailvisitedCount + (if (lastTailZeroX, lastTailZeroY) <> (tailX, tailY) then 1 else 0) }

        ((newTailZeroY, newTailZeroX), (lastTailZeroY, lastTailZeroX), grid, newTailRelativePositions |> List.rev, newHeadRelativePosition)

    let play (startingPosition, grid, (headMovements: HeadMovement[])) =
        let (finalPosition, finalTailPosition, finalGrid, finalTailPositions, lastHeadRelativePosition) = 
            headMovements 
            |> Array.fold step (startingPosition, startingPosition, grid, (Array.create 8 Overlapping |> List.ofArray), Overlapping)

        finalGrid

    let sumVisitedPositions (grid: Position[,]) =
        grid
        |> Array2D.map(fun p -> if p.TailVisitedNumberOfTimes > 0 then 1 else 0)
        |> Seq.cast<int>
        |> Seq.sum

    let solve input = 
        input
        |> flattenMoves
        |> createGrid
        |> play 
        |> sumVisitedPositions
        |> string

    let rec private executingModule = getModuleType <@ executingModule @> |> string |> formatCaller

    let data = { Path = path; Solver = solve; Title = executingModule };