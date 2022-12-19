(*
 * Advent of Code 2016 Day 1: No Time for a Taxicab
 * https://adventofcode.com/2016/day/1
 *)

#r "nuget: FParsec"

open FParsec

type Side =
    | Left
    | Right

type Direction =
    | North
    | South
    | East
    | West

    member self.Rotate =
        function
        | Right ->
            match self with
            | North -> East
            | East -> South
            | South -> West
            | West -> North
        | Left ->
            match self with
            | North -> West
            | West -> South
            | South -> East
            | East -> North


let move direction steps (x, y) =
    (match direction with
     | North -> x, y - steps
     | East -> x + steps, y
     | South -> x, y + steps
     | West -> x - steps, y)


type State =
    { Direction: Direction
      Coords: int * int }

    static member New() = { Direction = North; Coords = (0, 0) }


let parseInstructions str =
    let pStep =
        ((pchar 'R' |>> (fun _ -> Right)) <|> (pchar 'L' |>> (fun _ -> Left)))
        .>>. pint32 in

    match run (many (pStep .>> (skipString ", " <|> skipRestOfLine true))) str with
    | Success (str, _, _) -> str
    | _ -> failwith "error parsing"


let execInstruction
    ({ Direction = direction
       Coords = coords })
    =
    function
    | (dir, steps) ->
        let newDirection = direction.Rotate dir in

        { Direction = newDirection
          Coords = move newDirection steps coords }


///List.fold implementation for learning purposes
let rec applyInstructions state instructions =
    match instructions with
    | [] -> state
    | x :: xs -> applyInstructions (execInstruction state x) xs


let rec calcRecurringCoord
    ({ Direction = direction
       Coords = coords })
    visited
    target
    steps
    =
    match target with
    | None ->
        match steps with
        | [] -> coords
        | (dir, steps) :: tail ->
            let newDirection = direction.Rotate dir in
            let targetCoords = move newDirection steps coords in

            calcRecurringCoord
                { Direction = newDirection
                  Coords = coords }
                visited
                (Some targetCoords)
                tail
    | Some targetCoords ->
        let newCoords = move direction 1 coords

        if visited |> Set.contains newCoords then
            newCoords
        else
            let newVisited = visited |> Set.add newCoords in
            let newTarget = if newCoords = targetCoords then None else target in

            calcRecurringCoord
                { Direction = direction
                  Coords = newCoords }
                newVisited
                newTarget
                steps


let totalDistance (x, y) = abs x + abs y


let input = System.IO.File.ReadAllText "input.txt" |> parseInstructions

// Part I
input
|> applyInstructions (State.New())
|> (fun x -> totalDistance x.Coords)
|> printfn "Number of blocks to Easter Bunny HQ: %d"

// Part II
input
|> calcRecurringCoord (State.New()) Set.empty None
|> totalDistance
|> printfn "Distance of first location visited twice: %d"
