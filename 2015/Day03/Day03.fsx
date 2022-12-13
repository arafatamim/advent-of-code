(*
 * Advent of Code 2015 Day 3: Perfectly Spherical Houses in a Vacuum
 * https://adventofcode.com/2015/day/3
 *)

type Direction =
    | North
    | South
    | East
    | West

type PresentBearer =
    | Santa
    | Robot

let move (x, y) =
    function
    | '^' -> (x, y - 1)
    | 'v' -> (x, y + 1)
    | '>' -> (x + 1, y)
    | '<' -> (x - 1, y)
    | _ -> failwith "unhandled!"

let visitsReducer acc direction =
    match acc with
    | [] -> acc
    | (x, y) :: rest -> move (x, y) direction :: acc

let santaAndRobotVisitsReducer (santa, robot, turn) direction =
    match turn with
    | Santa -> visitsReducer santa direction, robot, Robot
    | Robot -> santa, visitsReducer robot direction, Santa

// Houses are represented by a list of coordinates
let visitedBySanta = Seq.fold visitsReducer [ (0, 0) ]

let visitedBySantaAndRobot =
    Seq.fold santaAndRobotVisitsReducer ([ (0, 0) ], [ (0, 0) ], Santa)
    >> (fun (santa, robot, _) -> santa @ robot)

let uniqueHouses = List.distinct >> List.length

let input = let file = System.IO.File.ReadAllText "input.txt" in file.Trim()

// Part I
printfn "Houses that received at least one present by Santa: %d" (input |> visitedBySanta |> uniqueHouses)

// Part II
printfn
    "Houses that received at least one present by Santa & Robo-Santa: %d"
    (input |> visitedBySantaAndRobot |> uniqueHouses)
