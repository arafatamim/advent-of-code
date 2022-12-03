(*
 * Advent of Code 2022 Day 2: Rock Paper Scissors
 * https://adventofcode.com/2022/day/2
 *)

type Move =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Loss
    | Win
    | Draw

let parseMove =
    function
    | "A"
    | "X" -> Rock
    | "B"
    | "Y" -> Paper
    | "C"
    | "Z" -> Scissors
    | _ -> failwith "Unhandled case!"

let parseOutcome =
    function
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "Unhandled case!"

let calculateMove =
    function
    | (Rock, Win) -> Paper
    | (Paper, Win) -> Scissors
    | (Scissors, Win) -> Rock
    | (Rock, Loss) -> Scissors
    | (Paper, Loss) -> Rock
    | (Scissors, Loss) -> Paper
    | (x, Draw) -> x

let calculateOutcome =
    function
    | (Scissors, Rock)
    | (Paper, Scissors)
    | (Rock, Paper) -> Win
    | (x, y) when x = y -> Draw
    | _ -> Loss

let moveScore =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let outcomeScore =
    function
    | Loss -> 0
    | Draw -> 3
    | Win -> 6

module Part1 =
    ///"A X" -> (Rock, Rock)
    let parseRound (x: string) =
        let split = x.Split(" ") in (parseMove split[0], parseMove split[1])

    let calculateRoundScore round =
        moveScore (snd round) + outcomeScore (calculateOutcome round)

    let calculateTotalScore = Seq.map calculateRoundScore >> Seq.sum


module Part2 =
    ///"A X" -> (Rock, Loss)
    let parseRound (x: string) =
        let split = x.Split(" ") in (parseMove split[0], parseOutcome split[1])

    let calculateRoundScore round =
        moveScore (calculateMove round) + outcomeScore (snd round)

    let calculateTotalScore = Seq.map calculateRoundScore >> Seq.sum


let roundsSeq = System.IO.File.ReadLines "input.txt"

// Part I
printfn
    "Total player score according to player's strategy: %d"
    (roundsSeq |> Seq.map Part1.parseRound |> Part1.calculateTotalScore)

// Part II
printfn
    "Total player score according to Elf's strategy: %d"
    (roundsSeq |> Seq.map Part2.parseRound |> Part2.calculateTotalScore)
