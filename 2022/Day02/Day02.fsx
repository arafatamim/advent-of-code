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
    let parseRound: seq<string> -> seq<Move * Move> =
        Seq.map (fun x -> let split = x.Split(" ") in (parseMove split[0], parseMove split[1]))

    let calculateRoundScore =
        Seq.map (fun round ->
            let outcome = calculateOutcome round in
            let outcomeScore = outcomeScore outcome in
            let moveScore = moveScore (snd round) in
            moveScore + outcomeScore)

    let calculateTotalScore = calculateRoundScore >> Seq.sum


module Part2 =
    ///"A X" -> (Rock, Loss)
    let parseRound: seq<string> -> seq<Move * Outcome> =
        Seq.map (fun x -> let split = x.Split(" ") in (parseMove split[0], parseOutcome split[1]))

    let calculateRoundScore =
        Seq.map (fun round ->
            let move = calculateMove round in
            let moveScore = moveScore move in
            let outcomeScore = outcomeScore (snd round) in
            moveScore + outcomeScore)

    let calculateTotalScore = calculateRoundScore >> Seq.sum


let input = System.IO.File.ReadLines "input.txt" |> List.ofSeq

// Part I
printfn "Total player score according to player's strategy: %d" (Part1.calculateTotalScore <| Part1.parseRound input)

// Part II
printfn "Total player score according to Elf's strategy: %d" (Part2.calculateTotalScore <| Part2.parseRound input)
