#r "nuget: FParsec"

open FParsec

type Set = { Red: int; Green: int; Blue: int }

type Game = { Id: int; Sets: Set list }

// Start of parser -->

let parseSet (set: list<int * string>) =
    List.fold
        (fun acc (num, col) ->
            match col with
            | "red" -> { acc with Red = num }
            | "green" -> { acc with Green = num }
            | "blue" -> { acc with Blue = num }
            | _ -> failwith "Impossible case")
        { Red = 0; Green = 0; Blue = 0 }
        set

let pcolor = (pstring "red" <|> pstring "green" <|> pstring "blue")

let pdice = pipe2 (pint32 .>> spaces1) pcolor (fun num color -> (num, color))

let pdiceSet = sepBy pdice (pstring "," .>> spaces)

let pdiceSets = sepBy pdiceSet (pstring ";" .>> spaces)

let pgame =
    pipe2 (pstring "Game" >>. spaces >>. pint32 .>> pstring ":" .>> spaces) pdiceSets (fun id sets ->
        ({ Id = id
           Sets = List.map parseSet sets }))

let parser = pgame .>> eof

let parseLine str =
    match run parser str with
    | Success(res, _, _) -> res
    | Failure(msg, _, _) -> invalidOp msg

// <-- End of parser

let input = System.IO.File.ReadLines "input.txt"
let games = input |> Seq.map parseLine

// Part 1

let setIsPossible testSet (set: Set) =
    set.Red <= testSet.Red && set.Green <= testSet.Green && set.Blue <= testSet.Blue

let setsArePossible testSet =
    List.forall (fun set -> setIsPossible testSet set)

let choosePossibleGames testSet =
    Seq.where (fun game -> setsArePossible testSet game.Sets)

let sumGameIds = Seq.sumBy (fun game -> game.Id)

let part1 = choosePossibleGames { Red = 12; Green = 13; Blue = 14 } >> sumGameIds

part1 games |> printfn "Sum of IDs of all possible games: %A"

// Part 2

let powerOfSet
    { Red = red
      Green = green
      Blue = blue }
    =
    red * green * blue

let findMinimumSetColor state currentSet =
    ({ Red =
        if currentSet.Red > state.Red then
            currentSet.Red
        else
            state.Red
       Green =
         if currentSet.Green > state.Green then
             currentSet.Green
         else
             state.Green
       Blue =
         if currentSet.Blue > state.Blue then
             currentSet.Blue
         else
             state.Blue })

let minimumColorsInGame =
    List.fold findMinimumSetColor { Red = 0; Green = 0; Blue = 0 }

let part2 =
    Seq.map ((fun { Sets = sets } -> minimumColorsInGame sets) >> powerOfSet)
    >> Seq.sum

part2 games |> printfn "Sum of the powers of the sets: %d"
