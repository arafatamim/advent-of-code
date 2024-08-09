(*
 * Advent of Code 2023 Day 1: Trebuchet?!
 * https://adventofcode.com/2023/day/1
 *)

open System

let inline charToInt c = int c - int '0'

let concatInts (a, b) = int (string a + string b)

let keepFirstAndLast seq = Seq.head seq, Seq.last seq

let extractDigits = Seq.filter Char.IsDigit >> Seq.map charToInt

let extractWordsAndDigits (text: string) =
    let numberDict =
        [ ("1", 1)
          ("2", 2)
          ("3", 3)
          ("4", 4)
          ("5", 5)
          ("6", 6)
          ("7", 7)
          ("8", 8)
          ("9", 9)
          ("one", 1)
          ("two", 2)
          ("three", 3)
          ("four", 4)
          ("five", 5)
          ("six", 6)
          ("seven", 7)
          ("eight", 8)
          ("nine", 9) ]

    let rec processLines acc (line: string) =
        match line with
        | "" -> acc |> List.rev
        | line ->
            let digit = numberDict |> List.tryFind (fun (word, _) -> line.StartsWith(word))

            match digit with
            | Some(_, digit) ->
                let rest = line.Substring(1)
                processLines (digit :: acc) rest
            | None -> processLines acc (line.Substring(1))

    processLines [] text

let concatFirstAndLast = keepFirstAndLast >> concatInts

let part1 = extractDigits >> concatFirstAndLast

let part2 = extractWordsAndDigits >> concatFirstAndLast

let sumLines fn = Seq.map fn >> Seq.sum

let input = System.IO.File.ReadLines "input.txt"

input |> sumLines part1 |> printfn "Part 1: %d"
input |> sumLines part2 |> printfn "Part 2: %d"
