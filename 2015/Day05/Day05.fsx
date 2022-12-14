(*
 * Advent of Code 2015 Day 5: Doesn't He Have Intern-Elves For This?
 * https://adventofcode.com/2015/day/5
 *)

let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
let disallowed = [ [ 'a'; 'b' ]; [ 'c'; 'd' ]; [ 'p'; 'q' ]; [ 'x'; 'y' ] ]

let hasRepeatChars: string -> bool =
    Seq.scan
        (fun (last, i) char ->
            match last with
            | Some last when last = char -> Some char, i
            | _ -> Some char, (i + 1))
        (None, 0)
    >> Seq.countBy id
    >> Seq.filter (function
        | (Some char, _), n when n > 1 -> true
        | _ -> false)
    >> Seq.length
    >> (<) 0

let hasThreeVowels: string -> bool =
    Seq.fold (fun acc char -> if vowels |> List.contains char then acc + 1 else acc) 0
    >> (<=) 3

let hasDisallowedChunks: string -> bool =
    Seq.windowed 2
    >> Seq.map (fun chunk -> Seq.contains (chunk |> List.ofArray) disallowed)
    >> Seq.exists id

let hasRepeatPairs str =
    let chunks = Seq.windowed 2 str |> Seq.mapi (fun i p -> i, p) |> List.ofSeq in
    chunks
    |> List.exists (fun (i, chunk) ->
        i < chunks.Length - 2
        && List.exists (fun (_, g) -> g = chunk) chunks.[i + 2 ..])

let hasSandwichedLetter: string -> bool =
    Seq.windowed 3
    >> Seq.exists (fun chunk -> chunk.[0] = chunk.[2] && chunk.[0] <> chunk.[1])

module Part1 =
    let isNiceString str =
        hasThreeVowels str && hasRepeatChars str && not <| hasDisallowedChunks str

module Part2 =
    let isNiceString str =
        hasRepeatPairs str && hasSandwichedLetter str

let numberOfNiceStrings cond = Seq.filter cond >> Seq.length

let input = System.IO.File.ReadLines "input.txt"

printfn "Number of nice strings under old rule: %d" (input |> numberOfNiceStrings Part1.isNiceString)

printfn "Number of nice strings under new rule: %d" (input |> numberOfNiceStrings Part2.isNiceString)
