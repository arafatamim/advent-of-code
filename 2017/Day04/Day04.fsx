(*
 * Advent of Code Day 4: High-Entropy Passphrases
 * https://adventofcode.com/2017/day/4
 *)

open System
open System.IO


let parseLine (line: string) =
    line.Trim().Split(" ")
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> List.ofArray


let rec findDuplicate =
    function
    | [] -> None
    | x :: xs when xs |> List.contains x -> Some x
    | x :: xs -> findDuplicate xs


let rec findAnagram =
    function
    | [] -> None
    | x :: xs ->
        xs
        |> List.tryPick (fun y ->
            if Seq.compareWith Operators.compare (Seq.sort x) (Seq.sort y) = 0 then
                Some(x, y)
            else
                None)
        |> function
            | None -> findAnagram xs
            | x -> x


let calc f =
    Seq.map f >> Seq.filter Option.isNone >> Seq.length


let input = File.ReadLines "input.txt" |> Seq.map parseLine

// Part I
input
|> calc findDuplicate
|> printfn "Number of passphrases without duplicate words: %d"

// Part II
input
|> calc findAnagram
|> printfn "Number of passphrases that don't have anagrams: %d"
