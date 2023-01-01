(*
 * Advent of Code 2018 Day 2: Inventory Management System
 * https://adventofcode.com/2018/day/2
 *)

let input = System.IO.File.ReadLines "testinput2.txt"

let (>>*) f g x = f x * g x

let findLetter count =
    Seq.tryFind (fun (_, xs) -> Seq.length xs = count) >> Option.map fst

let countRepeatChars count =
    Seq.choose (findLetter count) >> Seq.length

let calcChecksum = countRepeatChars 2 >>* countRepeatChars 3

// Part I
input
|> Seq.map (Seq.groupBy id)
|> calcChecksum
|> printfn "Checksum for list of IDs: %d"
