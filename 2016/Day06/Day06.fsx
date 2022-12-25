(*
 * Advent of Code Day 6: Signals and Noise
 * https://adventofcode.com/2016/day/6
 *)

let input = System.IO.File.ReadAllLines "input.txt"

let columnLettersByFrequency sortingFn =
    Seq.transpose
    >> Seq.map (Seq.countBy id >> sortingFn snd >> Seq.head)
    >> Seq.fold (fun acc (c, _) -> acc + string c) ""

// Part I
input
|> columnLettersByFrequency Seq.sortByDescending
|> printfn "Message formed by combining most common letters of each column: %A"

// Part II
input
|> columnLettersByFrequency Seq.sortBy
|> printfn "Message formed by combining most common letters of each column: %A"
