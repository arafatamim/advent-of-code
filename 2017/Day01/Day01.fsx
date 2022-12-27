(*
 * Advent of Code 2017 Day 1: Inverse Captcha
 * https://adventofcode.com/2017/day/1
 *)

let input = (System.IO.File.ReadAllText "input.txt").Trim()

// Part I
input + string input[0]
|> Seq.pairwise
|> Seq.fold (fun sum (a, b) -> if a = b then sum + (int a - int '0') else sum) 0
|> printfn "Solution to captcha: %d"

// Part II
input
|> Seq.indexed
|> Seq.fold
    (fun sum (i, c) ->
        let nextChar = input[(i + input.Length / 2) % input.Length]
        if nextChar = c then sum + (int c - int '0') else sum)
    0
|> printfn "Solution to new captcha: %d"
