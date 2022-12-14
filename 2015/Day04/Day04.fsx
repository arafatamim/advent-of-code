(*
 * Advent of Code 2015 Day 4: The Ideal Stocking Stuffer
 * https://adventofcode.com/2015/day/4
 *)

#r "nuget: MD5"

open System
open System.IO

let solve key zeroes =
    let prefix = String.init zeroes (fun _ -> "0")

    let rec bruteforce n =
        let s = key + string n
        let h = MD5Hash.Hash.Content(s) |> Seq.take zeroes |> Seq.Concat
        if h = prefix then n else bruteforce (n + 1)

    bruteforce 0

let input = "bgvyzdsv"

// Part I
printfn "MD5 hash starting with 5 zeroes: %d" <| solve "bgvyzdsv" 5

// Part II
printfn "MD5 hash starting with 6 zeroes: %d" <| solve "bgvyzdsv" 6
