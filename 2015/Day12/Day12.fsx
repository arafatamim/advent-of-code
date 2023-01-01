(*
 * Advent of Code Day 12: JSAbacusFramework.io
 * https://adventofcode.com/2015/day/12
 *)

#r "nuget: FSharp.Data"

open System
open FSharp.Data

let input = System.IO.File.ReadAllText "input.txt" |> JsonValue.Parse

let rec sumNumbers cond =
    function
    | JsonValue.Array x -> x |> Array.sumBy (sumNumbers cond)
    | JsonValue.Record x when not (cond x) -> x |> Array.sumBy (fun (k, v) -> sumNumbers cond v)
    | JsonValue.Number x -> int x
    | _ -> 0


let recordContainsValue value =
    Array.exists (fun (k, v) ->
        match v with
        | JsonValue.String x when x = value -> true
        | _ -> false)


// Part I
input
|> sumNumbers (fun _ -> false)
|> printfn "Sum of all numbers in the JSON document: %d"

// Part II
input
|> sumNumbers (recordContainsValue "red")
|> printfn "Sum of all numbers without counting records with property with value \"red\": %d"
