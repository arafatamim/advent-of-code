(*
 * Advent of Code Day 2: Corruption Checksum
 * https://adventofcode.com/2017/day/2
 *)

#r "nuget: FParsec"

open FParsec


let parseLine str =
    run (many (pint32 .>> spaces)) str
    |> function
        | Success (nums, _, _) -> nums
        | _ -> failwith "unhandled"


let findDifferenceBetweenMaxAndMinPair arr = List.max arr - List.min arr


let findDivisiblePair row =
    row
    |> List.collect (fun x -> row |> List.map (fun i -> (i, x)))
    |> List.filter (fun (a, b) -> a <> b)
    |> List.tryPick (fun (a, b) -> if a % b = 0 then Some(a / b) else None)
    |> function
        | Some i -> i
        | _ -> 0


let input = System.IO.File.ReadAllLines "input.txt" |> Array.map parseLine


// Part I
input
|> Array.sumBy findDifferenceBetweenMaxAndMinPair
|> printfn "Sum of each row's result according the difference between their largest and smallest values: %d"


// Part II
input
|> Array.sumBy findDivisiblePair
|> printfn "Sum of each row's result according to their evenly divisible values: %d"
