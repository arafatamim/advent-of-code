(*
 * Advent of Code 2016 Day 3: Squares With Three Sides
 * https://adventofcode.com/2016/day/3
 *)

let parseLine (line: string) =
    line.Split(" ")
    |> Array.filter (fun x -> x <> "")
    |> Array.map (int)
    |> function
        | [| a; b; c |] -> [| a; b; c |]
        | _ -> failwith "unhandled"

let isTriangle (a, b, c) = a + b > c && b + c > a && c + a > b

let input = System.IO.File.ReadAllLines "input.txt" |> Array.map parseLine

// Part I
input
|> Array.filter (function
    | [| a; b; c |] -> isTriangle (a, b, c)
    | _ -> false)
|> Array.length
|> printfn "Number of valid triangles: %d"

// Part II
input
|> Array.transpose
|> Array.concat
|> Array.chunkBySize 3
|> Array.filter (function
    | [| a; b; c |] -> isTriangle (a, b, c)
    | _ -> false)
|> Array.length
|> printfn "Number of valid triangles when sides are grouped by columns: %d"
