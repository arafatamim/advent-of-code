(*
 * Advent of Code Day 8: Matchsticks
 * https://adventofcode.com/2015/day/8
 *)

let rec decode =
    function
    | '\\' :: '\\' :: xs -> ('\\' :: decode xs)
    | '\\' :: '"' :: xs -> ('"' :: decode xs)
    | ('\\' :: 'x' :: x :: y :: xs) -> ('!' :: decode xs)
    | (x :: xs) -> (x :: decode xs)
    | [] -> []

let encode s =
    let rec f =
        function
        | '"' :: xs -> "\\\"" + f xs
        | '\\' :: xs -> "\\\\" + f xs
        | x :: xs -> x.ToString() + f xs
        | [] -> ""

    "\"" + f s + "\""

let strCodeLength = Seq.sumBy Seq.length

let input = System.IO.File.ReadLines "input.txt"

// Part I
input
|> Seq.sumBy (List.ofSeq >> decode >> Seq.length >> (-) 2 >> abs)
|> (-) (strCodeLength input)
|> printfn "Sum of differences between lengths of raw string literals & lengths of decoded strings: %d"

// Part II
input
|> Seq.sumBy (List.ofSeq >> encode >> Seq.length)
|> (-) (strCodeLength input)
|> abs
|> printfn "Sum of differences between lengths of encoded strings & lengths of raw string literals: %d"
