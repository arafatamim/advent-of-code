(*
 * Advent of Code 2015 Day 10: Elves Look, Elves Say
 * https://adventofcode.com/2015/day/10
 *)


module Seq =
    let groupByValue (values: seq<'T>) : list<list<'T>> =
        let values = List.ofSeq values

        let rec impl acc currentGroup values =
            match values with
            | [] -> if currentGroup = [] then acc else currentGroup :: acc
            | x :: xs ->
                if List.isEmpty currentGroup then
                    impl acc [ x ] xs
                else if List.head currentGroup = x then
                    impl acc (x :: currentGroup) xs
                else
                    impl (currentGroup :: acc) [ x ] xs

        impl [] [] values |> List.rev


let lookAndSay =
    Seq.groupByValue
    >> List.fold
        (fun acc cur ->
            let item = List.head cur in
            let length = List.length cur in
            acc + string length + string item)
        ""


let repeat count fn input =
    { 1..count } |> Seq.fold (fun state _ -> fn state) input


let input = "3113322113"


// Part I
input
|> repeat 40 lookAndSay
|> String.length
|> printfn "Length of puzzle result after 40 iterations: %d"


// Part II
input
|> repeat 50 lookAndSay
|> String.length
|> printfn "Length of puzzle result after 50 iterations: %d"
