(*
 * Advent of Code 2018 Day 1: Chronal Calibration
 * https://adventofcode.com/2018/day/1
 *)

let input = System.IO.File.ReadLines "input.txt"

let finalFreq = Seq.fold (fun acc x -> int acc + int x) 0

let findRepeatFreq changes =
    let rec add sum history =
        function
        | [] -> add sum history changes // restart loop with original frequencies
        | x :: xs ->
            let sum = x + sum in

            if history |> Set.contains sum then
                sum
            else
                add sum (history |> Set.add sum) xs

    add 0 (Set.ofList [0]) changes

// Part I
finalFreq input |> printfn "Resulting frequency: %d"

// Part II
findRepeatFreq (input |> Seq.map System.Int32.Parse |> List.ofSeq)
|> printfn "First frequency encountered twice: %d"
