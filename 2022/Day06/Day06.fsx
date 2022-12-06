(*
 * Advent of Code 2022 Day 6: Tuning Trouble
 * https://adventofcode.com/2022/day/6
 *)

let findMarkerIndex packetSize =
    List.windowed packetSize
    >> List.map Set.ofList
    >> List.findIndex (fun a -> Set.count a = packetSize)
    >> (+) packetSize

let input = System.IO.File.ReadAllText "input.txt" |> Seq.toList

// Part I
printfn "Position of first start-of-packet marker: %d" (input |> findMarkerIndex 4)

// Part II
printfn "Position of first start-of-message marker: %d" (input |> findMarkerIndex 14)
