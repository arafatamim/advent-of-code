(*
 * Advent of Code 2017 Day 6: Memory Reallocation
 * https://adventofcode.com/2017/day/6
 *)


let input =
    System.IO.File.ReadAllText "input.txt"
    |> (fun x -> x.Trim().Split("\t"))
    |> Array.map int


let solve banks =
    let rec distribute cycles history =
        let largestIndex, largestMemory = banks |> Array.indexed |> Array.maxBy snd
        banks[largestIndex] <- 0

        { 1..largestMemory }
        |> Seq.iter (fun cursor ->
            let nextCursor = (cursor + largestIndex) % banks.Length
            banks[nextCursor] <- banks[nextCursor] + 1)

        if history |> List.contains banks then
            cycles + 1
        else
            distribute (cycles + 1) (Array.copy banks :: history)

    distribute 0 []


// Part I
solve input |> printfn "Number of redistribution cycles without repetition: %d"
