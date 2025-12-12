open System

let initialPosition = 50
let dialRange = 100

let lines = System.IO.File.ReadLines "testinput.txt"

let inline (mod) x d =
    let r = x % d

    if r >= LanguagePrimitives.GenericZero then r
    elif d >= LanguagePrimitives.GenericZero then r + d
    else r - d

let dialPositions: seq<string> -> seq<int> =
    Seq.scan
        (fun position x ->
            let direction = x[0]
            let magnitude = x[1..] |> int

            match direction with
            | 'L' -> (position - magnitude) mod dialRange
            | 'R' -> (position + magnitude) mod dialRange
            | _ -> position)
        initialPosition

// part one

let part1 = lines |> dialPositions |> Seq.filter ((=) 0) |> Seq.length
printfn "Number of zeroes dial points to: %A" part1

// part two

let numZeroes: seq<string> -> int =
    // create flattened sequence of all individual rotations
    Seq.collect (fun line ->
        let direction = line[0]
        let magnitude = line[1..] |> int

        let step = if direction = 'L' then -1 else 1
        seq { for _ in 1..magnitude -> step })
    >> Seq.scan (+) initialPosition
    >> Seq.filter (fun pos -> pos mod dialRange = 0)
    >> Seq.length

let part2 = lines |> numZeroes

printfn "Number of times dial passes through zero: %A" numZeroes
