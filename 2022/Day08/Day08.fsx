(*
 * Advent of Code 2022 Day 8: Treetop Tree House
 * https://adventofcode.com/2022/day/8
 *)

let isCoordVisible grid (x, y) =
    let end' = (grid |> Array2D.length1) - 1

    if x = 0 || y = 0 || x = end' || y = end' then
        true
    else
        let h = grid.[x, *]
        let v = grid.[*, y]

        let height = grid.[x, y]

        let hl = h.[0 .. (max 0 (y - 1))] |> Array.max
        let hr = h.[y + 1 ..] |> Array.max
        let vl = v.[0 .. (max 0 (x - 1))] |> Array.max
        let vr = v.[x + 1 ..] |> Array.max

        height > hl || height > hr || height > vl || height > vr

let findVisible grid =
    let end' = (grid |> Array2D.length1) - 1

    [| 0..end' |]
    |> Array.map (fun x -> [| 0..end' |] |> Array.map (fun y -> (isCoordVisible grid (x, y), grid.[x, y])))
    |> array2D

let numVisible visible =
    visible |> Seq.cast<(bool * int)> |> Seq.filter fst |> Seq.length

let calcCoordScenicScore grid (x, y) =
    let end' = (grid |> Array2D.length1) - 1 // It's square, so length1 = length2

    if x = 0 || y = 0 || x = end' || y = end' then
        0
    else
        let h = grid.[x, *]
        let v = grid.[*, y]

        let height = grid.[x, y]

        let continue' = (fun h -> h < height)

        let scenicScore arr =
            let s = arr |> Array.takeWhile continue' |> Array.length
            if s = 0 then 1 else min (s + 1) arr.Length

        let hl = h.[0 .. (max 0 (y - 1))] |> Array.rev |> scenicScore
        let hr = h.[y + 1 ..] |> scenicScore
        let vl = v.[0 .. (max 0 (x - 1))] |> Array.rev |> scenicScore
        let vr = v.[x + 1 ..] |> scenicScore

        (hl * hr * vl * vr)

let findScenicScore grid =
    let end' = (grid |> Array2D.length1) - 1 // It's square, so length1 = length2

    [| 0..end' |]
    |> Array.map (fun x ->
        [| 0..end' |]
        |> Array.map (fun y -> (calcCoordScenicScore grid (x, y), grid.[x, y])))
    |> array2D

let maxScenicScore scores =
    scores |> Seq.cast<(int * int)> |> Seq.map fst |> Seq.max

let input =
    System.IO.File.ReadAllLines "input.txt"
    |> List.ofArray
    |> List.map (fun x -> x.ToCharArray() |> List.ofArray |> List.map (fun c -> int c - int '0'))
    |> array2D

// Part I
printfn "Number of trees visible from outside the grid: %d" (input |> findVisible |> numVisible)

// Part II
printfn "Highest scenic score possible for a tree: %d" (input |> findScenicScore |> maxScenicScore)
