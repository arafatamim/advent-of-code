(*
 * Advent of Code 2015 Day 2: I Was Told There Would Be No Math
 * https://adventofcode.com/2015/day/2
 *)

let cuboidArea (l, w, h) = 2 * (l * h + l * w + h * w)
let rectangleArea (l, w) = l * w

let calcPaperArea (l, w, h) =
    let smallestSide = [ l * w; w * h; h * l ] |> List.min
    cuboidArea (l, w, h) + smallestSide

let calcRibbonLength (l, w, h) =
    let perimeter = [ 2 * (l + w); 2 * (w + h); 2 * (l + h) ] |> List.min in perimeter + (l * w * h)

let input =
    System.IO.File.ReadAllLines "input.txt"
    |> List.ofArray
    |> List.map (
        (fun x -> x.Split("x"))
        >> (function
        | [| l; w; h |] -> (int l, int w, int h)
        | _ -> failwith "unhandled!")
    )

// Part I
printfn "Total square feet of wrapping paper required: %d" (input |> Seq.sumBy calcPaperArea)

// Part II
printfn "Total length of ribbon required (in feet): %A" (input |> List.sumBy calcRibbonLength)
