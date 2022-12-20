(*
 * Advent of Code Day 2: Bathroom Security
 * https://adventofcode.com/2016/day/2
 *)

let moveKey keypad (x, y) dir =
    let (i, j) =
        match dir with
        | 'U' -> x, max 0 (y - 1)
        | 'R' -> min (Array2D.length2 keypad - 1) (x + 1), y
        | 'D' -> x, min (Array2D.length2 keypad - 1) (y + 1)
        | 'L' -> max 0 (x - 1), y
        | _ -> x, y

    if keypad[j, i] = 'x' then x, y else i, j

let rec execInstructions (acc: string) (x, y) (keypad: char[,]) (instructions: string list) =
    match instructions with
    | [] -> acc
    | (head :: rest) ->
        let (i, j) = head |> Seq.fold (moveKey keypad) (x, y)
        execInstructions (acc + string keypad[j, i]) (i, j) keypad rest

let input = System.IO.File.ReadAllLines "input.txt" |> List.ofArray

// Part I
let keypad1 = array2D [ "123"; "456"; "789" ]
execInstructions "" (1, 1) keypad1 input |> printfn "Key code corresponding to first keypad: %s"

// Part II
let keypad2 = array2D [ "xx1xx"; "x234x"; "56789"; "xABCx"; "xxDxx" ]
execInstructions "" (0, 2) keypad2 input |> printfn "Key code corresponding to second keypad: %s"
