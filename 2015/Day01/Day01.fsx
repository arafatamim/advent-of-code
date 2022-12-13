(*
 * Advent of Code 2015 Day 1: Not Quite Lisp
 * https://adventofcode.com/2015/day/1
 *)

let input = (System.IO.File.ReadAllText "input.txt").Trim()

let finalFloor =
    Seq.fold (fun acc char -> if char = '(' then acc + 1 else acc - 1) 0

let basementCharacter =
    Seq.fold
        (fun (state, index) char ->
            let incr = index + 1 in

            (if state = -1 then (state, index)
             else if char = '(' then state + 1, incr
             else state - 1, incr))
        (0, 0)
    >> snd

printfn "Floor number that instructions point to: %d" (finalFloor input)

printfn "First character index that contains instruction to enter basement: %d" (basementCharacter input)
