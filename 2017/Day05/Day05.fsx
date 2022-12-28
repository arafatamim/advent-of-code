(*
 * Advent of Code 2017 Day 5: A Maze of Twisty Trampolines, All Alike
 * https://adventofcode.com/2017/day/5
 *)

let input = System.IO.File.ReadAllLines "input.txt" |> Array.map int

let solve newRule maze =
    let maze = Array.copy maze

    let rec loop steps cursor =
        let currentInstr = maze[cursor]

        maze[cursor] <-
            if newRule && maze[cursor] >= 3 then
                maze[cursor] - 1
            else
                maze[cursor] + 1

        let newCursor = cursor + currentInstr
        let steps = steps + 1

        if newCursor >= 0 && newCursor < maze.Length then
            loop steps newCursor
        else
            steps

    loop 0 0


// Part I
solve false input |> printfn "Steps taken to reach exit: %d"

// Part II
solve true input |> printfn "Steps taken to reach exit under new rule: %d"
