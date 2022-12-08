(*
 * Advent of Code 2022 Day 7: No Space Left On Device
 * https://adventofcode.com/2022/day/7
 *)

open System.Collections.Generic

type Cd =
    | Up
    | Root
    | Named of string

type File = { Size: int; Name: string }

type Node =
    | File of File
    | Dir of string

type Command =
    | Cd of Cd
    | Ls of Node list

let parseCommand (line: string) =
    if not (line.StartsWith("$")) then
        None
    else
        match line.Split(" ") |> Array.tail with
        | [| "cd"; ".." |] -> Some(Cd Up)
        | [| "cd"; "/" |] -> Some(Cd Root)
        | [| "cd"; dir |] -> Some(Cd(Named dir))
        | [| "ls" |] -> None
        | _ -> None

let rec parseLsOutput =
    function
    | [] -> []
    | (line: string) :: lines ->
        match line.Split(" ") |> List.ofArray with
        | [ "dir"; name ] -> Dir name :: parseLsOutput lines
        | "$" :: _ -> []
        | [ size; name ] -> File { Size = int size; Name = name } :: parseLsOutput lines
        | _ -> []

let parseOperations lines =
    let rec parse (lines: string list) acc =
        match lines with
        | [] -> acc
        | line :: lines ->
            if line.StartsWith("$") then
                match line.Split(" ") |> Array.skip (1) with
                | [| "cd"; "/" |] -> parse lines (Cd Root :: acc)
                | [| "cd"; ".." |] -> parse lines (Cd Up :: acc)
                | [| "cd"; dir |] -> parse lines (Cd(Named dir) :: acc)
                | [| "ls" |] ->
                    let lsOutput =
                        (lines |> List.takeWhile (fun x -> not (x.StartsWith("$"))) |> parseLsOutput) in

                    parse lines (Ls lsOutput :: acc)
                | _ -> acc
            else
                parse lines acc

    parse lines [] |> Seq.rev

let buildTree ops =
    let state: Stack<string> = Stack()
    let mutable dirs: Map<string, File list> = Map.empty

    for op in ops do
        match op with
        | Cd (Named name) -> state.Push(name)
        | Cd Root -> state.Push("")
        | Cd Up -> state.Pop() |> ignore
        | Ls nodes ->
            for node in nodes do
                match node with
                | Dir name -> ()
                | File file ->
                    let key = state |> Seq.rev |> Seq.reduce (fun x y -> x + "/" + y)

                    if dirs.ContainsKey(key) then
                        dirs <- dirs.Change(key, Option.map (fun prev -> prev @ [ file ]))
                    else
                        dirs <- dirs.Add(key, [ file ])

    dirs

let getDirSizes tree =
    let mutable sizes = Map.empty<string, int>

    tree
    |> Map.iter (fun (dir: string) files ->
        let dirs = dir.Split("/") |> List.ofArray
        let size = files |> List.map (fun x -> x.Size) |> List.sum

        for i in 0 .. dirs.Length - 1 do
            let key = dirs[0..i] |> List.reduce (fun x y -> x + "/" + y)

            if sizes.ContainsKey(key) then
                sizes <- sizes.Change(key, Option.map (fun prev -> prev + size))
            else
                sizes <- sizes.Add(key, size))

    sizes

let input = System.IO.File.ReadAllLines "input.txt" |> List.ofArray

let tree = buildTree <| parseOperations input

let sizes = getDirSizes tree

// Part I
let dirSizeSum =
    (sizes
     |> Seq.map (fun size -> size.Value)
     |> Seq.filter (fun size -> size < 100_000)
     |> Seq.sum)

printfn "Sum of directory sizes less than 100000: %d" dirSizeSum

// Part II
let totalDiskSpace = 70_000_000
let spaceRequired = 30_000_000

let usedSpace = sizes |> Map.find ("")

let freeSpace = totalDiskSpace - usedSpace
let needToFree = spaceRequired - freeSpace

let dirToFree =
    (sizes
     |> Seq.map (fun size -> size.Value)
     |> Seq.filter (fun size -> size > needToFree)
     |> Seq.min)

printfn "Size of smallest directory that would free up enough space once deleted: %d" dirToFree
