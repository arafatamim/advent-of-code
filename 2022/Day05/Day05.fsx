(*
 * Advent of Code 2022 Day 5: Supply Stacks
 * https://adventofcode.com/2022/day/5
 *)

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Crate = int * char

type Instruction = { NumCrates: int; From: int; To: int }

type CraneModel =
    | CrateMover9000
    | CrateMover9001

module Cargo =
    type t = Stack<char> array

    let make size : t =
        Array.init size (fun _ -> Stack<char>())

    let push (index: int, value: char) (this: t) = this[ index - 1 ].Push(value)

    let move craneModel instruction (this: t) =
        match craneModel with
        | CrateMover9000 ->
            { 1 .. instruction.NumCrates }
            |> Seq.iter (fun _ -> this[ instruction.To - 1 ].Push(this[ instruction.From - 1 ].Pop()))
        | CrateMover9001 ->
            { 1 .. instruction.NumCrates }
            |> Seq.map (fun _ -> this[ instruction.From - 1 ].Pop())
            |> Seq.rev
            |> Seq.iter (fun x -> this[ instruction.To - 1 ].Push(x))

    let topCrates: t -> string =
        Array.map (fun x -> x.Peek()) >> Array.map string >> Array.reduce (+)

let splitInto lines =
    let index = List.findIndex (fun x -> x = "") lines
    let (crates, instructions) = lines |> List.splitAt index
    (crates |> List.rev |> List.tail, instructions |> List.filter (fun x -> x <> ""))

///"[Z] [M] [P]" -> [(1, 'Z'); (2, 'M'); (3, 'P')]
let parseCrateRow: string -> list<Crate> =
    Seq.chunkBySize 4
    // transform into a tuple of crate position & name
    >> Seq.mapi (fun i crate -> i + 1, crate[1])
    // filter empty crate row
    >> Seq.filter (fun (_, crate) -> crate <> ' ')
    >> List.ofSeq

///"move 1 from 2 to 1" -> (1, 2, 1)
let parseInstruction str =
    let regex = Regex(@"move (\d\d?) from (\d\d?) to (\d\d?)")
    let match' = regex.Match str

    { NumCrates = int match'.Groups[1].Value
      From = int match'.Groups[2].Value
      To = int match'.Groups[3].Value }

let maxHeight crates =
    crates |> List.maxBy List.length |> List.length

let buildCargo crates cargo =
    crates
    |> List.iter (fun x -> List.iter (fun crate -> cargo |> Cargo.push crate) x)

    cargo

let driveCrane model instructions cargo =
    instructions |> List.iter (fun instr -> Cargo.move model instr cargo)
    cargo

let buildAndArrangeCargo crates instructions craneModel =
    crates
    |> maxHeight
    |> Cargo.make
    |> (buildCargo crates >> driveCrane craneModel instructions)

let input = File.ReadAllLines "input.txt" |> List.ofArray

let crates, instructions =
    splitInto input
    |> fun (x, y) -> List.map parseCrateRow x, List.map parseInstruction y

let topCargo model =
    buildAndArrangeCargo crates instructions model |> Cargo.topCrates

// Part I
printfn "Crates that end up on top after rearranging with CrateMover9000: %s" (topCargo CrateMover9000)

// Part II
printfn "Crates that end up on top after rearranging with CrateMover9001: %s" (topCargo CrateMover9001)
