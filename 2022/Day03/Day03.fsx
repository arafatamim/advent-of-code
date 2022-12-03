(*
 * Advent of Code 2022 Day 3: Rucksack Reorganization
 * https://adventofcode.com/2022/day/3
 *)

type Rucksack = string * string
type Item = char

let parseRucksack (str: string) =
    (str.Substring(0, str.Length / 2), str.Substring(str.Length / 2))

let commonItem (rucksack: Rucksack) =
    fst rucksack |> Seq.filter (fun x -> (snd rucksack).Contains(x)) |> Seq.head

let itemPriority (item: Item) =
    if item >= 'a' && item <= 'z' then
        (int item) - (int 'a') + 1
    else
        (int item) - (int 'A') + 27

let input = System.IO.File.ReadLines "input.txt"
let rucksacks = input |> Seq.map parseRucksack

// Part I
let sumOfPriorities = Seq.map commonItem >> Seq.sumBy itemPriority

printfn
    "The sum of the priorities of the common item types in all rucksack compartments are: %d"
    (rucksacks |> sumOfPriorities)

// Part II
let sumOfBadgePriorities =
    Seq.map (Set.ofSeq)
    >> Seq.chunkBySize 3
    >> Seq.map (Set.intersectMany >> Set.toArray >> Array.head)
    >> Seq.sumBy itemPriority

printfn
    "The sum of the priorities of the common item types that represents the badges of each Elf group is: %d"
    (input |> sumOfBadgePriorities)
