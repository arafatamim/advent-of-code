(*
 * Advent of Code 2015 Day 6: Probably a Fire Hazard
 * https://adventofcode.com/2015/day/6
 *)

type Grid = seq<seq<int>>

type Operation =
    | On
    | Off
    | Toggle

    static member FromStr str =
        match str with
        | "on" -> On
        | "off" -> Off
        | "toggle" -> Toggle
        | _ -> failwith "unhandled"

type Instruction =
    { From: int * int
      To: int * int
      Op: Operation }

let parseInstruction (str: string) =
    let split = str.Split(" ")

    let xy1 i =
        split.[i].Split(",")
        |> (function
        | [| x; y |] -> int x, int y
        | _ -> failwith "unhandled")

    let xy2 i =
        split.[i].Split(",")
        |> (function
        | [| x; y |] -> int x, int y
        | _ -> failwith "unhandled")

    if str.StartsWith("turn on") || str.StartsWith("turn off") then
        let op = Operation.FromStr split[1] in { From = xy1 2; To = xy2 4; Op = op }
    else
        { From = xy1 1
          To = xy2 3
          Op = Toggle }

let applyGrid
    translator
    ({ From = (x1, y1)
       To = (x2, y2)
       Op = op })
    =
    Seq.mapi (fun ix ->
        Seq.mapi (fun iy y ->
            if ix >= x1 && ix <= x2 && iy >= y1 && iy <= y2 then
                translator y op
            else
                y))

let flipLights =
    applyGrid (fun light ->
        function
        | On -> 1
        | Off -> 0
        | Toggle -> 1 - light)

let regulateLights =
    applyGrid (fun light ->
        function
        | On -> light + 1
        | Off -> max 0 (light - 1)
        | Toggle -> light + 2)

let countGlowingLights =
    Seq.fold (Seq.fold (fun j y -> if y = 1 then j + 1 else j)) 0

let countTotalBrightness = Seq.fold (Seq.fold (fun j y -> j + y)) 0

let applyMethod (fn: Instruction -> Grid -> Grid) grid instr = fn instr grid
let applyInstructions fn = Seq.fold <| applyMethod fn

let input = System.IO.File.ReadLines "input.txt" |> Seq.map parseInstruction

let initGrid size =
    Seq.init size (fun _ -> Seq.init size (fun _ -> 0))

let grid = initGrid 1000

// Part I
printfn
    "Number of lights lit per the lighting configuration: %d"
    (input |> applyInstructions flipLights grid |> countGlowingLights)

// Part II
printfn
    "Combined brightness of all lights per updated lighting configuration: %d"
    (input |> applyInstructions regulateLights grid |> countTotalBrightness)
