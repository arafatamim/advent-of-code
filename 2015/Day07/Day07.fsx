(*
 * Advent of Code 2015 Day 7: Some Assembly Required
 * https://adventofcode.com/2015/day/7
 *)

open System

let mutable input =
    System.IO.File.ReadLines "input.txt"
    |> Seq.fold
        (fun calc command ->
            match command.Split(" -> ") with
            | [| ops; res |] -> calc |> Map.add (res.Trim()) (ops.Split(" "))
            | _ -> failwith "unhandled")
        (Map<string, string array> [])

let mutable cache = Map.empty<string, uint16>

let rec getValue (input': string) =
    match UInt16.TryParse input' with
    | (true, i) -> i
    | (false, _) ->
        match Map.tryFind input' cache with
        | Some v -> v
        | None ->
            match input |> Map.tryFind input' with
            | Some op ->
                let value =
                    match op with
                    | [| "NOT"; r |] -> ~~~(getValue r)
                    | [| l; "AND"; r |] -> getValue l &&& getValue r
                    | [| l; "OR"; r |] -> getValue l ||| getValue r
                    | [| l; "LSHIFT"; r |] -> getValue l <<< Int32.Parse r
                    | [| l; "RSHIFT"; r |] -> getValue l >>> Int32.Parse r
                    | [| x |] -> getValue x
                    | _ -> failwith "unhandled"

                cache <- cache |> Map.add input' value
                value
            | None ->
                eprintfn "Wire labeled '%s' not present in circuit!" input'
                exit 1

// Part I
printfn "Signal of wire 'a': %d" <| getValue "a"

input <- input |> Map.add "b" [| (getValue "a").ToString() |]
cache <- Map.empty

// Part II
printfn "New signal of wire 'a' after overriding to 'b' & resetting the circuit: %d" (getValue "a")
