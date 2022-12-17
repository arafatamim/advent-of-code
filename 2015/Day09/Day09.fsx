(*
 * Advent of Code 2015 Day 9: All in a Single Night
 * https://adventofcode.com/2015/day/9
 *)

#r "nuget: FParsec"

open System
open FParsec

let parseRoute inp =
    let pWord = many1CharsTill (satisfy (fun _ -> true)) spaces1

    match
        run
            (pWord .>> pstring "to" .>> skipAnyChar .>>. pWord .>> skipString "= "
             .>>. pint32)
            inp
    with
    | Success (res, _, _) -> res
    | Failure _ -> failwith "invalid input"


let collectCities =
    Map.fold (fun acc (from, to') _ -> from :: to' :: acc) []
    >> List.ofSeq
    >> List.distinct


let rec calcDistance pick (routes: Map<string * string, int>) =
    function
    | []
    | [ _ ] -> 0
    | [ from; to' ] -> routes[from, to']
    | from :: cities ->
        cities
        |> List.map (fun city ->
            routes[from, city]
            + calcDistance pick routes (city :: (List.except [ city ] cities)))
        |> pick


let findRoutes order (routes: Map<string * string, int>) (cities: string list) =
    cities
    |> List.map (fun city -> order routes (city :: (List.except [ city ] cities)))


let input =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map parseRoute
    |> Seq.fold
        (fun acc ((from, to'), distance) -> acc |> Map.add (from, to') distance |> Map.add (to', from) distance)
        Map.empty

let findDistance order routes = findRoutes (calcDistance order) routes (collectCities routes) |> order

// Part I
input |> findDistance List.min |> printfn "Distance of shortest route: %d"

// Part II
input |> findDistance List.max |> printfn "Distance of longest route: %d"
