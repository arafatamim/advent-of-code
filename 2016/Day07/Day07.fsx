(*
 * Advent of Code 2016 Day 7: Internet Protocol Version 7
 * https://adventofcode.com/2016/day/7
 *)

#r "nuget: FParsec"

open FParsec

module List =
    let sequence: list<'a * 'a> -> list<'a> * list<'a> =
        List.fold (fun (l1, l2) (x, y) -> (x :: l1), (y :: l2)) ([], [])


let tupleApply f (a, b) = f a, f b


let isAbbaSequence =
    function
    | [| a; b; c; d |] -> a = d && b = c && b <> a
    | _ -> false


let isAbaSequence =
    function
    | [| a; b; c |] -> a = c && b <> a && b <> c
    | _ -> false


///aba <=> bab; cnc <=> ncn
let matchingAbaBabSequences chars1 chars2 =
    match (chars1, chars2) with
    | ([| a; b; c |], [| x; y; z |]) -> a = c && a = y && b <> a && x = z && x = b && y <> x
    | _ -> false


let supportsTls =
    tupleApply (
        List.map (Seq.windowed 4 >> Seq.tryFind isAbbaSequence)
        >> List.choose id
        >> function
            | [] -> false
            | _ -> true
    )
    >> function
        | (true, false) -> true
        | _ -> false


let supportsSsl =
    tupleApply (List.map (Seq.windowed 3 >> Seq.filter isAbaSequence) >> Seq.concat)
    >> fun (hypernets, supernets) ->
        hypernets
        |> Seq.exists (fun hypSeq ->
            supernets
            |> Seq.exists (fun supSeq -> (hypSeq, supSeq) ||> matchingAbaBabSequences))


let parseLine str =
    let pSupernetSeq = many1Chars letter
    let pHypernetSeq = between (pchar '[') (pchar ']') (many1Chars letter)
    let p = (many (pSupernetSeq .>>. (pHypernetSeq <|> pstring "")))

    run p str
    |> function
        | Success (res, _, _) -> List.sequence res
        | Failure _ -> failwith "unhandled"


let input = System.IO.File.ReadAllLines "input.txt" |> Array.map parseLine

// Part I
input
|> Array.filter supportsTls
|> Array.length
|> printfn "Number of addresses that support TLS: %d"

// Part II
input
|> Array.filter supportsSsl
|> Array.length
|> printfn "Number of addresses that support SSL: %d"
