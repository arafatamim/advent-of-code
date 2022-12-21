(*
 * Advent of Code 2016 Day 4: Security Through Obscurity
 * https://adventofcode.com/2016/day/4
 *)

#r "nuget: FParsec"

open System
open FParsec

(*
module Option =
    let mapBoth ifSome ifNone opt =
        match opt with
        | Some x -> Some(ifSome x)
        | None -> Some ifNone
*)

let parseInput str =
    let parser =
        pipe3
            (manyChars (letter <|> pchar '-') |>> (fun x -> x.TrimEnd('-')))
            (pint32)
            (between (pchar '[') (pchar ']') (manyChars letter))
            (fun name sid checksum -> name, sid, checksum) in

    run parser str
    |> function
        | Success ((name, sid, checksum), _, _) -> (name, sid, checksum)
        | Failure _ -> failwith "unhandled"


let takeTopLetters count =
    // build a collection of letter frequencies
    Seq.countBy id
    // or Seq.fold (fun dict letter -> dict |> Map.change letter (Option.mapBoth ((+) 1) 1)) Map.empty
    // sort by most frequent letters
    >> Seq.sortBy (fun (k, v) -> -v, k)
    // take most common letters
    >> Seq.take count
    >> Seq.map fst
    >> Set.ofSeq


let areAllLettersInWord word =
    Set.intersect word >> Set.count >> (=) (Set.count word)


let isRoomReal name checksum =
    name
    |> Seq.filter ((<>) '-') // ignore dashes
    |> takeTopLetters 5
    |> areAllLettersInWord (Set.ofSeq checksum)


let totalSectionIdOfRealRooms =
    Seq.filter (fun (name, _, checksum) -> isRoomReal name checksum)
    >> Seq.sumBy (fun (_, sid, _) -> sid)


let decrypt count =
    Seq.map (fun c ->
        let startCode = int 'a'
        if Char.IsLetter c then
            let charCode = int c - startCode
            let newCharCode = (charCode + count) % 26
            char (newCharCode + startCode)
        else
            c)
    >> Seq.toArray
    >> String


let input = System.IO.File.ReadLines "input.txt" |> Seq.map parseInput

// Part I
input
|> totalSectionIdOfRealRooms
|> printfn "Sum of sector IDs of all real rooms: %d"

// Part II
input
|> Seq.map (fun (name, sid, _) -> decrypt sid name, sid)
|> Seq.tryFind (fun (decrypted, _) -> decrypted.Contains "northpole-object-storage")
|> function
    | Some (_, sid) -> printfn "Sector ID of the room where North Pole objects are stored: %d" sid
    | None -> printfn "North Pole object storage room not found"
