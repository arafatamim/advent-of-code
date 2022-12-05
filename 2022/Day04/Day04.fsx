let tupleApply f (a, b) = f a, f b

///"2-4,6-8" -> ("2-4", "6-8")
let parsePair (str: string) =
    match str.Split(",") with
    | [| x; y |] -> (x, y)
    | _ -> failwith "unhandled!"

///"2-4" -> Set [2; 3; 4]
let parseRange (str: string) =
    match str.Split("-") with
    | [| x; y |] -> Set [ int x .. int y ]
    | _ -> failwith "unhandled!"

let parseLine = parsePair >> tupleApply parseRange

let lengthBy fn = Seq.filter fn >> Seq.length

let input = System.IO.File.ReadLines "input.txt" |> Seq.map parseLine

// Part I
let isSubset (x, y) = Set.isSubset x y || Set.isSubset y x
printfn "Number of Elf assignment pairs that fully contain the other: %d" (input |> lengthBy isSubset)

// Part II
let isIntersect (x, y) = Set.intersect x y <> Set.empty
printfn "Number of Elf assignment pairs that overlap: %d" (input |> lengthBy isIntersect)
